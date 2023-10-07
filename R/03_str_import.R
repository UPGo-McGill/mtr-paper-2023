#### IMPORT STR DATA ###########################################################

source("R/01_startup.R")
qload("output/data/geometry.qsm")

# Add property file -------------------------------------------------------

property <- 
  read_csv(c("data/property_MTL.csv", "data/property_TO.csv", 
             "data/property_VAN.csv"), show_col_types = FALSE) |> 
  select(property_ID = `Property ID`, listing_title = `Listing Title`, 
         property_type = `Property Type`, listing_type = `Listing Type`,
         created = `Created Date`, scraped = `Last Scraped Date`, 
         latitude = `Latitude`, longitude = `Longitude`, bedrooms = `Bedrooms`,
         city = `City`, minimum_stay = "Minimum Stay", 
         ab_property = `Airbnb Property ID`, ab_host = `Airbnb Host ID`, 
         ha_property = `HomeAway Property ID`, 
         ha_host = `HomeAway Property Manager ID`,
         img_url = `Listing Main Image URL`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-")) |>
  mutate(host_ID = coalesce(as.character(ab_host), ha_host),
         .after = property_ID) |> 
  strr_housing() |> 
  mutate(housing = if_else(property_type %in% c(
    "Home", "condo", "apartment", "house", "townhome", "cottage", "bungalow",
    "studio"), TRUE, housing))

property <- 
  property |> 
  filter(city %in% c("Toronto", "Montreal", "Vancouver"))


# Do raffle and add wards -------------------------------------------------

property <-
  property |> 
  strr_as_sf(3347) |> 
  # Remove extremely distant outliers
  st_filter(st_buffer(city, 1000))

property_1 <- 
  property |> 
  slice(1:70000) |> 
  strr_raffle(CT, GeoUID, dwellings)

property_2 <- 
  property |> 
  slice(70001:140000) |> 
  strr_raffle(CT, GeoUID, dwellings)

property_3 <- 
  property |> 
  slice(140001:210000) |> 
  strr_raffle(CT, GeoUID, dwellings)

property_4 <- 
  property |> 
  slice(210001:269374) |> 
  strr_raffle(CT, GeoUID, dwellings)

property <- bind_rows(property_1, property_2, property_3, property_4)

# Save progress
qsave(property, file = "output/data/property.qs", nthreads = availableCores())

property <-
  property |> 
  select(-.grid_ID) |> 
  rename(CT = GeoUID)

rm(property_1, property_2, property_3, property_4)


# Add monthly file --------------------------------------------------------

monthly_all <- 
  read_csv(c("data/monthly_MTL.csv", "data/monthly_TO.csv", 
             "data/monthly_VAN.csv"), show_col_types = FALSE) |> 
  select(property_ID = `Property ID`, month = `Reporting Month`,
         rev = `Revenue (Native)`, r = `Reservation Days`,
         a = `Available Days`, b = `Blocked Days`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-"),
         month = yearmonth(month)) |> 
  filter(month <= yearmonth("2023-05"))


# Process monthly ---------------------------------------------------------

monthly <- 
  monthly_all |> 
  filter(property_ID %in% property$property_ID[property$housing]) |> 
  # Hold on to old reservations for later adjusting revenue
  mutate(r_old = r)

# Start by removing all months that are < created, > scraped, or 0 days
monthly <- 
  monthly |> 
  left_join(select(st_drop_geometry(property), property_ID, created, scraped), 
            by = "property_ID") |> 
  filter(month >= yearmonth(created), month <= yearmonth(scraped),
         r + a + b > 0)

# Proportionally remove entries so r + a + b == scraped_days
scraped_change <- 
  monthly |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(scraped)) |> 
  mutate(scraped_days = day(scraped)) |> 
  mutate(across(c(r:b), \(x) x * scraped_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > scraped_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < scraped_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:scraped_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > scraped_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > scraped_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < scraped_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < scraped_days, a + 1, a)) |> 
  select(-scraped_days)

monthly <- 
  monthly |> 
  anti_join(scraped_change, by = c("property_ID", "month")) |> 
  bind_rows(scraped_change) |> 
  arrange(property_ID, month)

# Repeat for created
created_change <-
  monthly |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(created)) |> 
  mutate(created_days = days_in_month(
    month(as.Date(month))) - day(created) + 1) |> 
  mutate(across(c(r:b), \(x) x * created_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > created_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < created_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:created_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > created_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > created_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < created_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < created_days, a + 1, a)) |> 
  select(-created_days)

monthly <- 
  monthly |> 
  anti_join(created_change, by = c("property_ID", "month")) |> 
  bind_rows(created_change) |> 
  arrange(property_ID, month)

# Adjust revenue with r / r_old
monthly <- 
  monthly |> 
  mutate(rev = coalesce(rev * r / r_old, 0)) |> 
  select(-r_old, -created, -scraped)

monthly <- 
  monthly |> 
  inner_join(select(st_drop_geometry(property), property_ID, host_ID, 
                    listing_type, housing, CT), by = "property_ID") |> 
  transmute(property_ID, month, R = r, A = a, B = b, revenue = rev, host_ID, 
            listing_type, housing, CT)

rm(created_change, scraped_change)


# Calculate multilistings -------------------------------------------------

monthly_host <- monthly
data.table::setDT(monthly_host)
monthly_host <- monthly_host[!is.na(host_ID), .(host_ID, month, listing_type)]
host <- monthly_host[, .(count = .N), by = .(host_ID, month, listing_type)]
host <- dplyr::as_tibble(host)
thresholds <- c(EH = 2L, PR = 3L, SR = NA, HR = NA)
thresholds_names <- names(thresholds)
thresholds <- if_else(thresholds == 0, NA_integer_, as.integer(thresholds))
col_names <- names(monthly)
data.table::setDT(monthly)
data.table::setDT(host)
EH <- thresholds[thresholds_names %in% c("EH", "Entire home/apt")]
PR <- thresholds[thresholds_names %in% c("PR", "Private room")]
SR <- thresholds[thresholds_names %in% c("SR", "Shared room")]
HR <- thresholds[thresholds_names %in% c("HR", "Hotel room")]
multi <- host[listing_type == "Entire home/apt" & count >= 
                EH][, `:=`(c(".ML", "count"), list(TRUE, NULL))]

multi <- data.table::rbindlist(list(multi, host[
  listing_type == "Private room" & count >= PR][
    , `:=`(c(".ML", "count"), list(TRUE, NULL))]))

join_cols <- setdiff(names(multi), ".ML")
multi <- multi[, .(.ML = sum(.ML)), by = c("host_ID", "month")][
  , `:=`(.ML, as.logical(.ML))]
join_cols <- setdiff(names(multi), c(".ML", "listing_type"))

monthly <- multi[monthly, on = join_cols][
  , `:=`(.ML, if_else(is.na(.ML), FALSE, .ML))]
monthly <- dplyr::as_tibble(monthly)

monthly <- 
  monthly |> 
  rename(multi = .ML) |> 
  relocate(multi, .after = listing_type) |> 
  relocate(property_ID, month) |> 
  relocate(host_ID, .after = revenue)

rm(host, monthly_host, multi, col_names, EH, HR, join_cols, PR, SR, thresholds, 
   thresholds_names)


# Minimum stay ------------------------------------------------------------

min_2017 <- 
  read_csv(c("data/prop_2017_MTL.csv", "data/prop_2017_TO.csv", 
             "data/prop_2017_VAN.csv")) |> 
  select(property_ID = `Property ID`, scraped = `Last Scraped Date`, 
         minimum_stay = "Minimum Stay") |> 
  mutate(property_ID = paste0("ab-", property_ID),
         max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2018 <- 
  read_csv("data/prop_2018.csv") |> 
  select(property_ID = `Property ID`, scraped = `Last Scraped Date`, 
         minimum_stay = "Minimum Stay") |> 
  mutate(property_ID = paste0("ab-", property_ID),
         max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019 <- 
  read_csv("data/prop_2019.csv") |> 
  select(property_ID = `Property ID`, scraped = `Last Scraped Date`, 
         minimum_stay = "Minimum Stay") |> 
  mutate(property_ID = paste0("ab-", property_ID),
         max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_04 <- 
  qread("data/property_2019_04.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_05 <- 
  qread("data/property_2019_05.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_06 <- 
  qread("data/property_2019_06.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_07 <- 
  qread("data/property_2019_07.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_08 <- 
  qread("data/property_2019_08.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_09 <- 
  qread("data/property_2019_09.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_10 <- 
  qread("data/property_2019_10.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_11 <- 
  qread("data/property_2019_11.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2019_12 <- 
  qread("data/property_2019_12.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_01 <- 
  qread("data/property_2020_01.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_02 <- 
  qread("data/property_2020_02.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_03 <- 
  qread("data/property_2020_03.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_04 <- 
  qread("data/property_2020_04.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_05 <- 
  qread("data/property_2020_05.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_06 <- 
  qread("data/property_2020_06.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_07 <- 
  qread("data/property_2020_07.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_08 <- 
  qread("data/property_2020_08.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_09 <- 
  qread("data/property_2020_09.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_10 <- 
  qread("data/property_2020_10.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_11 <- 
  qread("data/property_2020_11.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2020_12 <- 
  qread("data/property_2020_12.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_01 <- 
  qread("data/property_2021_01.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_02 <- 
  qread("data/property_2021_02.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_03 <- 
  qread("data/property_2021_03.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_04 <- 
  qread("data/property_2021_04.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_05 <- 
  qread("data/property_2021_05.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_06 <- 
  qread("data/property_2021_06.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_07 <- 
  qread("data/property_2021_07.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_08 <- 
  qread("data/property_2021_08.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_09 <- 
  qread("data/property_2021_09.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_11 <- 
  qread("data/property_2021_11.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2021_12 <- 
  qread("data/property_2021_12.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2022_01 <- 
  qread("data/property_2022_01.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2022_02 <- 
  qread("data/property_2022_02.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2022_03 <- 
  qread("data/property_2022_03.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2022_04 <- 
  qread("data/property_2022_04.qs", nthreads = 10) |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  filter(property_ID %in% property$property_ID) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_2023_05 <- 
  property |> 
  st_drop_geometry() |> 
  mutate(max_date = max(scraped, na.rm = TRUE)) |> 
  select(property_ID, start_date = scraped, max_date, minimum_stay)

min_stay <- 
  bind_rows(min_2017, min_2018, min_2019, min_2019_04, min_2019_05, min_2019_06, 
            min_2019_07, min_2019_08, min_2019_09, min_2019_10, min_2019_11,
            min_2019_12, min_2020_01, min_2020_02, min_2020_03, min_2020_04, 
            min_2020_05, min_2020_06, min_2020_07, min_2020_08, min_2020_09, 
            min_2020_10, min_2020_11, min_2020_12, min_2021_01, min_2021_02, 
            min_2021_03, min_2021_04, min_2021_05, min_2021_06, min_2021_07,
            min_2021_08, min_2021_09, min_2021_11, min_2021_12, min_2022_01, 
            min_2022_02, min_2022_03, min_2022_04, min_2023_05)

min_stay <-
  min_stay |> 
  summarize(max_date = max(max_date), 
            .by = c(property_ID, start_date, minimum_stay)) |> 
  group_by(property_ID) |> 
  reframe(start_date = c(start_date, max(max_date)),
          minimum_stay = c(minimum_stay, 
                           minimum_stay[max_date == max(max_date)])) |> 
  distinct() |> 
  filter(!is.na(start_date)) |> 
  mutate(month = yearmonth(start_date)) |> 
  arrange(property_ID, month) |> 
  select(property_ID, month, minimum_stay, start_date) |> 
  summarize(minimum_stay = first(minimum_stay[start_date == max(start_date)]),
            .by = c(property_ID, month)) |> 
  arrange(property_ID, month)

min_stay_last <- 
  min_stay |> 
  filter(month == max(month), .by = property_ID) |> 
  select(property_ID, min_2 = minimum_stay)

min_stay <- 
  min_stay |> 
  filter(month <= yearmonth("2023-05"))

qsavem(min_stay, min_stay_last, file = "output/data/min_stay.qsm", 
       nthreads = availableCores())
qload("output/data/min_stay.qsm")

monthly <- 
  monthly |> 
  select(-any_of(c("minimum_stay"))) |> 
  left_join(min_stay, by = c("property_ID", "month")) |> 
  relocate(minimum_stay, .after = revenue) |> 
  group_by(property_ID) |> 
  fill(minimum_stay, .direction = "downup") |> 
  ungroup() |> 
  left_join(min_stay_last, by = "property_ID") |> 
  mutate(minimum_stay = coalesce(minimum_stay, min_2, 1)) |> 
  select(-min_2)


# Clean up property fields ------------------------------------------------

property <- 
  property |> 
  select(-any_of(c("dwellings", "min_check")))


# Save output -------------------------------------------------------------

qsave(property, file = "output/data/property.qs", nthreads = availableCores())
qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())
qsave(monthly_all, file = "output/data/monthly_all.qs", 
      nthreads = availableCores())

rm(min_2017, min_2018, min_2019, min_2019_04, min_2019_05, min_2019_06, 
   min_2019_07, min_2019_08, min_2019_09, min_2019_10, min_2019_11, min_2019_12, 
   min_2020_01, min_2020_02, min_2020_03, min_2020_04, min_2020_05, min_2020_06, 
   min_2020_07, min_2020_08, min_2020_09, min_2020_10, min_2020_11, min_2020_12, 
   min_2021_01, min_2021_02, min_2021_03, min_2021_04, min_2021_05, min_2021_06, 
   min_2021_07, min_2021_08, min_2021_09, min_2021_11, min_2021_12, min_2022_01, 
   min_2022_02, min_2022_03, min_2022_04, min_2023_05, min_stay, min_stay_last, 
   monthly_all)
