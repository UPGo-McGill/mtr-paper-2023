#### FIGURES AND ANALYSIS ######################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
GH <- qread("output/data/GH.qs", nthreads = availableCores())
ltr <- qread("output/data/ltr.qs", nthreads = availableCores())
qload("output/data/geometry.qsm", nthreads = availableCores())
water <- qread("output/data/water.qs", nthreads = availableCores())


# Table 2 -----------------------------------------------------------------

# Unique properties
property |> 
  filter(scraped >= "2015-01-01") |> 
  nrow()
length(unique(ltr$id))

# Time period
c(min(monthly$month), max(monthly$month))
c(min(ltr$scraped), max(ltr$scraped))

# Total data points
monthly |> 
  filter(year(month) >= 2015) |> 
  summarize(tot = sum(R + A + B)) |> 
  pull()
nrow(ltr)


# Figure 1 ----------------------------------------------------------------

monthly_activity <- 
  monthly |> 
  filter(month >= yearmonth("2017-06-01")) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(
    displayed_pct = sum(A[min28] + R[min28] + B[min28]) / sum(A + R + B),
    active_pct = sum(A[min28] + R[min28]) / sum(A + R),
    res_pct = sum(R[min28]) / sum(R),
    rev_pct = sum(revenue[min28] / sum(revenue)),
    .by = c(month, city)) |> 
  pivot_longer(-c(month, city))

# 2023 max/min active
monthly_activity |> 
  filter(year(month) == 2023, name == "active_pct") |> 
  filter(value %in% c(min(value), max(value)), .by = city)

# 2017 max/min active
monthly_activity |> 
  filter(year(month) == 2017, name == "active_pct") |> 
  filter(value %in% c(min(value), max(value)), .by = city)

# Covid
monthly_activity |> 
  filter(month(month) == 12, year(month) %in% 2019:2020, 
         name == "active_pct") |> 
  arrange(city, month)

# Covid overall change
monthly_activity |> 
  filter(month(month) == 12, year(month) %in% 2019:2020, 
         name == "active_pct") |> 
  summarize(y19 = mean(value[year(month) == 2019]),
            y20 = mean(value[year(month) == 2020]),
            change = (y20 - y19) / y19)

# Figure 1
figure_1 <-
  monthly_activity |> 
  mutate(label = if_else(month == yearmonth("2022-04-01") & city == "Toronto", 
                         case_when(
                           name == "displayed_pct" ~ "Displayed listings",
                           name == "active_pct" ~ "Active listings",
                           name == "res_pct" ~ "Reserved nights",
                           name == "rev_pct" ~ "Host revenue"), 
                         NA_character_)) |>
  ggplot(aes(month, value, colour = name)) +
  geom_line(lwd = 1.5) +
  ggrepel::geom_label_repel(aes(label = label), alpha = 0.9, family = "Futura", 
                            size = 3, force_pull = 0.5) +
  facet_wrap(~city, ncol = 3) +
  scale_y_continuous(name = "MTR share of Airbnb/Vrbo listings", 
                     labels = scales::percent) +
  scale_x_yearmonth(name = NULL) +
  scale_linetype_discrete(name = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1, 2, 4)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"), 
        strip.text = element_text(face = "bold"))

ggsave("output/figure_1.png", plot = figure_1, width = 8, height = 5, 
       units = "in")


# Figure 2 ----------------------------------------------------------------

mtr_for_map <-
  monthly |> 
  filter(month == yearmonth("2023-05"), A + R > 0) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(active = sum(A[min28] + R[min28]) / days_in_month(5),
            active_pct = active / (sum(A + R) / days_in_month(5)),
            .by = c(city, CT)) |> 
  filter(active >= 1) |> 
  inner_join(CT, by = c("CT" = "GeoUID")) |> 
  st_as_sf()

# Correlations
mtr_for_map |> 
  st_drop_geometry() |> 
  transmute(dwell_pct = log(active / dwellings + 0.0000001), 
            active_pct = log(active_pct + 0.0000001),
            city) |> 
  group_by(city) |> 
  dplyr::group_split() |> 
  map(select, -city) |> 
  map(cor)

# Correlations for only EH or for excluding condos
monthly |> 
  inner_join(select(property, property_ID, property_type), 
             by = "property_ID") |> 
  filter(listing_type == "Entire home/apt") |> 
  # filter(!str_detect(property_type, "ondo")) |> 
  filter(month == yearmonth("2023-05"), A + R > 0) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(active = sum(A[min28] + R[min28]) / days_in_month(5),
            active_pct = active / (sum(A + R) / days_in_month(5)),
            .by = c(city, CT)) |> 
  filter(active >= 1) |> 
  inner_join(CT, by = c("CT" = "GeoUID")) |> 
  transmute(dwell_pct = log(active / dwellings + 0.0000001), 
            active_pct = log(active_pct + 0.0000001),
            city) |> 
  group_by(city) |> 
  dplyr::group_split() |> 
  map(select, -city) |> 
  map(cor)

fig_2_1_MTL <-
  mtr_for_map |> 
  filter(name == "Montréal") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active / dwellings), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 6)], na.value = "grey80",
                    limits = c(0, 0.005), oob = scales::squish, 
                    breaks = c(0, 0.001, 0.002, 0.003, 0.004), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Montréal")) +
  ggtitle("Montreal") +
  guides(fill = guide_coloursteps(title = "MTRs/dwelling", title.vjust = 0.8,
                                  nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", plot.title = element_text(face = "bold"))

fig_2_1_TO <-
  mtr_for_map |> 
  filter(name == "Toronto") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active / dwellings), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 6)], na.value = "grey80",
                    limits = c(0, 0.005), oob = scales::squish, 
                    breaks = c(0, 0.001, 0.002, 0.003, 0.004), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Toronto")) +
  ggtitle("Toronto") +
  guides(fill = guide_coloursteps(title = "MTRs/dwelling", title.vjust = 0.8,
                                  nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", plot.title = element_text(face = "bold"))

fig_2_1_VAN <-
  mtr_for_map |> 
  filter(name == "Vancouver") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active / dwellings), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 6)], na.value = "grey80",
                    limits = c(0, 0.005), oob = scales::squish, 
                    breaks = c(0, 0.001, 0.002, 0.003, 0.004), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Vancouver")) +
  ggtitle("Vancouver") +
  guides(fill = guide_coloursteps(title = "MTRs/dwelling", title.vjust = 0.8,
                                  nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", plot.title = element_text(face = "bold"))

fig_2_2_MTL <-
  mtr_for_map |> 
  filter(name == "Montréal") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active_pct), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 5)], na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    breaks = c(0, 0.15, 0.3, 0.45, 0.6), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Montréal")) +
  guides(fill = guide_coloursteps(title = "MTR share of Airbnb/Vrbo listings", 
                                  title.vjust = 0.8, nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")

fig_2_2_TO <-
  mtr_for_map |> 
  filter(name == "Toronto") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active_pct), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 5)], na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    breaks = c(0, 0.15, 0.3, 0.45, 0.6), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Toronto")) +
  guides(fill = guide_coloursteps(title = "MTR share of Airbnb/Vrbo listings", 
                                  title.vjust = 0.8, nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")

fig_2_2_VAN <-
  mtr_for_map |> 
  filter(name == "Vancouver") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active_pct), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 5)], na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    breaks = c(0, 0.15, 0.3, 0.45, 0.6), 
                    labels = scales::percent)  +
  gg_bbox(filter(city, name == "Vancouver")) +
  guides(fill = guide_coloursteps(title = "MTR share of Airbnb/Vrbo listings", 
                                  title.vjust = 0.8, nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")

fig_2_3_MTL <-
  mtr_for_map |> 
  filter(city == "Montreal") |> 
  ggplot(aes(active / dwellings, active_pct)) +
  geom_point(size = 0.5, colour = col_palette[1]) +
  geom_smooth(method = "lm", se = FALSE, colour = col_palette[5]) +
  scale_y_log10(name = "MTR share of Airbnb/Vrbo listings",
                labels = scales::percent) +
  scale_x_log10(name = "MTRs/dwelling", labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"))
    
fig_2_3_TO <-
  mtr_for_map |> 
  st_drop_geometry() |> 
  filter(city == "Toronto") |> 
  ggplot(aes(active / dwellings, active_pct)) +
  geom_point(size = 0.5, colour = col_palette[1]) +
  geom_smooth(method = "lm", se = FALSE, colour = col_palette[5]) +
  scale_y_log10(name = "MTR share of Airbnb/Vrbo listings",
                labels = scales::percent, limits = c(0.1, 1)) +
  scale_x_log10(name = "MTRs/dwelling", labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"))

fig_2_3_VAN <-
  mtr_for_map |> 
  filter(city == "Vancouver") |> 
  ggplot(aes(active / dwellings, active_pct)) +
  geom_point(size = 0.5, colour = col_palette[1]) +
  geom_smooth(method = "lm", se = FALSE, colour = col_palette[5]) +
  scale_y_log10(name = "MTR share of Airbnb/Vrbo listings",
                labels = scales::percent) +
  scale_x_log10(name = "MTRs/dwelling", labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(family = "Futura", size = 7),
        plot.background = element_rect(fill = "white", colour = "transparent"))

figure_2 <- fig_2_1_MTL + fig_2_2_MTL + fig_2_3_MTL + fig_2_1_TO + fig_2_2_TO + 
  fig_2_3_TO + fig_2_1_VAN + fig_2_2_VAN + fig_2_3_VAN + 
  plot_layout(guides = "collect", nrow = 3) & 
  theme(legend.position = "bottom", plot.margin = unit(c(3, 3, 3, 3), "points"))

ggsave("output/figure_2.png", plot = figure_2, width = 7, height = 9, 
       units = "in")


# Figure 3 ----------------------------------------------------------------

# FREH
FREH_total <- 
  monthly |> 
  filter(month >= yearmonth("2016-01-01")) |> 
  mutate(min_28 = minimum_stay >= 28) |> 
  summarize(FREH = sum(FREH_2022), .by = c(min_28, month, city))

# Re-process GH
GH <- 
  GH |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  inner_join(monthly, by = c("month", "host_ID", "property_ID")) |> 
  summarize(
    property_IDs = list(property_ID),
    min_28 = mean(minimum_stay >= 28, na.rm = TRUE) >= 0.5,
    .by = c(ghost_ID, month, city, n, host_ID, listing_count, housing_units, 
            active_pct))

# GH
GH_total <-
  GH |> 
  st_drop_geometry() |> 
  mutate(days = days_in_month(month)) |> 
  summarize(GH = sum(housing_units * n / days), .by = c(min_28, month, city)) |> 
  arrange(month, min_28, city)

# Total housing loss
housing_loss <-
  FREH_total |> 
  mutate(listing = if_else(min_28, "Entire home MTR", "Entire home STR")) |> 
  rename(value = FREH) |> 
  select(-min_28) |> 
  bind_rows(
    GH_total |> 
      mutate(listing = if_else(
        min_28, "Private room MTR", "Private room STR")) |> 
      select(-min_28) |> 
      rename(value = GH))

# 2018/2023 shares
housing_loss |> 
  filter(month == yearmonth("2018-04")) |> 
  summarize(mtr_share = sum(value[str_detect(listing, "MTR")]) / sum(value), 
            .by = city)

housing_loss |> 
  filter(month == yearmonth("2023-04")) |> 
  summarize(mtr_share = sum(value[str_detect(listing, "MTR")]) / sum(value), 
            .by = city)

figure_3 <-
  housing_loss |> 
  mutate(listing = factor(listing, levels = c(
    "Private room MTR", "Entire home MTR", "Private room STR", 
    "Entire home STR"))) |> 
  ggplot(aes(month, value, fill = listing)) +
  geom_col(lwd = 0) +
  scale_fill_manual(name = "Listing type", 
                    values = col_palette[c(1, 2, 5, 4)]) +
  scale_x_yearmonth(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = "Housing units", labels = scales::comma) +
  facet_wrap(~city, scales = "free_y", nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"), 
        strip.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

ggsave("output/figure_3.png", plot = figure_3, width = 8, height = 7, 
       units = "in")


# Figure 4 ----------------------------------------------------------------

ltr_shares <- 
  ltr |> 
  st_drop_geometry() |> 
  filter(scraped >= "2020-06-01", scraped <= "2022-12-31") |> 
  summarize(short_pct = mean(short_long == "short", na.rm = TRUE), 
            furn_pct = mean(furnished, na.rm = TRUE), .by = c(scraped, city)) |> 
  filter(short_pct >= 0.01, furn_pct > 0.1) |>
  mutate(furn_pct = slide_dbl(furn_pct, mean, .before = 1),
         short_pct = slide_dbl(short_pct, mean, .before = 1)) |> 
  pivot_longer(c(short_pct, furn_pct))

# Furnished share at end of 2022
ltr_shares |> 
  filter(name == "furn_pct", scraped == max(scraped), .by = city)

figure_4 <-
  ltr_shares |> 
  mutate(label = if_else(scraped == "2020-10-07" & name == "furn_pct", 
                         city, NA_character_)) |>
  mutate(name = if_else(
    name == "short_pct", "Share of Kijiji listings which are monthly rentals",
    "Share of Craigslist/Kijiji listings which are furnished rentals")) |> 
  ggplot(aes(scraped, value, colour = city)) +
  geom_line(lwd = 1) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  scale_colour_manual(values = col_palette[c(1, 5, 2)]) +
  scale_y_continuous(name = "Share of listings", labels = scales::percent) +
  scale_x_date(name = NULL) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_4.png", plot = figure_4, width = 8, height = 5, 
       units = "in")


# Figure 5 ----------------------------------------------------------------

rents <- 
  ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(short_long), !is.na(price)) |> 
  filter(price > 425, price < 8000) |> 
  mutate(month = yearmonth(scraped)) |> 
  filter(month >= yearmonth("2020-04")) |> 
  summarize(rent = mean(price), .by = c(month, short_long, city)) |> 
  transmute(platform = "Kijiji", month, city,
            type = if_else(short_long == "short", "MTR", "LTR"), rent)

rents <- 
  monthly |> 
  mutate(min_28 = minimum_stay >= 28) |> 
  filter(month >= yearmonth("2020-04"), revenue > 0) |> 
  summarize(rent = sum(revenue) / sum(R), .by = c(month, min_28, city)) |> 
  transmute(platform = "Airbnb/Vrbo", month, city,
            type = if_else(min_28, "MTR", "STR"), rent) |> 
  bind_rows(rents)

# Correlations
rents |> 
  filter(year(month) != 2023) |> 
  mutate(platform = if_else(platform == "Kijiji", "kj", "ab")) |> 
  pivot_wider(id_cols = c(month, city), names_from = c(platform, type), 
              values_from = rent) |> 
  group_split(city) |> 
  map(\(x) cor(select(x, -month, -city)))
  
figure_5 <-
  rents |> 
  filter(year(month) != 2023) |> 
  mutate(label = if_else(month == yearmonth("2021-09") & city == "Montreal", 
                         type, NA_character_)) |>
  ggplot(aes(month, rent, colour = type)) +
  geom_line(lwd = 1) +
  ggrepel::geom_label_repel(aes(label = label), alpha = 0.9, family = "Futura", 
                            size = 3) +
  facet_grid(rows = vars(platform), cols = vars(city), scale = "free_y") + 
  scale_y_continuous(name = "Nightly (top) or monthly (bottom) asking prices", 
                     labels = scales::dollar) +
  scale_x_yearmonth(name = NULL, breaks = as.Date(
    c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")), 
    labels = c("Jan 2020", "Jan 2021", "Jan 2022", "Jan 2023")) +
  scale_colour_manual(values = col_palette[c(2, 5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_5.png", plot = figure_5, width = 8, height = 5, 
       units = "in")
