#### STR PROCESSING ############################################################

source("R/01_startup.R")
library(caret)

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")


# FREH --------------------------------------------------------------------

monthly <- 
  monthly |> 
  arrange(property_ID, month) |> 
  mutate(R_12 = slide_dbl(R, sum, .before = 11), 
         A_12 = slide_dbl(A, sum, .before = 11), 
         AR_12 = R_12 + A_12, 
         .by = property_ID) |> 
  mutate(FREH = R_12 >= 90 & AR_12 >= 183 & listing_type == "Entire home/apt", 
         .after = revenue)


# Produce monthly activity table for all EH listings ----------------------

monthly_FREH <-
  monthly |> 
  left_join(select(property, property_ID, created), by = "property_ID") |> 
  # Trim listings to the start of the month
  mutate(created = if_else(day(created) == 1, created,
                           floor_date(created, "month") %m+% months(1))) |> 
  mutate(year = year(month),
         month = month(month),
         created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 +
           (month - created_month)) |> 
  select(property_ID, year, month, month_since_created, R, A, B, FREH)


# Model based on last 3 months --------------------------------------------

# Summarize by month
after_one_year_2022 <-
  monthly_FREH |> 
  filter(year >= 2022) |> 
  mutate(month = month.name[.data$month], AR = A + R) |> 
  group_by(property_ID) |> 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) |> 
  ungroup() |> 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)


# Fit models and apply to listings > 2 months -----------------------------

model_2022 <- glm(FREH ~ R_3 + AR_3 + month, data = after_one_year_2022,
                  family = binomial)

model_2022_results <-
  monthly_FREH |> 
  mutate(month = month.name[.data$month], AR = A + R) |> 
  group_by(property_ID) |> 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) |> 
  ungroup() |> 
  filter(!is.na(R_3), !is.na(AR_3)) |> 
  modelr::add_predictions(model_2022, type = "response") |> 
  mutate(FREH_3 = pred) |> 
  select(-pred) |> 
  rowwise() |> 
  mutate(month = which(month.name == month)) |> 
  ungroup()

monthly <-
  monthly |> 
  mutate(year = year(month), month_int = month(month)) |> 
  left_join(select(model_2022_results, property_ID, year, month_int = month, 
                   FREH_3), by = c("property_ID", "year", "month_int")) |> 
  mutate(FREH_2022 = coalesce(FREH_3, 0)) |> 
  relocate(FREH_2022, .after = FREH) |> 
  select(-year, -month_int, -FREH_3)


# Model testing -----------------------------------------------------------

# Split the data into training and test set
training_samples_2022 <-
  after_one_year_2022$FREH %>% createDataPartition(p = 0.80, list = FALSE)

train_data_2022 <- after_one_year_2022[training_samples_2022, ]
test_data_2022 <- after_one_year_2022[-training_samples_2022, ]

# Fit the model
model_2022_test <- glm(FREH ~ R_3 + AR_3 + month, data = train_data_2022,
                       family = binomial)

# Test models
probabilities_2022 <- 
  predict(model_2022_test, test_data_2022, type = "response")
predicted_classes_2022 <- ifelse(probabilities_2022 > 0.5, "TRUE", "FALSE")
mean(predicted_classes_2022 == test_data_2022$FREH)
# Outcome: 0.905


# GH ----------------------------------------------------------------------

GH <- 
  property |> 
  strr_ghost() |> 
  select(-data)

GH <- 
  GH |> 
  mutate(month = yearmonth(date)) |> 
  summarize(n = n(), .by = c(ghost_ID, month, host_ID, listing_count, 
                             housing_units, property_IDs, geometry)) |> 
  relocate(n, .after = month)

GH <-
  GH |> 
  # st_drop_geometry() |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  left_join(select(monthly, property_ID, month, R, A, B), 
            by = c("property_ID", "month")) |> 
  summarize(property_IDs = list(property_ID),
            active_pct = sum(R + A) / sum(R + A + B),
            .by = c(ghost_ID, month, n, host_ID, listing_count, housing_units, 
                    geometry)) |> 
  relocate(property_IDs, .before = geometry)

GH <-
  GH |>
  filter(month <= yearmonth("2023-05"))


# Add city to monthly -----------------------------------------------------

monthly <- 
  monthly |> 
  left_join(st_drop_geometry(select(property, property_ID, city)), 
            by = "property_ID") |> 
  relocate(city, CT, .after = listing_type)



# Save output -------------------------------------------------------------

qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())
qsave(GH, file = "output/data/GH.qs", nthreads = availableCores())
qsavem(after_one_year_2022, model_2022, model_2022_results,
       file = "output/data/FREH_model.qsm", nthreads = availableCores())

rm(after_one_year_2022, model_2022, model_2022_results, model_2022_test, 
   monthly_FREH, test_data_2022, train_data_2022, training_samples_2022, 
   predicted_classes_2022, probabilities_2022)
