#### LTR DATA IMPORT ###########################################################

source("R/01_startup.R")


# Load and filter data ----------------------------------------------------

kj <- qread("data/kj.qs", nthreads = availableCores())
cl <- qread("data/cl.qs", nthreads = availableCores())


# Clean location field ----------------------------------------------------

kj <-
  kj |> 
  mutate(location = str_remove(location, "^([:punct:]|[:space:])*"))


# Get geometry from KJ listings -------------------------------------------

# Get previously geocoded addresses
geolocation <-
  read_csv("data/geo.csv") |> 
  set_names(c("entity", "lon", "lat"))

processed_addresses <-
  geolocation |> 
  filter(entity %in% !!kj$location) |> 
  distinct(entity, .keep_all = TRUE)

kj_old_geography <-
  kj |> 
  inner_join(processed_addresses, by = c("location" = "entity"))

kj_new_geography <-
  kj |> 
  filter(!location %in% processed_addresses$entity, !is.na(location))

while (nrow(kj_new_geography) > 0) {

  arc_output <- 
    kj_new_geography |> 
    distinct(location) |> 
    slice(1:1000) |> 
    tidygeocoder::geocode(address = location, method = "arcgis") |> 
    select(location, lon = long, lat)
  
  locations_new <-
    arc_output |> 
    select(entity = location, lon, lat) |> 
    distinct(entity, .keep_all = TRUE)
  
  geolocation <-
    geolocation |> 
    bind_rows(locations_new) |> 
    distinct(entity, .keep_all = TRUE)
  
  write_csv(geolocation, "data/geo.csv")
  
  kj_new_geography_added <-
    kj_new_geography |> 
    inner_join(arc_output, by = "location")
  
  kj_new_geography_not_added <- 
    kj_new_geography |> 
    anti_join(arc_output, by = "location")
  
  kj <- bind_rows(kj_old_geography, kj_new_geography_added, 
                  kj_new_geography_not_added)
  
  processed_addresses <-
    geolocation |> 
    filter(entity %in% !!kj$location) |> 
    distinct(entity, .keep_all = TRUE)
  
  kj_old_geography <-
    kj |> 
    select(-lon, -lat) |> 
    inner_join(processed_addresses, by = c("location" = "entity"))
  
  kj_new_geography <-
    kj |> 
    filter(!location %in% processed_addresses$entity, !is.na(location)) |> 
    select(-lon, -lat)
  
  rm(kj_new_geography_added, kj_new_geography_not_added)
  
}

kj <- 
  kj |> 
  select(-any_of(c("lon", "lat"))) |> 
  inner_join(processed_addresses, by = c("location" = "entity"))

suppressWarnings(rm(arc_output, processed_addresses, kj_old_geography, 
                    kj_new_geography, locations_new, geolocation))


# Clean up KJ file --------------------------------------------------------

kj <-
  kj |> 
  mutate(bedrooms = str_replace(bedrooms, pattern = "Bachelor/Studio|Studio",
                                replacement = "0"),
         bedrooms = as.numeric(str_sub(bedrooms, end = 1L)),
         bathrooms = str_sub(bathrooms, end = 3L),
         bathrooms = str_replace(bathrooms, pattern = "Ut|U|\\+",
                                 replacement = ""),
         bathrooms = as.numeric(bathrooms),
         type = details |> 
           str_extract("(?<=Agreement Type).*?(?=(Move-In)|(Pet Friendly))") |> 
           str_remove('</dd.*') |> 
           str_remove('.*">'),
         type = if_else(type == "Not Available", NA_character_, type))

kj <-
  kj |> 
  select(id, short_long:furnished, type, lat, lon, title, text, photos) |> 
  mutate(kj = TRUE)

kj_with_geom <-
  kj |> 
  filter(!is.na(lon), !is.na(lat)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

kj_without_geom <-
  kj |> 
  filter(is.na(lon) | is.na(lat)) |> 
  mutate(geometry = st_sfc(st_point())) |> 
  st_as_sf(crs = 4326) |> 
  select(-lon, -lat)

kj <-
  bind_rows(kj_with_geom, kj_without_geom) |> 
  arrange(scraped, id)

rm(kj_with_geom, kj_without_geom)


# Clean up CL file --------------------------------------------------------

cl <-
  cl |> 
  select(id, created:furnished, title, text, photos) |> 
  separate(location, c("lat", "lon"), sep = ";") |> 
  mutate(city = "Toronto",
         bedrooms = as.numeric(bedrooms),
         bathrooms = as.numeric(bathrooms),
         created = as.Date(created),
         scraped = as.Date(scraped),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         short_long = NA,
         location = NA,
         type = NA,
         kj = FALSE)

cl_with_geom <-
  cl |> 
  filter(!is.na(lon), !is.na(lat)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cl_without_geom <-
  cl |> 
  filter(is.na(lon) | is.na(lat)) |> 
  mutate(geometry = st_sfc(st_point())) |> 
  st_as_sf(crs = 4326) |> 
  select(-lon, -lat)

cl <-
  bind_rows(cl_with_geom, cl_without_geom) |> 
  arrange(scraped, id)

rm(cl_with_geom, cl_without_geom)


# Bind into one table -----------------------------------------------------

ltr <- bind_rows(kj, select(cl, names(kj)))

rm(kj, cl)


# Add geometry ------------------------------------------------------------

qload("output/data/geometry.qsm", nthreads = availableCores())

ltr <- st_transform(ltr, 3347)

ltr <-
  ltr |> 
  st_join(CT) |> 
  select(-dwellings, -name) |> 
  rename(CT = GeoUID) |> 
  relocate(CT, .before = geometry)


# Save output -------------------------------------------------------------

qsave(ltr, file = "output/data/ltr.qs", nthreads = availableCores())
