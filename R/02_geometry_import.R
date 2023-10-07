#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)


# CMAs --------------------------------------------------------------------

CMA <-
  get_census(
    dataset = "CA21", regions = list(CMA = c("35535", "24462", "59933")), 
    geo_format = "sf") |> 
  st_transform(3347) |> 
  as_tibble() |> 
  st_as_sf() |> 
  transmute(GeoUID, name = str_remove(name, " \\(B\\)"), geometry)


# CSDs --------------------------------------------------------------------

city <-
  get_census(
    dataset = "CA21", regions = list(CSD = c("3520005", "2466023", "5915022")), 
    geo_format = "sf") |> 
  st_transform(3347) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(GeoUID, name, Dwellings, geometry) |> 
  set_names(c("GeoUID", "name", "dwellings", "geometry")) |> 
  st_set_agr("constant") |> 
  mutate(name = str_remove(name, " \\(V\\)")) |> 
  mutate(name = str_remove(name, " \\(C\\)")) |> 
  mutate(name = str_remove(name, " \\(CY\\)"))


# CTs ---------------------------------------------------------------------

CT <-
  get_census(
    dataset = "CA21", regions = list(CSD = c("3520005", "2466023", "5915022")), 
    level = "CT", geo_format = "sf") |> 
  st_transform(3347) |> 
  select(-name) |> 
  left_join(st_drop_geometry(select(city, CSD_UID = GeoUID, name)), 
            by = "CSD_UID") |> 
  select(GeoUID, name, Dwellings) |> 
  set_names(c("GeoUID", "name", "dwellings", "geometry")) |> 
  st_set_agr("constant")


# Water -------------------------------------------------------------------

water <-
  read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(3347) |> 
  st_filter(CMA)


# Save output -------------------------------------------------------------

qsavem(city, CMA, CT, file = "output/data/geometry.qsm")
qsave(water, file = "output/data/water.qs")
