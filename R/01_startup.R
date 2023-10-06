#### 01 STARTUP ################################################################

# There is usually no need to run this script directly; it is sourced from the
# other scripts which need it.


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)
library(future)
library(progressr)
library(slider)
library(data.table)
library(gt)
library(patchwork)
library(qs)
library(tsibble)
# remotes::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
library(ggh4x)
library(feasts)
library(fable)


# Set global variables ----------------------------------------------------

plan(multisession)
handlers(global = TRUE)
col_palette <- c(
  "#14325C", # 1
  "#5398D9", # 2
  "#F4E3B1", # 3
  "#D96B0C", # 4 
  "#A53A3B", # 5 
  "#A2E8AF", # 6
  "#C7C9C9") # 7

