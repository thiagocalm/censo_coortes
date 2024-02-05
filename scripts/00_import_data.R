#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Download data
#' @last-update 2024-02-05
#' @update-description Creating looping for download data
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# libraries -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, ipumsr)

# Import data ----------------------------------------------------

# setting DDI vector for each year

ddis <- c("ipumsi_census_1960.xml")

# Reading parse file ipums

censo_ddi <- read_ipums_ddi(
  file.path("./inputs/raw data", ddis),
  lower_vars = TRUE
)

# Reading microdata file ipums

censo <- ipumsr::read_ipums_micro_chunked(
  ddi =   censo_ddi,
  data_file = file.path("./inputs/raw data"),
  callback = IpumsDataFrameCallback$new(
    function(x, pos) {
      return(x %>%
               filter(!is.na(age)) |>
               filter(age >= 15)
      )}
  ),
  chunk_size = 10000,
  verbose = TRUE
)
