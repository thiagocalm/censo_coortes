#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Download data
#' @last-update 2024-02-06
#' @update-description Adjusting loop and handling data
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
invisible(gc())

# libraries -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, ipumsr)

# Import data ----------------------------------------------------

# setting DDI vector for each year

ddis <- c("ipumsi_census_1960.xml")

# Reading parse file ipums

censo_ddi <- read_ipums_ddi(
  file.path("./inputs/raw", ddis),
  lower_vars = TRUE
)

# Renaming ipums .dat file

censo_ddi$file_name <- paste0("ipumsi_census_1960.dat")

# Reading microdata file ipums

censo <- ipumsr::read_ipums_micro_chunked(
  ddi =   censo_ddi,
  callback = IpumsDataFrameCallback$new(
    function(x, pos) {
      return(
        x %>%
          filter(!is.na(age)) |>
          filter(age >= 15) |>
          mutate(
            id_pessoa = as.numeric(paste0(serial, pernum))
          ) |>
          rename(
            "ano" = year,
            "peso" = perwt,
            "idade" = age,
            "sexo" = sex,
            "filhos_vivos" = chborn,
            "filhos_sobreviventes" = chsurv,
            "raca_cor" = race,
            "escolaridade" = edattain,
            "cond_atividade" = empstat,
            "cond_forca_trabalho" = labforce,
            "posicao_ocupacao" = classwk
          ) |>
          select(-c(country, sample, serial, hhwt, pernum, resident, edattaind, empstatd, classwkd))
      )}
  ),
  chunk_size = 10000,
  verbose = TRUE
)

# Validating some derived variables that will be created

censo |>
  select(cond_forca_trabalho, cond_atividade) |>
  mutate(
    PEA_1 = case_when(cond_forca_trabalho == 2 ~ 1, TRUE ~ 0),
    PEA_2 = case_when(cond_atividade %in% c(1,2) ~ 1, TRUE ~ 0)
  ) |>
  summarise(
    PEA_1 = sum(PEA_1),
    PEA_2 = sum(PEA_2)
  )
