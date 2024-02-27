#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Analysis - Indicator 1 - Participation in the labor force rates
#' @last-update 2024-02-19
#' @update-description Adjusting outputs
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
invisible(gc())

# libraries -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, stats, xlsx)

# Auxiliar functions ------------------------------------------------------

source("./scripts/X_functions_indicators.R")

# Looping -----------------------------------------------------------------

years <- c(1960,1970,1980,1991,2000,2010)

for(i in seq_along(years)){

  print(paste0("Starting year: ", years[i],"..."))

  # Importing data

  data <- read_parquet(
    file = file.path("./inputs",glue::glue("censo_{years[i]}.parquet"))
  )

  ## 10-years aged data

  # Total

  total_10age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "total"
  ) |>
    mutate(ano = years[i])

  # By sex

  sex_10age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo"
  ) |>
    mutate(ano = years[i])

  # By sex and race

  sex_race_10age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e raca"
  ) |>
    mutate(ano = years[i])

  # By sex and education level

  sex_educ_10age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e escolaridade"
  ) |>
    mutate(ano = years[i])

  # output

  if(i == 1){
    # 10-years-aged
    labor_force_total_10age <- total_10age

    labor_force_sex_10age <- sex_10age

    labor_force_sex_race_10age <- sex_race_10age

    labor_force_sex_educ_10age <- sex_educ_10age

  } else{
    # 10-years-aged
    labor_force_total_10age <- labor_force_total_10age |>
      bind_rows(total_10age)

    labor_force_sex_10age <- labor_force_sex_10age |>
      bind_rows(sex_10age)

    labor_force_sex_race_10age <- labor_force_sex_race_10age |>
      bind_rows(sex_race_10age)

    labor_force_sex_educ_10age <- labor_force_sex_educ_10age |>
      bind_rows(sex_educ_10age)

  }

  # next loop
  rm(total_10age, sex_10age, sex_race_10age, sex_educ_10age)

  print(paste0("Year ", years[i]," is finished!!!"))
}

# Saving outputs - dataframe -----------------------------------------------------

## 10-years - Total

table_export <-  labor_force_total_10age |>
  select(ano, everything()) |>
  rename(
    "PEA" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "1_tot_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex

table_export <-  labor_force_sex_10age |>
  select(ano, everything()) |>
  rename(
    "PEA" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "2_sex_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and race

table_export <-  labor_force_sex_race_10age |>
  select(ano, everything()) |>
  rename(
    "PEA" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "3_sex_raca_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and education

table_export <-  labor_force_sex_educ_10age |>
  select(ano, everything()) |>
  rename(
    "PEA" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "4_sex_educ_10anos",
  append = TRUE,
  showNA = FALSE
)
