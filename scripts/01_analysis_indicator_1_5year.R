#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Indicator 1 - Participation in the labor force rates - 5years group
#' @last-update 2024-02-27
#' @update-description Creating outputs for 5-years age group
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

  ## 5-years aged data

  # Total

  total_5age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "total"
  ) |>
    mutate(ano = years[i])

  # By sex

  sex_5age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo"
  ) |>
    mutate(ano = years[i])

  # By sex and race

  sex_race_5age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e raca"
  ) |>
    mutate(ano = years[i])

  # By sex and education level

  sex_educ_5age <- labor_force_rates(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e escolaridade"
  ) |>
    mutate(ano = years[i])

  # output

  if(i == 1){
    # 10-years-aged
    labor_force_total_5age <- total_5age

    labor_force_sex_5age <- sex_5age

    labor_force_sex_race_5age <- sex_race_5age

    labor_force_sex_educ_5age <- sex_educ_5age

  } else{
    # 5-years-aged
    labor_force_total_5age <- labor_force_total_5age |>
      bind_rows(total_5age)

    labor_force_sex_5age <- labor_force_sex_5age |>
      bind_rows(sex_5age)

    labor_force_sex_race_5age <- labor_force_sex_race_5age |>
      bind_rows(sex_race_5age)

    labor_force_sex_educ_5age <- labor_force_sex_educ_5age |>
      bind_rows(sex_educ_5age)

  }

  # next loop
  rm(total_5age, sex_5age, sex_race_5age, sex_educ_5age)

  print(paste0("Year ", years[i]," is finished!!!"))
}

# Saving outputs - dataframe -----------------------------------------------------

## 5-years - Total

table_export <-  labor_force_total_5age |>
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
  sheetName = "1_tot_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex

table_export <-  labor_force_sex_5age |>
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
  sheetName = "2_sex_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and race

table_export <-  labor_force_sex_race_5age |>
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
  sheetName = "3_sex_raca_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and education

table_export <-  labor_force_sex_educ_5age |>
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
  sheetName = "4_sex_educ_5anos",
  append = TRUE,
  showNA = FALSE
)
