#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Analysis - Indicator 2 - Occupation Level - 5year age group
#' @last-update 2024-02-27
#' @update-description Creating script for 5 year age group analysis
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

  total_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "total"
  ) |>
    mutate(ano = years[i])

  # By sex

  sex_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo"
  ) |>
    mutate(ano = years[i])

  # By sex and race

  sex_race_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e raca"
  ) |>
    mutate(ano = years[i])

  # By sex and education level

  sex_educ_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e escolaridade"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (aggregated)

  sex_ocup_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e ocupacao"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (aggregated)

  sex_ocup_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e ocupacao"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (disaggregated)

  sex_ocup4d_5age <- occupation_level(
    df = data,
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo = "sexo e ocupacao4d"
  ) |>
    mutate(ano = years[i])

  # output

  if(i == 1){
    # 5-years-aged
    occup_level_total_5age <- total_5age

    occup_level_sex_5age <- sex_5age

    occup_level_sex_race_5age <- sex_race_5age

    occup_level_sex_educ_5age <- sex_educ_5age

    occup_level_sex_ocup_5age <- sex_ocup_5age

    occup_level_sex_ocup4d_5age <- sex_ocup4d_5age

  } else{
    # 5-years-aged
    occup_level_total_5age <- occup_level_total_5age |>
      bind_rows(total_5age)

    occup_level_sex_5age <- occup_level_sex_5age |>
      bind_rows(sex_5age)

    occup_level_sex_race_5age <- occup_level_sex_race_5age |>
      bind_rows(sex_race_5age)

    occup_level_sex_educ_5age <- occup_level_sex_educ_5age |>
      bind_rows(sex_educ_5age)

    occup_level_sex_ocup_5age <- occup_level_sex_ocup_5age |>
      bind_rows(sex_ocup_5age)

    occup_level_sex_ocup4d_5age <- occup_level_sex_ocup4d_5age |>
      bind_rows(sex_ocup4d_5age)

  }

  # next loop
  rm(total_5age, sex_5age, sex_race_5age, sex_educ_5age, sex_ocup_5age, sex_ocup4d_5age)

  print(paste0("Year ", years[i]," is finished!!!"))
}

# Saving outputs - dataframe -----------------------------------------------------

## 5-years - Total

table_export <-  occup_level_total_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "1_tot_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex

table_export <-  occup_level_sex_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "2_sex_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and race

table_export <-  occup_level_sex_race_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "3_sex_raca_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and education

table_export <-  occup_level_sex_educ_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "4_sex_educ_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and occupation

table_export <-  occup_level_sex_ocup_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "5_sex_ocup_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and occupation disaggregated

table_export <-  occup_level_sex_ocup4d_5age |>
  select(ano, everything()) |>
  rename(
    "PO" = numerador,
    "PIA" = denominador,
  ) |>
  arrange(ano, grupo_etario, ocupacao) |>
  as.data.frame()


# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.xlsx"),
  row.names = FALSE,
  col.names = TRUE,
  sheetName = "6_sex_ocup4d_5anos",
  append = TRUE,
  showNA = FALSE
)
write_csv2(
  table_export,
  file = file.path("./outputs","Indicador 2 - Nível de ocupação - base de dados.csv"),
  col_names = TRUE,
  append = TRUE
)

rm(list = ls())
