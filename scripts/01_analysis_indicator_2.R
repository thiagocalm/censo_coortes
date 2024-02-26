#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Analysis - Indicator 2 - Occupation Level
#' @last-update 2024-02-26
#' @update-description Including disaggregated occupation analysis
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

  # Transforming occupational variable in a factor type

  codes <- data |> select(cod_ocupacao_4d) |> unique() |> pull()

  labels <- read_csv2(
    file = file.path("./docs","codigo_cbo_censo.csv"),
    col_names = TRUE,
    trim_ws = TRUE
  ) |>
    filter(Ano %in% years[i]) |>
    filter(Codigo %in% codes) |>
    select(Rotulo) |> pull()

  data <- data |>
    mutate(
      cod_ocupacao_4d = factor(
        cod_ocupacao_4d,
        levels = codes,
        labels = labels
      )
    )

  ## 10-years aged data

  # Total

  total_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "total"
  ) |>
    mutate(ano = years[i])

  # By sex

  sex_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo"
  ) |>
    mutate(ano = years[i])

  # By sex and race

  sex_race_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e raca"
  ) |>
    mutate(ano = years[i])

  # By sex and education level

  sex_educ_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e escolaridade"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (aggregated)

  sex_ocup_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e ocupacao"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (aggregated)

  sex_ocup_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e ocupacao"
  ) |>
    mutate(ano = years[i])

  # By sex and occupation code (disaggregated)

  sex_ocup4d_10age <- occupation_level(
    df = data,
    grupo_etario = "idade_decenal",
    coorte_nascimento = "coorte_decenal",
    tipo = "sexo e ocupacao4d"
  ) |>
    mutate(ano = years[i])

  # output

  if(i == 1){
    # 10-years-aged
    occup_level_total_10age <- total_10age

    occup_level_sex_10age <- sex_10age

    occup_level_sex_race_10age <- sex_race_10age

    occup_level_sex_educ_10age <- sex_educ_10age

    occup_level_sex_ocup_10age <- sex_ocup_10age

    occup_level_sex_ocup4d_10age <- sex_ocup4d_10age

  } else{
    # 10-years-aged
    occup_level_total_10age <- occup_level_total_10age |>
      bind_rows(total_10age)

    occup_level_sex_10age <- occup_level_sex_10age |>
      bind_rows(sex_10age)

    occup_level_sex_race_10age <- occup_level_sex_race_10age |>
      bind_rows(sex_race_10age)

    occup_level_sex_educ_10age <- occup_level_sex_educ_10age |>
      bind_rows(sex_educ_10age)

    occup_level_sex_ocup_10age <- occup_level_sex_ocup_10age |>
      bind_rows(sex_ocup_10age)

    occup_level_sex_ocup4d_10age <- occup_level_sex_ocup4d_10age |>
      bind_rows(sex_ocup4d_10age)

  }

  # next loop
  rm(total_10age, sex_10age, sex_race_10age, sex_educ_10age, sex_ocup_10age, sex_ocup4d_10age)

  print(paste0("Year ", years[i]," is finished!!!"))
}

# Saving outputs - dataframe -----------------------------------------------------

## 10-years - Total

table_export <-  occup_level_total_10age |>
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
  sheetName = "1_tot_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex

table_export <-  occup_level_sex_10age |>
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
  sheetName = "2_sex_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and race

table_export <-  occup_level_sex_race_10age |>
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
  sheetName = "3_sex_raca_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and education

table_export <-  occup_level_sex_educ_10age |>
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
  sheetName = "4_sex_educ_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and occupation

table_export <-  occup_level_sex_ocup_10age |>
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
  sheetName = "5_sex_ocup_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and occupation disaggregated

table_export <-  occup_level_sex_ocup4d_10age |>
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
  sheetName = "5_sex_ocup4d_10anos",
  append = TRUE,
  showNA = FALSE
)
