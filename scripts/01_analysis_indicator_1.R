#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Analysis
#' @last-update 2024-02-15
#' @update-description Starting analysis
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
invisible(gc())

# libraries -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow)

# Auxiliar functions ------------------------------------------------------

source("./scripts/X_functions_indicators.R")

data <- read_parquet(
  file = file.path("./inputs","censo_1960.parquet")
)

### Indicator 1

## 5-years aged data

# Total

labor_force_5age_total <- labor_force_rates(
  df = data,
  grupo_etario = "idade_quinquenal",
  coorte_nascimento = "coorte_quinquenal",
  tipo = "total"
)

# By sex

labor_force_5age_sex <- labor_force_rates(
  df = data,
  grupo_etario = "idade_quinquenal",
  coorte_nascimento = "coorte_quinquenal",
  tipo = "sexo"
)

# By sex and race

labor_force_5age_sex_race <- labor_force_rates(
  df = data,
  grupo_etario = "idade_quinquenal",
  coorte_nascimento = "coorte_quinquenal",
  tipo = "sexo e raca"
)

# By sex and education level

labor_force_5age_sex_educ <- labor_force_rates(
  df = data,
  grupo_etario = "idade_quinquenal",
  coorte_nascimento = "coorte_quinquenal",
  tipo = "sexo e escolaridade"
)

## 10-years aged data

# Total

labor_force_10age_total <- labor_force_rates(
  df = data,
  grupo_etario = "idade_decenal",
  coorte_nascimento = "coorte_decenal",
  tipo = "total"
)

# By sex

labor_force_10age_sex <- labor_force_rates(
  df = data,
  grupo_etario = "idade_decenal",
  coorte_nascimento = "coorte_decenal",
  tipo = "sexo"
)

# By sex and race

labor_force_10age_sex_race <- labor_force_rates(
  df = data,
  grupo_etario = "idade_decenal",
  coorte_nascimento = "coorte_decenal",
  tipo = "sexo e raca"
)

# By sex and education level

labor_force_10age_sex_educ <- labor_force_rates(
  df = data,
  grupo_etario = "idade_decenal",
  coorte_nascimento = "coorte_decenal",
  tipo = "sexo e escolaridade"
)

