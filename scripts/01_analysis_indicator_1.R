#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Analysis - Indicator 1 - Participation in the labor force rates
#' @last-update 2024-02-15
#' @update-description Starting analysis
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
    # 5-years-aged
    labor_force_total_5age <- total_5age

    labor_force_sex_5age <- sex_5age

    labor_force_sex_race_5age <- sex_race_5age

    labor_force_sex_educ_5age <- sex_educ_5age

    # 10-years-aged
    labor_force_total_10age <- total_10age

    labor_force_sex_10age <- sex_10age

    labor_force_sex_race_10age <- sex_race_10age

    labor_force_sex_educ_10age <- sex_educ_10age

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
  rm(
    total_5age, sex_5age, sex_race_5age, sex_educ_5age,
    total_10age, sex_10age, sex_race_10age, sex_educ_10age
  )

  print(paste0("Year ", years[i]," is finished!!!"))
}


# Saving outputs - table -----------------------------------------------------

## 5-years - Total

txt <-  labor_force_total_5age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento,txt),
               row.vars = c("coorte_nascimento"),
               col.vars = c("ano", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "1.1_tot_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex

txt <-  labor_force_sex_5age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "2.1_sex_5anos",
  append = TRUE,
  showNA = FALSE
)

## 5-years - By sex and race

txt <-  labor_force_sex_race_5age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo + cor_raca, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano","cor_raca", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo e cor/raça - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "3.1_sex_raca_5anos",
  append = TRUE,
  showNA = FALSE
)

# 5-years - By sex and education

txt <-  labor_force_sex_educ_5age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo + escolaridade, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano","escolaridade", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo e escolaridade - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "4.1_sex_educ_5anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - Total

txt <-  labor_force_total_10age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento, txt),
         row.vars = c("coorte_nascimento"),
         col.vars = c("ano", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "1.2_tot_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex

txt <-  labor_force_sex_10age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "2.2_sex_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and race

txt <-  labor_force_sex_race_10age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo + cor_raca, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano","cor_raca", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo e cor/raça - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "3.2_sex_raca_10anos",
  append = TRUE,
  showNA = FALSE
)

## 10-years - By sex and education

txt <-  labor_force_sex_educ_10age |>
  select(-grupo_etario) |>
  pivot_longer(
    numerador:taxa,
    names_to = "indice",
    values_to = "value"
  )

txt <-  ftable(xtabs(value ~ ano + indice + coorte_nascimento + sexo + escolaridade, txt),
         row.vars = c("sexo", "coorte_nascimento"),
         col.vars = c("ano","escolaridade", "indice")) %>%
  stats:::format.ftable(quote = FALSE, dec = ",") %>%
  trimws() %>%
  as.data.frame()

# Constructing table

title <- matrix(ncol = dim(txt)[2], nrow = 2)
title[1,1] <- "Tabela: Taxa de participação na força de trabalho por coorte de nascimento, segundo sexo e escolaridade - Brasil, 1960-2010"

note <- matrix(ncol = dim(txt)[2], nrow = 7)
note[2,1] <- "Fonte: IBGE/Censo Demográfico brasileiro."
note[4,1] <- "Nota:"
note[5,1] <- "1. Numerador = PEA."
note[6,1] <- "2. Denominador = PIA."
note[7,1] <- "3. Para 1991, os grupos etários iniciam na idade 16, mas foram apresentados em termos de grupos convencionais."

table_export <- rbind(title,txt, note)

# Saving file

write.xlsx(
  table_export,
  file = file.path("./outputs","Indicador 1 - Taxa de participação.xlsx"),
  row.names = FALSE,
  col.names = FALSE,
  sheetName = "4.2_sex_educ_10anos",
  append = TRUE,
  showNA = FALSE
)

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
  sheetName = "1.1_tot_5anos",
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
  sheetName = "2.1_sex_5anos",
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
  sheetName = "3.1_sex_raca_5anos",
  append = TRUE,
  showNA = FALSE
)

# 5-years - By sex and education

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
  sheetName = "4.1_sex_educ_5anos",
  append = TRUE,
  showNA = FALSE
)

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
  sheetName = "1.2_tot_10anos",
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
  sheetName = "2.2_sex_10anos",
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
  sheetName = "3.2_sex_raca_10anos",
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
  sheetName = "4.2_sex_educ_10anos",
  append = TRUE,
  showNA = FALSE
)
