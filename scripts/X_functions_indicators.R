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
pacman::p_load(tidyverse, arrow)

# Import data -------------------------------------------------------------

df <- read_parquet(
  file.path("./inputs","censo_2000.parquet")
)

# Populacao total

df |>
  summarise(n = sum(peso))

# Populacao total por idade

df |>
  summarise(
    n = sum(peso),
    .by = c(idade_quinquenal)
  ) |>
  arrange(idade_quinquenal)

# Populacao Economicamente Ativa

df |>
  summarise(
    n = sum(peso),
    n_pea = sum(peso[PEA == 1]),
    n_pnea = sum(peso[PEA == 0]),
    .by = c(idade_quinquenal)
  ) |>
  arrange(idade_quinquenal)


# Indicador 1 - Taxa de participacao --------------------------------------

## Parameters
peso = "peso"
grupo_etario = "idade_quinquenal"
coorte_nascimento = "coorte_quinquenal"
PEA = "PEA"
tipo = c("total","sexo","sexo e raca","sexo e escolaridade")

## Function

labor_force_rates <- function(
    peso = "peso",
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    PEA = "PEA",
    tipo # = c("total","sexo","sexo e raca","sexo e escolaridade")
    ){

  if(is.null(tipo)){
    stop("You need for describing wich type of tabulation you want before...")
  }

  if(tipo == "total"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA)
      )

    # Numerator

    num <- df |>
      filter(PEA == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      arrange(grupo_etario)

    # Denominator

    den <- df |>
      summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      arrange(grupo_etario)

    # join

    table <- num |>
      left_join(
        den,
        by = c("coorte_nascimento","grupo_etario"),
        keep = FALSE
      ) |>
      mutate(across(everything(),~ round(.x,0))) |>
      mutate(taxa = numerador/denominador)
  } if(tipo == "sexo"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo"
      )

    # Numerator

    num <- df |>
      filter(PEA == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      arrange(grupo_etario)

    # Denominator

    den <- df |>
      summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      arrange(grupo_etario)

    # join

    table <- num |>
      left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo"),
        keep = FALSE
      ) |>
      mutate(across(everything(),~ round(.x,0))) |>
      mutate(taxa = numerador/denominador) |>
      mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino"))
      )
  } if(tipo == "sexo e raca"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo",
        cor_raca = "cor_raca"
      )

    # Numerator

    num <- df |>
      filter(PEA == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo,cor_raca)
      ) |>
      arrange(grupo_etario)

    # Denominator

    den <- df |>
      summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, cor_raca)
      ) |>
      arrange(grupo_etario)

    # join

    table <- num |>
      left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "cor_raca"),
        keep = FALSE
      ) |>
      mutate(across(everything(),~ round(.x,0))) |>
      mutate(taxa = numerador/denominador) |>
      mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        cor_raca = factor(
          cor_raca,
          levels = c(1,2,3,4,5,99),
          labels = c("Branco","Preto","Indígena","Amarelo","Pardo","NR")
        )
      )
  } if(tipo == "sexo e escolaridade"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo",
        escolaridade = "escolaridade"
      )

    # Numerator

    num <- df |>
      filter(PEA == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      arrange(grupo_etario)

    # Denominator

    den <- df |>
      summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      arrange(grupo_etario)

    # join

    table <- num |>
      left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "escolaridade"),
        keep = FALSE
      ) |>
      mutate(across(everything(),~ round(.x,0))) |>
      mutate(taxa = numerador/denominador) |>
      mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        escolaridade = factor(
          escolaridade,
          levels = c(1,2,3,4,9),
          labels = c("Menos que fundamental completo","Fundamental completo","Médio completo","Superior completo","NR")
        )
      )
  }

  # output
  return(table)
}

