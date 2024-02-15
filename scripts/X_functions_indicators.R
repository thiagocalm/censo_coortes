#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @code-description Function for indicators
#' @last-update 2024-02-14
#' @update-description Inserting 4th indicator
#' -----------------------------------------------------

# Indicator 1 - Labor force participation rate ----------------------------------

## Function

labor_force_rates <- function(
    df,
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
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA)
      )

    # Numerator

    num <- df |>
      dplyr::filter(PEA == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador)
  }
  if(tipo == "sexo"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PEA == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino"))
      )
  }
  if(tipo == "sexo e raca"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo",
        cor_raca = "cor_raca"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PEA == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo,cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "cor_raca"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        cor_raca = factor(
          cor_raca,
          levels = c(1,2,3,4,5,99),
          labels = c("Branco","Preto","Indígena","Amarelo","Pardo","NR")
        )
      )
  }
  if(tipo == "sexo e escolaridade"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PEA = all_of(PEA),
        sexo = "sexo",
        escolaridade = "escolaridade"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PEA == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "escolaridade"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
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

# Indicator 2 - Ocupation level --------------------------------------

## Function

ocupation_level <- function(
    df,
    peso = "peso",
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    PO = "PO",
    tipo # = c("total","sexo","sexo e raca","sexo e escolaridade")
){

  if(is.null(tipo)){
    stop("You need for describing wich type of tabulation you want before...")
  }

  if(tipo == "total"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO = all_of(PO)
      )

    # Numerator

    num <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador)
  }
  if(tipo == "sexo"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO = all_of(PO),
        sexo = "sexo"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino"))
      )
  }
  if(tipo == "sexo e raca"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO = all_of(PO),
        sexo = "sexo",
        cor_raca = "cor_raca"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo,cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "cor_raca"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        cor_raca = factor(
          cor_raca,
          levels = c(1,2,3,4,5,99),
          labels = c("Branco","Preto","Indígena","Amarelo","Pardo","NR")
        )
      )
  }
  if(tipo == "sexo e escolaridade"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO = all_of(PO),
        sexo = "sexo",
        escolaridade = "escolaridade"
      )

    # Numerator

    num <- df |>
      dplyr::filter(PO == 1) |>
      summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, escolaridade)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "escolaridade"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
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

# Indicator 3 - Undergraduate level --------------------------------------

## Function

undergraduate_level <- function(
    df,
    peso = "peso",
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo # = c("total","sexo","sexo e raca")
){

  if(is.null(tipo)){
    stop("You need for describing wich type of tabulation you want before...")
  }

  if(tipo == "total"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        escolaridade
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador)
  }
  if(tipo == "sexo"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        escolaridade,
        sexo = "sexo"
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino"))
      )
  }
  if(tipo == "sexo e raca"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        escolaridade,
        sexo = "sexo",
        cor_raca = "cor_raca"
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo,cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "cor_raca"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        cor_raca = factor(
          cor_raca,
          levels = c(1,2,3,4,5,99),
          labels = c("Branco","Preto","Indígena","Amarelo","Pardo","NR")
        )
      )
  }

  # output
  return(table)
}

# Indicator 4 - High-skilled labor force --------------------------------------

## Function

high_labor_force <- function(
    df,
    peso = "peso",
    grupo_etario = "idade_quinquenal",
    coorte_nascimento = "coorte_quinquenal",
    tipo # = c("total","sexo","sexo e raca")
){

  if(is.null(tipo)){
    stop("You need for describing wich type of tabulation you want before...")
  }

  if(tipo == "total"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO,
        escolaridade
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4 & PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador)
  }
  if(tipo == "sexo"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO,
        escolaridade,
        sexo = "sexo"
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4 & PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino"))
      )
  }
  if(tipo == "sexo e raca"){

    # Dataframe
    df <- get(glue::glue("df")) |>
      dplyr::select(
        peso = all_of(peso),
        grupo_etario = all_of(grupo_etario),
        coorte_nascimento = all_of(coorte_nascimento),
        PO,
        escolaridade,
        sexo = "sexo",
        cor_raca = "cor_raca"
      )

    # Numerator

    num <- df |>
      dplyr::filter(escolaridade == 4 & PO == 1) |>
      dplyr::summarise(
        numerador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo,cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # Denominator

    den <- df |>
      dplyr::filter(PO == 1) |>
      dplyr::summarise(
        denominador = sum(peso),
        .by = c(coorte_nascimento, grupo_etario, sexo, cor_raca)
      ) |>
      dplyr::arrange(grupo_etario)

    # join

    table <- num |>
      dplyr::left_join(
        den,
        by = c("coorte_nascimento","grupo_etario", "sexo", "cor_raca"),
        keep = FALSE
      ) |>
      dplyr::mutate(across(everything(),~ round(.x,0))) |>
      dplyr::mutate(taxa = numerador/denominador) |>
      dplyr::mutate(
        sexo = factor(sexo, levels = c(2,1), labels = c("Feminino","Masculino")),
        cor_raca = factor(
          cor_raca,
          levels = c(1,2,3,4,5,99),
          labels = c("Branco","Preto","Indígena","Amarelo","Pardo","NR")
        )
      )
  }

  # output
  return(table)
}
