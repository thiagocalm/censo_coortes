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
anos = 1960
# i = 1
#
# ddi <- paste0("ipumsi_census_", anos[i],".xml")
#
# # Reading parse file ipums
#
# censo_ddi <- read_ipums_ddi(
#   file.path("./inputs/raw", ddi),
#   lower_vars = TRUE
# )
#
# # Renaming ipums .dat file
#
# censo_ddi$file_name <- paste0("ipumsi_census_", anos[i],".xml")

# setting DDI vector for each year

ddi <- paste0("ipumsi_census_",anos,".xml")

# Reading parse file ipums

censo_ddi <- read_ipums_ddi(
  file.path("./inputs/raw", ddi),
  lower_vars = TRUE
)

# Renaming ipums .dat file

censo_ddi$file_name <- paste0("ipumsi_census_",anos,".dat")

# Reading microdata file ipums

censo <- ipumsr::read_ipums_micro_chunked(
  ddi =   censo_ddi,
  callback = IpumsDataFrameCallback$new(
    function(x, pos) {
      return(
        x %>%
          filter(!is.na(age)) |>
          filter(age != 999) |>
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
            "cor_raca" = race,
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

censo <- censo |>
  mutate(
    PEA = case_when(cond_forca_trabalho == 2 ~ 1, TRUE ~ 0),
    PO = case_when(cond_atividade == 1 ~ 1, TRUE ~ 0),
    PIA = case_when(cond_forca_trabalho == 1 ~ 1, TRUE ~ 0)
  ) |>
  select(-c(cond_atividade, cond_forca_trabalho))

# Mutating race variable

censo <- censo |>
  mutate(
    cor_raca = case_when(
      cor_raca == 10 ~ 1,
      cor_raca == 20 ~ 2,
      cor_raca == 30 ~ 3,
      cor_raca == 40 ~ 4,
      cor_raca == 51 ~ 5,
      cor_raca == 99 ~ 99
    )
  )

# Creating variables based on age

censo <- censo |>
  mutate(
    idade = case_when(idade >= 75 ~ 75, TRUE ~ idade),
    ano_nascimento = ano - idade
  ) |>
  mutate(
    idade_quinquenal = case_when(
      idade <= 19 ~ 15,
      idade <= 24 ~ 20,
      idade <= 29 ~ 25,
      idade <= 34 ~ 30,
      idade <= 39 ~ 35,
      idade <= 44 ~ 40,
      idade <= 49 ~ 45,
      idade <= 54 ~ 50,
      idade <= 59 ~ 55,
      idade <= 64 ~ 60,
      idade <= 69 ~ 65,
      idade <= 74 ~ 70,
      idade > 74  ~ 75
    ),
    idade_decenal = case_when(
      idade <= 24 ~ 15,
      idade <= 34 ~ 25,
      idade <= 44 ~ 35,
      idade <= 54 ~ 45,
      idade <= 64 ~ 55,
      idade <= 74 ~ 65,
      idade > 74  ~ 75
    ),
    coorte_quinquenal = case_when(
      ano_nascimento %in% 2001:2005 ~ 2001,
      ano_nascimento %in% 1996:2000 ~ 1996,
      ano_nascimento %in% 1991:1995 ~ 1991,
      ano_nascimento %in% 1986:1990 ~ 1986,
      ano_nascimento %in% 1981:1985 ~ 1981,
      ano_nascimento %in% 1976:1980 ~ 1976,
      ano_nascimento %in% 1971:1975 ~ 1971,
      ano_nascimento %in% 1966:1970 ~ 1966,
      ano_nascimento %in% 1961:1965 ~ 1961,
      ano_nascimento %in% 1956:1960 ~ 1956,
      ano_nascimento %in% 1951:1955 ~ 1951,
      ano_nascimento %in% 1946:1950 ~ 1946,
      ano_nascimento %in% 1941:1945 ~ 1941,
      ano_nascimento %in% 1936:1940 ~ 1936,
      ano_nascimento %in% 1931:1935 ~ 1931,
      ano_nascimento %in% 1926:1930 ~ 1926,
      ano_nascimento %in% 1921:1925 ~ 1921,
      ano_nascimento %in% 1916:1920 ~ 1916,
      ano_nascimento %in% 1911:1915 ~ 1911,
      ano_nascimento %in% 1906:1910 ~ 1906,
      ano_nascimento %in% 1901:1905 ~ 1901,
      ano_nascimento %in% 1896:1900 ~ 1896,
      ano_nascimento %in% 1891:1895 ~ 1891,
      ano_nascimento %in% 1886:1890 ~ 1886,
      ano_nascimento < 1896 ~ 1895
    ),
    coorte_decenal = case_when(
      ano_nascimento %in% 1996:2005 ~ 1996,
      ano_nascimento %in% 1986:1995 ~ 1986,
      ano_nascimento %in% 1976:1985 ~ 1976,
      ano_nascimento %in% 1966:1975 ~ 1966,
      ano_nascimento %in% 1956:1965 ~ 1956,
      ano_nascimento %in% 1946:1955 ~ 1946,
      ano_nascimento %in% 1936:1945 ~ 1936,
      ano_nascimento %in% 1926:1935 ~ 1926,
      ano_nascimento %in% 1916:1925 ~ 1916,
      ano_nascimento %in% 1906:1915 ~ 1906,
      ano_nascimento %in% 1896:1905 ~ 1896,
      ano_nascimento %in% 1886:1895 ~ 1886,
      ano_nascimento < 1896 ~ 1895
    )
  )


# Looping -----------------------------------------------------------------

anos <- c(1960,1970,1980, 1991, 2000, 2010)

for(i in seq_along(anos)){
  ano = anos[i]
  print(paste0("Let's start the year ", anos[i], "..."))

  # setting DDI vector for each year

  ddi <- paste0("ipumsi_census_", ano,".xml")

  # Reading parse file ipums

  censo_ddi <- read_ipums_ddi(
    file.path("./inputs/raw", ddi),
    lower_vars = TRUE
  )

  # Renaming ipums .dat file

  censo_ddi$file_name <- paste0("ipumsi_census_", ano,".dat")

  if(anos[i] == 1970){
    # Reading microdata file ipums

    censo <- ipumsr::read_ipums_micro_chunked(
      ddi =   censo_ddi,
      callback = IpumsDataFrameCallback$new(
        function(x, pos) {
          return(
            x %>%
              filter(!is.na(age)) |>
              filter(age != 999) |>
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
                "escolaridade" = edattain,
                "cond_atividade" = empstat,
                "cond_forca_trabalho" = labforce,
                "posicao_ocupacao" = classwk
              ) |>
              select(-any_of(c("country", 'sample', "serial", "hhwt", "pernum", "filhos_vivos",
                               "resident", "edattaind", "empstatd", "classwkd","filhos_sobreviventes")))
          )}
      ),
      chunk_size = 10000,
      verbose = TRUE
    )

    print(paste0("Finished the importation for the ", anos[i], "'s..."))

    # Race variable

    censo <- censo |>
      mutate(
        cor_raca = 0
      )

  } else{
    # Reading microdata file ipums

    censo <- ipumsr::read_ipums_micro_chunked(
      ddi =   censo_ddi,
      callback = IpumsDataFrameCallback$new(
        function(x, pos) {
          return(
            x %>%
              filter(!is.na(age)) |>
              filter(age != 999) |>
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
                "cor_raca" = race,
                "escolaridade" = edattain,
                "cond_atividade" = empstat,
                "cond_forca_trabalho" = labforce,
                "posicao_ocupacao" = classwk
              ) |>
              select(-any_of(c("country", 'sample', "serial", "hhwt", "pernum", "filhos_vivos",
                               "resident", "edattaind", "empstatd", "classwkd","filhos_sobreviventes")))
          )}
      ),
      chunk_size = 10000,
      verbose = TRUE
    )

    # Race variable

    censo <- censo |>
      mutate(
        cor_raca = case_when(
          cor_raca == 10 ~ 1,
          cor_raca == 20 ~ 2,
          cor_raca == 30 ~ 3,
          cor_raca == 40 ~ 4,
          cor_raca == 51 ~ 5,
          cor_raca == 99 ~ 99
        )
      )
  }

  invisible(gc())

  # Validating some derived variables that will be created

  censo <- censo |>
    mutate(
      PEA = case_when(cond_forca_trabalho == 2 ~ 1, TRUE ~ 0),
      PO = case_when(cond_atividade == 1 ~ 1, TRUE ~ 0),
      PIA = case_when(cond_forca_trabalho == 1 ~ 1, TRUE ~ 0)
    ) |>
    select(-c(cond_atividade, cond_forca_trabalho))

  # Creating variables based on age

  censo <- censo |>
    mutate(
      idade = case_when(idade >= 75 ~ 75, TRUE ~ idade),
      ano_nascimento = ano - idade
    ) |>
    mutate(
      idade_quinquenal = case_when(
        idade <= 19 ~ 15,
        idade <= 24 ~ 20,
        idade <= 29 ~ 25,
        idade <= 34 ~ 30,
        idade <= 39 ~ 35,
        idade <= 44 ~ 40,
        idade <= 49 ~ 45,
        idade <= 54 ~ 50,
        idade <= 59 ~ 55,
        idade <= 64 ~ 60,
        idade <= 69 ~ 65,
        idade <= 74 ~ 70,
        idade > 74  ~ 75
      ),
      idade_decenal = case_when(
        idade <= 24 ~ 15,
        idade <= 34 ~ 25,
        idade <= 44 ~ 35,
        idade <= 54 ~ 45,
        idade <= 64 ~ 55,
        idade <= 74 ~ 65,
        idade > 74  ~ 75
      ),
      coorte_quinquenal = case_when(
        ano_nascimento %in% 2001:2005 ~ 2001,
        ano_nascimento %in% 1996:2000 ~ 1996,
        ano_nascimento %in% 1991:1995 ~ 1991,
        ano_nascimento %in% 1986:1990 ~ 1986,
        ano_nascimento %in% 1981:1985 ~ 1981,
        ano_nascimento %in% 1976:1980 ~ 1976,
        ano_nascimento %in% 1971:1975 ~ 1971,
        ano_nascimento %in% 1966:1970 ~ 1966,
        ano_nascimento %in% 1961:1965 ~ 1961,
        ano_nascimento %in% 1956:1960 ~ 1956,
        ano_nascimento %in% 1951:1955 ~ 1951,
        ano_nascimento %in% 1946:1950 ~ 1946,
        ano_nascimento %in% 1941:1945 ~ 1941,
        ano_nascimento %in% 1936:1940 ~ 1936,
        ano_nascimento %in% 1931:1935 ~ 1931,
        ano_nascimento %in% 1926:1930 ~ 1926,
        ano_nascimento %in% 1921:1925 ~ 1921,
        ano_nascimento %in% 1916:1920 ~ 1916,
        ano_nascimento %in% 1911:1915 ~ 1911,
        ano_nascimento %in% 1906:1910 ~ 1906,
        ano_nascimento %in% 1901:1905 ~ 1901,
        ano_nascimento %in% 1896:1900 ~ 1896,
        ano_nascimento %in% 1891:1895 ~ 1891,
        ano_nascimento %in% 1886:1890 ~ 1886,
        ano_nascimento < 1896 ~ 1895
      ),
      coorte_decenal = case_when(
        ano_nascimento %in% 1996:2005 ~ 1996,
        ano_nascimento %in% 1986:1995 ~ 1986,
        ano_nascimento %in% 1976:1985 ~ 1976,
        ano_nascimento %in% 1966:1975 ~ 1966,
        ano_nascimento %in% 1956:1965 ~ 1956,
        ano_nascimento %in% 1946:1955 ~ 1946,
        ano_nascimento %in% 1936:1945 ~ 1936,
        ano_nascimento %in% 1926:1935 ~ 1926,
        ano_nascimento %in% 1916:1925 ~ 1916,
        ano_nascimento %in% 1906:1915 ~ 1906,
        ano_nascimento %in% 1896:1905 ~ 1896,
        ano_nascimento %in% 1886:1895 ~ 1886,
        ano_nascimento < 1896 ~ 1895
      )
    )

  print(paste0("Finished the creating of variables for the ", anos[i], "'s..."))

  # exporting data

  arrow::write_parquet(
    censo,
    sink = file.path("./inputs", glue::glue("censo_{anos[i]}.parquet")),
  )

  # next loop...

  print(paste0("Loop's finished for ", anos[i], "!!!"))
  rm(censo, censo_ddi)
  invisible(gc())
}
