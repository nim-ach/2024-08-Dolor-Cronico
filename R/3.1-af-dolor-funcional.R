

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Valorar el efecto de la actividad física (controlando y sin controlar
#> por factores contextuales) sobre el dolor.

## Variables de análisis

#> - Actividad física: score_ipaq, mets_total, mets_vigorosa, mets_moderada, mets_caminar
#> - Sociodemográficos: edad, genero, universidad, nacionalidad,
#> area_estudios, consumo_tabaco, consumo_drogas
#> - Dolor: cuanto_afecta_dolor

# Preparamos espacio de trabajo -------------------------------------------

## Cargamos paquetes
library(data.table)
library(brms)

## Cargamos datos
data("chronic")

## Cargamos funciones auxiliares
source("R/_functions.R")

## Filtramos las variables de interés
vars = c("edad", "genero", "universidad",
         "area_estudios", "consumo_alcohol", "consume_tabaco",
         "score_ipaq", "mets_total", "mets_vigorosa", "mets_moderada", "mets_caminar",
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, mets_total := scale(mets_total)]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, cuanto_afecta_dolor]) |> plot()
  performance::check_distribution(tbl_data[, cuanto_afecta_dolor])
}

## Cuanto afecta el dolor funcionalmente

ms1_score_ipaq <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ score_ipaq,
  data = tbl_data,
  file = "models/af-dolor/ms1_score_ipaq.RDS"
)

ms1_mets_total <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ mets_total,
  data = tbl_data, file = "models/af-dolor/ms1_mets_total.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc1_score_ipaq <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ score_ipaq + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc1_score_ipaq.RDS"
)

mc1_mets_total <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ mets_total + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc1_mets_total.RDS"
)

