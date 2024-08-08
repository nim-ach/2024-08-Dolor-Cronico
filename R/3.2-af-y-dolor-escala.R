

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Valorar el efecto del sexo, área de estudio del pregrado, factores étnicos
#> y geográficos sobre el dolor.

## Variables de análisis

#> - Sociodemográficos: edad, genero, universidad, nacionalidad,
#> area_estudios, consumo_tabaco, consumo_drogas
#> - Dolor: escala_dolor

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
  table(tbl_data[, escala_dolor]) |> plot()
  performance::check_distribution(tbl_data[, escala_dolor])
}


## Cuanto afecta el dolor funcionalmente

ms2_score_ipaq <- bayesian_beta_binomial(
  escala_dolor | trials(10) ~ score_ipaq,
  data = tbl_data, file = "models/af-dolor/ms2_score_ipaq.RDS"
)

ms2_mets_total <- bayesian_beta_binomial(
  escala_dolor | trials(10) ~ mets_total,
  data = tbl_data, file = "models/af-dolor/ms2_mets_total.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc2_score_ipaq <- bayesian_beta_binomial(
  escala_dolor | trials(10) ~ score_ipaq + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc2_score_ipaq.RDS"
)

mc2_mets_total <- bayesian_beta_binomial(
  escala_dolor | trials(10) ~  mets_total + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc2_mets_total.RDS"
)

