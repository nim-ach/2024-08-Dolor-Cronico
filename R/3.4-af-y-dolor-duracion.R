

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Valorar el efecto del sexo, área de estudio del pregrado, factores étnicos
#> y geográficos sobre el dolor.

## Variables de análisis

#> - Sociodemográficos: edad, genero, universidad, nacionalidad,
#> area_estudios, consumo_tabaco, consumo_drogas
#> - Dolor: frecuencia_dolor

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
tbl_data <- chronic[, .SD, .SDcols = vars]
tbl_data[, mets_total := scale(mets_total)]
tbl_data[, edad := scale(edad)]

## Transformamos la variable duración de dolor en una variable dicotomica
tbl_data[, dolor_frecuente_3meses := fifelse(dolor_frecuente_3meses == "≥ 3 meses", 1, 0)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, dolor_frecuente_3meses]) |> plot()
}

## Cuanto afecta el dolor funcionalmente

ms4_score_ipaq <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ score_ipaq,
  data = tbl_data, file = "models/af-dolor/ms4_score_ipaq.RDS"
)

ms4_mets_total <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ mets_total,
  data = tbl_data, file = "models/af-dolor/ms4_mets_total.RDS"
)


# Análisis con múltiples variables ----------------------------------------

mc4_score_ipaq <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ score_ipaq + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc4_score_ipaq.RDS"
)

mc4_mets_total <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ mets_total + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc4_mets_total.RDS"
)


