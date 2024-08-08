

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
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, mets_total := scale(mets_total)]
tbl_data[, edad := scale(edad)]

## Transformamos la variable frecuencia de dolor en una variable ordinal
tbl_data[, frecuencia_dolor := as.ordered(frecuencia_dolor)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, frecuencia_dolor]) |> plot()
}

## Cuanto afecta el dolor funcionalmente

ms3_score_ipaq <- bayesian_cumulative(
  frecuencia_dolor ~ score_ipaq,
  data = tbl_data, file = "models/af-dolor/ms3_score_ipaq.RDS"
)

ms3_mets_total <- bayesian_cumulative(
  frecuencia_dolor ~ mets_total,
  data = tbl_data, file = "models/af-dolor/ms3_mets_total.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc3_score_ipaq <- bayesian_cumulative(
  frecuencia_dolor ~ score_ipaq + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc3_score_ipaq.RDS"
)

mc3_mets_total <- bayesian_cumulative(
  frecuencia_dolor ~ mets_total + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/af-dolor/mc3_mets_total.RDS"
)
