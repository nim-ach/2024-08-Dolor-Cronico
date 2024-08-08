

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
         "pittsburg_horasdormidas", "clasificacion_global",
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, mets_total := scale(pittsburg_horasdormidas)]
tbl_data[, mets_total := scale(clasificacion_global)]
tbl_data[, edad := scale(edad)]

## Transformamos la variable frecuencia de dolor en una variable ordinal
tbl_data[, frecuencia_dolor := as.ordered(frecuencia_dolor)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, frecuencia_dolor]) |> plot()
}

## Cuanto afecta el dolor funcionalmente

ms3_pittsburg_horasdormidas <- bayesian_cumulative(
  frecuencia_dolor ~ pittsburg_horasdormidas,
  data = tbl_data, file = "models/psqi-dolor/ms3_pittsburg_horasdormidas.RDS"
)

ms3_clasificacion_global <- bayesian_cumulative(
  frecuencia_dolor ~ clasificacion_global,
  data = tbl_data, file = "models/psqi-dolor/ms3_clasificacion_global.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc3_pittsburg_horasdormidas <- bayesian_cumulative(
  frecuencia_dolor ~ pittsburg_horasdormidas + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc3_pittsburg_horasdormidas.RDS"
)

mc3_clasificacion_global <- bayesian_cumulative(
  frecuencia_dolor ~ clasificacion_global + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc3_clasificacion_global.RDS"
)
