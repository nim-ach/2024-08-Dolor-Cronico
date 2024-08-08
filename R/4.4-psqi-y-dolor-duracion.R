

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
tbl_data <- chronic[, .SD, .SDcols = vars]
tbl_data[, mets_total := scale(pittsburg_horasdormidas)]
tbl_data[, mets_total := scale(clasificacion_global)]
tbl_data[, edad := scale(edad)]

## Transformamos la variable duración de dolor en una variable dicotomica
tbl_data[, dolor_frecuente_3meses := fifelse(dolor_frecuente_3meses == "≥ 3 meses", 1, 0)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, dolor_frecuente_3meses]) |> plot()
}

## Cuanto afecta el dolor funcionalmente

ms4_pittsburg_horasdormidas <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ pittsburg_horasdormidas,
  data = tbl_data, file = "models/psqi-dolor/ms4_pittsburg_horasdormidas"
)

ms4_clasificacion_global <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ clasificacion_global,
  data = tbl_data, file = "models/psqi-dolor/ms4_clasificacion_global"
)


# Análisis con múltiples variables ----------------------------------------

mc4_pittsburg_horasdormidas <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ pittsburg_horasdormidas + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc4_pittsburg_horasdormidas"
)

mc4_clasificacion_global <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ clasificacion_global + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc4_clasificacion_global"
)


