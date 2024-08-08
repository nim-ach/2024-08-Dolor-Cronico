

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
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[, .SD, .SDcols = vars]

## Transformamos la variable duración de dolor en una variable dicotomica
tbl_data[, dolor_frecuente_3meses := fifelse(dolor_frecuente_3meses == "≥ 3 meses", 1, 0)]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, dolor_frecuente_3meses]) |> plot()
}

## Cuanto afecta el dolor funcionalmente

ms4_edad <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ edad,
  data = tbl_data, file = "models/factores-y-dolor/ms4_edad.RDS"
)

ms4_genero <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ genero,
  data = tbl_data, file = "models/factores-y-dolor/ms4_genero.RDS"
)

ms4_universidad <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ universidad,
  data = tbl_data, file = "models/factores-y-dolor/ms4_universidad.RDS"
)

ms4_area_estudios <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ area_estudios,
  data = tbl_data, file = "models/factores-y-dolor/ms4_area_estudios.RDS"
)

ms4_consumo_alcohol <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ consumo_alcohol,
  data = tbl_data, file = "models/factores-y-dolor/ms4_consumo_alcohol.RDS"
)

ms4_consume_tabaco <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/ms4_consume_tabaco.RDS"
)


# Análisis con múltiples variables ----------------------------------------

mc4_factores <- bayesian_bernoulli(
  dolor_frecuente_3meses ~ edad + genero + universidad + area_estudios +
    consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/mc4_factores.RDS"
)
