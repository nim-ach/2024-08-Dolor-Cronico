

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
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]

## Transformamos la variable frecuencia de dolor en una variable ordinal
tbl_data[, frecuencia_dolor := as.ordered(frecuencia_dolor)]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, frecuencia_dolor]) |> plot()
}


## Cuanto afecta el dolor funcionalmente

ms3_edad <- bayesian_cumulative(
  frecuencia_dolor ~ edad,
  data = tbl_data, file = "models/factores-y-dolor/ms3_edad.RDS"
)

ms3_genero <- bayesian_cumulative(
  frecuencia_dolor ~ genero,
  data = tbl_data, file = "models/factores-y-dolor/ms3_genero.RDS"
)

ms3_universidad <- bayesian_cumulative(
  frecuencia_dolor ~ universidad,
  data = tbl_data, file = "models/factores-y-dolor/ms3_universidad.RDS"
)

ms3_area_estudios <- bayesian_cumulative(
  frecuencia_dolor ~ area_estudios,
  data = tbl_data, file = "models/factores-y-dolor/ms3_area_estudios.RDS"
)

ms3_consumo_alcohol <- bayesian_cumulative(
  frecuencia_dolor ~ consumo_alcohol,
  data = tbl_data, file = "models/factores-y-dolor/ms3_consumo_alcohol.RDS"
)

ms3_consume_tabaco <- bayesian_cumulative(
  frecuencia_dolor ~ consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/ms3_consume_tabaco.RDS"
)


# Análisis con múltiples variables ----------------------------------------

mc3_factores <- bayesian_cumulative(
  frecuencia_dolor ~ edad + genero + universidad + area_estudios +
    consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/mc3_factores.RDS"
)
