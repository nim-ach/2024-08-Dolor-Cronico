

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Valorar el efecto de la actividad física (controlando y sin controlar
#> por factores contextuales) sobre el dolor.

## Variables de análisis

#> - Actividad física: pittsburg_horasdormidas, clasificacion_global
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
         "pittsburg_horasdormidas", "clasificacion_global",
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, mets_total := scale(pittsburg_horasdormidas)]
tbl_data[, mets_total := scale(clasificacion_global)]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, cuanto_afecta_dolor]) |> plot()
  performance::check_distribution(tbl_data[, cuanto_afecta_dolor])
}

## Cuanto afecta el dolor funcionalmente

ms1_pittsburg_horasdormidas <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ pittsburg_horasdormidas,
  data = tbl_data,
  file = "models/psqi-dolor/ms1_pittsburg_horasdormidas.RDS"
)

ms1_clasificacion_global <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ clasificacion_global,
  data = tbl_data, file = "models/psqi-dolor/ms1_clasificacion_global.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc1_pittsburg_horasdormidas <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ pittsburg_horasdormidas + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc1_pittsburg_horasdormidas.RDS"
)

mc1_clasificacion_global <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ clasificacion_global + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/psqi-dolor/mc1_clasificacion_global.RDS"
)

