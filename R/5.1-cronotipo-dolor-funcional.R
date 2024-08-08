

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
         "matutinidad_score", "score_matutinidad",
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, mets_total := scale(matutinidad_score)]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, cuanto_afecta_dolor]) |> plot()
  performance::check_distribution(tbl_data[, cuanto_afecta_dolor])
}

## Cuanto afecta el dolor funcionalmente

ms1_matutinidad_score <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ matutinidad_score,
  data = tbl_data,
  file = "models/cronotipo-dolor/ms1_matutinidad_score.RDS"
)

ms1_score_matutinidad <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ score_matutinidad,
  data = tbl_data, file = "models/cronotipo-dolor/ms1_score_matutinidad.RDS"
)

# Análisis con múltiples variables ----------------------------------------

mc1_matutinidad_score <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ matutinidad_score + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/cronotipo-dolor/mc1_matutinidad_score.RDS"
)

mc1_score_matutinidad <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ score_matutinidad + edad + genero +
    universidad + area_estudios + consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/cronotipo-dolor/mc1_score_matutinidad.RDS"
)

