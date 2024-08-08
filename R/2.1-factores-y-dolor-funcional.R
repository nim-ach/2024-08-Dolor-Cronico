

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Valorar el efecto del sexo, área de estudio del pregrado, factores étnicos
#> y geográficos sobre el dolor.

## Variables de análisis

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
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor")
tbl_data <- chronic[dolor_frecuente_3meses == "≥ 3 meses", .SD, .SDcols = vars]
tbl_data[, edad := scale(edad)]

# Análisis bivariados ----------------------------------------------------

## Cuanto afecta el dolor ----

if (interactive()) {
  table(tbl_data[, cuanto_afecta_dolor]) |> plot()
  performance::check_distribution(tbl_data[, cuanto_afecta_dolor])
}

## Cuanto afecta el dolor funcionalmente

ms1_edad <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ edad,
  data = tbl_data, file = "models/factores-y-dolor/ms1_edad.RDS"
)

ms1_genero <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ genero,
  data = tbl_data, file = "models/factores-y-dolor/ms1_genero.RDS"
)

ms1_universidad <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ universidad,
  data = tbl_data, file = "models/factores-y-dolor/ms1_universidad.RDS"
)

ms1_area_estudios <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ area_estudios,
  data = tbl_data, file = "models/factores-y-dolor/ms1_area_estudios.RDS"
)

ms1_consumo_alcohol <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ consumo_alcohol,
  data = tbl_data, file = "models/factores-y-dolor/ms1_consumo_alcohol.RDS"
)

ms1_consume_tabaco <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/ms1_consume_tabaco.RDS"
)


# Análisis con múltiples variables ----------------------------------------

mc1_factores <- bayesian_beta_binomial(
  cuanto_afecta_dolor | trials(10) ~ edad + genero + universidad + area_estudios +
    consumo_alcohol + consume_tabaco,
  data = tbl_data, file = "models/factores-y-dolor/mc1_factores.RDS"
)
