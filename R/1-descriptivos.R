

# Objetivos de trabajo ----------------------------------------------------

## Objetivos de análisis

#> Generar estadísticos descriptivos de resultados de encuestas aplicadas
#> y datos sociodemográficos.

## Variables de análisis

#> - Sociodemográficos: edad, genero, universidad, nacionalidad,
#> area_estudios, consumo_tabaco, consumo_drogas
#> - Dolor: cuanto_afecta_dolor, escala_dolor, frecuencia_dolor
#> - Actividad física: score_ipaq, mets_total, mets_vigorosa, mets_moderada,
#> mets_caminando
#> - Calidad de sueño: pittsburg_horasdormidas, clasificacion_global
#> - Cronotipo: matutinidad_score (numerico), score_matutinidad (categorico)

# Preparamos espacio de trabajo -------------------------------------------

## Cargamos paquetes
library(data.table)
library(gtsummary)
library(gt)

## Cargamos datos
data("chronic")

## Filtramos las variables de interés
vars = c("edad", "genero", "universidad", "nacionalidad",
         "area_estudios", "consumo_alcohol", "consume_tabaco",
         "dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor",
         "score_ipaq", "mets_total", "mets_vigorosa", "mets_moderada",
         "mets_caminar", "pittsburg_horasdormidas", "clasificacion_global",
         "matutinidad_score", "score_matutinidad")
tbl_data <- chronic[, .SD, .SDcols = vars]

## Etiquetas de variables
var_labels <- list(
  "edad" ~ "Edad", "genero" ~ "Género", "universidad" ~ "Universidad", "nacionalidad" ~ "Nacionalidad",
  "area_estudios" ~ "Área de Estudios", "consumo_alcohol" ~ "Consumo de Alcohol",
  "consume_tabaco" ~ "Hábito tabáquico", "dolor_frecuente_3meses" ~ "Dolor Frecuente >3 meses", "cuanto_afecta_dolor" ~ "Afectación funcional del Dolor",
  "escala_dolor" ~ "Escala Numérica de Dolor (0-10)", "frecuencia_dolor" ~ "Frecuencia del Dolor",
  "score_ipaq" ~ "Nivel de Actividad Física", "mets_total" ~ "MET Total", "mets_vigorosa" ~ "MET AF Vigorosa",
  "mets_moderada" ~ "MET AF Moderada", "mets_caminar" ~ "MET AF Caminata",
  "pittsburg_horasdormidas" ~ "Horas de sueño", "clasificacion_global" ~ "PSQI Total",
  "matutinidad_score" ~ "Matutinidad total", "score_matutinidad" ~ "Categoría de Matutinidad"
)

## Definiendo idioma de tabla
theme_gtsummary_language("es")

## Descriptivos univariados
tbl_1 <-
  tbl_summary(tbl_data, label = var_labels, missing = "no") |>
  bold_labels()

## Descriptivos bivariados por sexo
tbl_2 <-
  tbl_summary(tbl_data[genero %in% c("Masculino", "Femenino")] |> droplevels(),
            by = genero, label = var_labels[-2], missing = "no") |>
  bold_labels() |>
  add_difference(test = all_continuous() ~ "smd", include = all_continuous())

## Descriptivos bivariados por duración del dolor
tbl_3 <-
  tbl_summary(
  data = within(tbl_data, {
    dolor_frecuente_3meses = `levels<-`(dolor_frecuente_3meses, c("≥ 3 meses", "< 3 meses"))
  }),
  by = dolor_frecuente_3meses, label = var_labels[-8], missing = "no") |>
  bold_labels() |>
  add_difference(test = all_continuous() ~ "smd", include = all_continuous())

## Tabla con descriptivos univariados y bivariados por genero y duración del dolor
tbl_final <- tbl_merge(tbls = list(tbl_1,tbl_2,tbl_3),
                     tab_spanner = c("**Global**",
                                     "**Genero**",
                                     "**Duración Dolor**"))

print(tbl_final)

as_gt(tbl_final) |>
  gt::gtsave(filename = "manuscript/tables/table-1.html")
