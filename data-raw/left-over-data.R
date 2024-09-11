library(data.table)

chronic <- fread(file = "data-raw/chronic.csv")

chronic |> names()

used_vars <- c("dolor_frecuente_3meses", "cuanto_afecta_dolor", "escala_dolor", "frecuencia_dolor",
               "score_ipaq", "mets_total", "mets_vigorosa", "mets_moderada",
               "mets_caminar", "pittsburg_horasdormidas", "clasificacion_global",
               "matutinidad_score", "score_matutinidad")

not_used_vars <- names(chronic)[!names(chronic) %in% used_vars]

chronic2 <- chronic[, .SD, .SDcols = not_used_vars]

fwrite(chronic2, file = "data-raw/chronic-not-used.csv")
save(chronic2, file = "data/chronic2.RData")
