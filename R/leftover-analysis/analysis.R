
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(ggplot2)
library(rstatix)
library(ggpubr)

theme_set(new = theme_light())

## Load dataset
load(file = here::here("data/chronic2.RData"))

## Cambiar variables a factor
chronic2[, genero := factor(genero, levels = c("Femenino","Masculino"))]
chronic2[, universidad := factor(universidad, levels = c("Universidad de Magallanes", "Universidad de Aysén"))]
chronic2[, consumo_alcohol := factor(consumo_alcohol, levels = c("Sí", "No"))]

## Load auxiliary functions
# ----------------------- #

# Analysis ----------------------------------------------------------------

## Research questions

#> 1.1 En la universidad de Aysén las mujeres presentan números más altos de
#>     ansiedad que en la Universidad de Magallanes.

data_p1 <- chronic2 |>
  na.omit(cols = c("universidad", "puntaje", "genero"))

stat_p1 <- data_p1 |>
  group_by(universidad) |>
  wilcox_test(puntaje ~ genero, paired = FALSE, exact = FALSE) |>
  transform(p_label = fifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3))))

fig_p1 <- ggplot(data_p1, aes(genero, puntaje)) +
  facet_wrap(~ universidad) +
  geom_violin(trim = FALSE, fill = "#099") +
  geom_boxplot(width = 0.1, col = "white", fill = "#099") +
  labs(y = "Puntaje de Ansiedad de Beck", x = "Genero") +
  ggpubr::stat_pvalue_manual(stat_p1, label = "p_label", y.position = 90, vjust = -.5) +
  scale_y_continuous(expand = c(0,10))

#> 1.2 En la universidad de Aysén las mujeres presentan números más altos de
#>     depresión que en la Universidad de Magallanes.

data_p2 <- chronic2 |>
  na.omit(cols = c("universidad", "score_ans_beck", "genero"))

stat_p2 <- data_p2 |>
  group_by(universidad) |>
  wilcox_test(score_ans_beck ~ genero, paired = FALSE, exact = FALSE) |>
  transform(p_label = fifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3))))

fig_p2 <- ggplot(data_p2, aes(genero, score_ans_beck)) +
  facet_wrap(~ universidad) +
  geom_violin(trim = FALSE, fill = "#099") +
  geom_boxplot(width = 0.1, col = "white", fill = "#099") +
  labs(y = "Puntaje de Depresión de Beck", x = "Genero") +
  ggpubr::stat_pvalue_manual(stat_p2, label = "p_label", y.position = 75, tip.length = c(.05, .2), vjust = -.5) +
  scale_y_continuous(expand = c(0,10))

#> 2. Y al mismo tiempo nos dimos cuenta que en la Universidad de Magallanes
#>    son muchas más las mujeres que consumen alcohol.

data_p3 <- chronic2 |>
  na.omit(cols = c("universidad", "consumo_alcohol", "genero"))

stat_p3 <- vector(mode = "list", length = 2)
stat_p3[[1]] <- rbind(
  cbind(universidad = "Universidad de Magallanes", data_p3[universidad == "Universidad de Magallanes", table(consumo_alcohol, genero)] |> chisq_test(correct = F)),
  cbind(universidad = "Universidad de Aysén", data_p3[universidad == "Universidad de Aysén", table(consumo_alcohol, genero)] |> chisq_test(correct = F))
) |>
  transform(p_label = fifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3))))

stat_p3[[2]] <- data_p3[, table(genero) |> pairwise_chisq_gof_test(p.adjust.method = "none"), list(consumo_alcohol, universidad)]

fig_p3 <- ggstatsplot::grouped_ggbarstats(
  data = data_p3[, list(Género = genero,
                            `Consumo de alcohol` = consumo_alcohol,
                            Universidad = universidad)],
  x = Género,
  y = `Consumo de alcohol`,
  label = "both",
  grouping.var = Universidad,
  bf.message = FALSE,
  results.subtitle = FALSE,
  ggplot.component = list(
    scale_fill_brewer(type = "qual", palette = 3)
  )
)

#> 3. Mujeres mas depresivas y ansiosas, pero hombres con más estres


data_p4 <- chronic2 |>
  na.omit(cols = c("puntaje", "score_ans_beck", "puntaje_estres_academico", "genero"))

data_p4 <- melt(data_p4,
     measure.vars = c("puntaje", "score_ans_beck", "puntaje_estres_academico"),
     id.vars = c("record_id", "genero"))

data_p4[, variable := `levels<-`(variable, c("BAI", "BDI", "EA"))]

stat_p4 <- data_p4 |>
  group_by(variable) |>
  wilcox_test(value ~ genero, paired = FALSE, exact = FALSE) |>
  transform(p_label = fifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3))))

fig_p4 <- ggplot(data_p4, aes(genero, value)) +
  facet_wrap(~ variable) +
  geom_violin(trim = FALSE, fill = "#099") +
  geom_boxplot(width = 0.1, col = "white", fill = "#099") +
  labs(y = "Puntaje de de la escala", x = "Genero") +
  ggpubr::stat_pvalue_manual(stat_p4, label = "p_label", y.position = c(90, 80, 120), vjust = -.5,
                             step.group.by = "variable") +
  scale_y_continuous(expand = c(0,10))
