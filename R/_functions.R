
## Hyperparameters
.iter <- 4500
.warmup <- 2000
.chains <- 4
.cores <- 4
.seed <- 1234
.control <- list(adapt_delta = .99, max_treedepth = 30)


## Bayesian GLM negative-binomial
bayesian_beta_binomial <- function(formula, data, file, ...) {
  .prior <- prior(normal(0, 3), class = b)

  brm(
    formula,
    family = beta_binomial(link = "logit"),
    data = data,
    prior = .prior, chains = .chains, cores = .cores,
    iter = .iter, warmup = .warmup, seed = .seed,
    control = .control, file = file
  )
}

## Bayesian GLM cumulative function
bayesian_cumulative <- function(formula, data, file, ...) {
  .prior <- prior(normal(0, 3), class = b)

  brm(
    formula,
    family = cumulative(link = "logit"),
    data = data,
    prior = .prior, chains = .chains, cores = .cores,
    iter = .iter, warmup = .warmup, seed = .seed,
    control = .control, file = file
  )
}

## Bayesian GLM cumulative function
bayesian_bernoulli <- function(formula, data, file, ...) {
  .prior <- prior(normal(0, 3), class = b)

  brm(
    formula,
    family = bernoulli(link = "logit"),
    data = data,
    prior = .prior, chains = .chains, cores = .cores,
    iter = .iter, warmup = .warmup, seed = .seed,
    control = .control, file = file
  )
}

mc1_pittsburg_horasdormidas |> (
  function(m, var, compare = TRUE, summary = TRUE, .f = NULL) {
    if (is.null(.f)) {
      .f <- function(i) return(i)
    }

    var_char <- deparse(substitute(var))
    if (ncol(m$data) <= 2) {
      newdat <- subset(m$data, select = var_char) |>
        data.table::as.data.table() |>
        unique()
    } else {
      newdat <- m$data[,-1L] |>
        data.table::as.data.table() |>
        lapply(unique) |>
        do.call(what = expand.grid)
    }

    ind <- grep(var_char, names(newdat), ignore.case = TRUE)
    if (length(ind) == 0) {
      stop("There is no \"var\" variable in the data of the model provided")
    }

    if (ncol(newdat) > 1) {
      newdat[,-ind] <- NA
      newdat <- unique(newdat)
    }

    out <- predict(m, newdat, summary = FALSE)
    out <- reshape2::melt(out) |>
      data.table::as.data.table()
    names(out) <- c(".draw", ".row", "pred")

    newdat$.row <- seq_len(nrow(newdat))
    out <- data.table::merge.data.table(out, newdat, by = ".row")

    if (ncol(newdat) > 2) {
      ind <- grep(var_char, names(m$data[,-1L]), ignore.case = TRUE, value = TRUE, invert = TRUE)
      out[, ind] <- NA
      ind <- sapply(out, function(i) all(!is.na(i)))
      out <- out[, .SD, .SDcols = ind]
    }

    out <- out[, list(pred = mean(pred)), by = c(".row", var_char)]


    if (compare) {
      out <- tidybayes::compare_levels(out, pred, {{var}})
    }
    if (summary) {
      if (compare) {
        .rope_range <- c(-.1, .1)

        dens_at_0 <- function(i) {
          dens <- density(x = as.numeric(i))
          approx(dens$x, dens$y, xout = 0, n = 1000, rule = 2)$y
        }

        out <- data.table::data.table(
          levels = unique(out[[var_char]]),
          Median = round(median(out$pred), 2),
          `IC~95%~` = paste0("[", paste(round(tidybayes::hdci(x = as.numeric(out$pred)), 2), collapse = ", "), "]"),
          pd = format(bayestestR:::p_direction.numeric(out$pred)[[2]], nsmall = 2, digits = 2),
          ps = format(bayestestR:::p_significance.numeric(out$pred, threshold = .rope_range)[[2]], nsmall = 2, digits = 2),
          `% in ROPE` = format(bayestestR:::rope.numeric(as.numeric(out$pred), range = .rope_range)$ROPE_Percentage, nsmall = 2, digits = 2),
          ## Density at 0 derived from empirical normal distribution from N(0, 10)
          bf = round(dnorm(0, 0, 3) / dens_at_0(out$pred), 3)
        )
      } else {
        out <- data.table::data.table(
          levels = unique(out[[var_char]]),
          Median = round(median(out$pred), 2),
          `CI~95%~` = paste0("[", paste(round(tidybayes::hdci(x = as.numeric(out$pred)), 2), collapse = ", "), "]")
        )
      }
    }
    names(out)[1] <- c("Levels")
    out|>
      .f()
  }
)(pittsburg_horasdormidas)
