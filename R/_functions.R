
## Hyperparameters
.iter <- 4500
.warmup <- 2000
.chains <- 4
.cores <- 4
.seed <- 1234
.control <- list(adapt_delta = .99, max_treedepth = 30)


## Bayesian GLM negative-binomial
bayesian_beta_binomial <- function(formula, data, file) {
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
bayesian_cumulative <- function(formula, data, file) {
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
bayesian_bernoulli <- function(formula, data, file) {
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
