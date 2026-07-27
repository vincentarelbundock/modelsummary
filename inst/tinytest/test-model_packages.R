source("helpers.R")
using("modelsummary")

# `model_packages()` tells `future` which namespaces a worker must attach to
# handle the models. Getting it wrong is not loud: the worker fails to dispatch,
# `get_gof()` swallows the error, and the table silently loses every GOF row
# while warning about the model class instead.

mp <- modelsummary:::model_packages

# base-package classes need nothing extra: stats and base are always there
mod_lm <- lm(mpg ~ wt, data = mtcars)
expect_true(!"stats" %in% mp(mod_lm))
expect_true(!"base" %in% mp(mod_lm))

mod_glm <- glm(am ~ wt, data = mtcars, family = binomial())
expect_true(!"stats" %in% mp(mod_glm))

# a single model may be passed unwrapped or in a list. A fitted model IS a
# list, so an `is.list()` guard here would iterate over its components and
# silently return nothing -- this is the regression test for that.
expect_equal(mp(mod_lm), mp(list(mod_lm)))
expect_equal(mp(mod_glm), mp(list(mod_glm)))

# every returned name must be a loaded namespace, or the worker cannot attach it
expect_true(all(mp(list(mod_lm, mod_glm)) %in% loadedNamespaces()))

# the real case: a class whose methods live in a contributed package
if (requireNamespace("MASS", quietly = TRUE)) {
  set.seed(1)
  dat <- data.frame(x = rnorm(200))
  dat$y <- MASS::rnegbin(200, mu = exp(1 + 0.5 * dat$x), theta = 2)
  mod_nb <- MASS::glm.nb(y ~ x, data = dat)
  expect_true("MASS" %in% mp(mod_nb))
  # and it is picked up alongside others, without duplicates
  both <- mp(list(mod_lm, mod_nb, mod_nb))
  expect_true("MASS" %in% both)
  expect_equal(anyDuplicated(both), 0L)
}

if (requireNamespace("glmmTMB", quietly = TRUE)) {
  set.seed(2)
  dat <- data.frame(x = rnorm(200))
  dat$y <- rpois(200, lambda = exp(1 + 0.4 * dat$x))
  mod_tmb <- glmmTMB::glmmTMB(y ~ x, data = dat, family = stats::poisson)
  expect_true("glmmTMB" %in% mp(mod_tmb))
}

# unknown / classless objects must not error, and must not invent a package
expect_equal(length(mp(structure(list(), class = "no_such_model_class"))), 0L)
expect_equal(length(mp(list())), 0L)

# a modelsummary_list carries its own estimates and needs no modelling package
ml <- structure(
  list(
    tidy = data.frame(term = "a", estimate = 1),
    glance = data.frame(nobs = 1)
  ),
  class = "modelsummary_list"
)
expect_true(all(mp(ml) %in% loadedNamespaces()))
