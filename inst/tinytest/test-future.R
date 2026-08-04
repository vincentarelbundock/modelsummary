source("helpers.R")
using("modelsummary")

# A `future` plan with more than one worker used to silently drop every
# goodness-of-fit row from a table of more than one model: `future` attaches
# only the packages it infers from static analysis of the extraction loop, so
# the worker could not dispatch on the model class, and `get_gof()` swallowed
# the error into a warning that blamed the class instead of the plan.
# `future.packages = .packages()` mirrors the main session on the worker.

exit_if_not(!ON_CRAN)
exit_if_not(requiet("future"))
exit_if_not(requiet("future.apply"))

old_plan <- future::plan(future::multisession, workers = 2)

# base models: the parallel table must match the sequential one exactly
mod <- list(
  lm(mpg ~ wt, data = mtcars),
  lm(mpg ~ wt + hp, data = mtcars)
)
par <- modelsummary(mod, output = "dataframe")
future::plan(future::sequential)
seq <- modelsummary(mod, output = "dataframe")
expect_equal(par, seq)
expect_true("Num.Obs." %in% par[[2]])

# the reported case: a class whose methods live in a contributed package
if (requiet("glmmTMB")) {
  set.seed(1)
  dat <- data.frame(x = rnorm(300), z = rnorm(300))
  dat$y <- rpois(300, lambda = exp(1 + 0.4 * dat$x))
  mod <- list(
    a = glmmTMB::glmmTMB(y ~ x, data = dat, family = stats::poisson),
    b = glmmTMB::glmmTMB(y ~ x + z, data = dat, family = stats::poisson)
  )

  future::plan(future::multisession, workers = 2)
  nwarn <- 0
  par <- withCallingHandlers(
    modelsummary(mod, output = "dataframe"),
    warning = function(w) {
      nwarn <<- nwarn + 1
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(nwarn, 0)
  future::plan(future::sequential)
  seq <- modelsummary(mod, output = "dataframe")

  expect_equal(par, seq)
  expect_true(all(c("Num.Obs.", "AIC", "BIC", "RMSE") %in% par[[2]]))
}

future::plan(old_plan)
