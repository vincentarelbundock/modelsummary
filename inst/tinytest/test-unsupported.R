# CRAN and old versions fail when trying to load `broom.mixed`
# unsupported models require `broom.mixed`
# should no longer be a problem now that easystats is first
# skip_on_cran()
if (getRversion() < "4.0.0") exit_file("old R")

# Characters raise error
# build issue with packages broom.mixed and TMB
mod <- list()
mod[[1]] <- lm(hp ~ mpg, mtcars)
mod[[2]] <- 'ljaksdf'
expect_error(suppressWarnings(modelsummary(mod)))
expect_warning(modelsummary(mod[[2]]))
expect_error(suppressWarnings(modelsummary(mod[[2]])))

# Numerics raise error
mod <- list()
mod[[1]] <- lm(hp ~ mpg, mtcars)
mod[[2]] <- 1:30
expect_warning(expect_error(modelsummary(mod)))
expect_warning(expect_error(modelsummary(mod[[2]])))

# NULL raises error
mod <- lm(hp ~ mpg, mtcars)
mod <- list(mod, NULL)
expect_error(modelsummary(mod))
expect_error(modelsummary(mod[[2]]))