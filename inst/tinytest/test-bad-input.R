source("helpers.R")

# multiple models must be wrapped in a list
mod <- lm(mpg ~ hp, mtcars)
expect_error(modelplot(mod, mod), pattern = "is sometimes raised")
expect_error(modelsummary(mod, mod), pattern = "is sometimes raised")

# coef_map: two variables with the same name within or between models
# within one model produces an error
x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
mod <- lm(y ~ x + z)
expect_error(modelsummary(mod, "data.frame", coef_map = c("x" = "X", "z" = "X")))
expect_error(modelsummary(mod, "data.frame", coef_rename = c("x" = "X", "z" = "X")))

# across models works well
mod <- list()
mod[[1]] <- lm(y ~ x)
mod[[2]] <- lm(y ~ z)
tab <- modelsummary(mod, output = "data.frame", coef_map = c("x" = "X", "z" = "X"))
expect_inherits(tab, "data.frame")