source("helpers.R")

# first time output='latex' produces a warning (run first)
mod <- lm(mpg ~ hp, mtcars)
expect_warning(modelsummary(mod, "latex"))

# first call raises a warning about `performance` metrics.
requiet("brms")
mod <- insight::download_model("brms_1")
expect_warning(modelsummary(mod, statistic = "conf.int"))
