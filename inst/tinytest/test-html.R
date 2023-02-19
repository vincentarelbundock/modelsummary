exit_file("snapshot")

# raw html output
mod <- lm(hp ~ mpg, data = mtcars)
expect_snapshot(modelsummary(mod, output = "html", gof_omit = ".*"))

# PR 538
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- msummary(mod, stars = TRUE, output = "html")
expect_true(grepl("\\&lt", tab))