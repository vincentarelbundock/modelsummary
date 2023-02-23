source("helpers.R")
requiet("flextable")

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['Poisson 1']] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())

# no error with caption and notes
tab <- modelsummary(models, "flextable", title = "test title", notes = "test note", stars = TRUE)
expect_inherits(tab, "flextable")