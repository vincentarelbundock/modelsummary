# lm F tests conform to `vcov` argument
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
res <- modelsummary(mod, "data.frame", vcov = c("HC1", "stata", "HC2", "classical"))
expect_true(res[res$term == "F", "(1)"] == res[res$term == "F", "(2)"])
expect_false(res[res$term == "F", "(1)"] == res[res$term == "F", "(3)"])
expect_false(res[res$term == "F", "(1)"] == res[res$term == "F", "(4)"])
expect_false(res[res$term == "F", "(2)"] == res[res$term == "F", "(3)"])
expect_false(res[res$term == "F", "(2)"] == res[res$term == "F", "(4)"])

# lm & glm F tests conform to `vcov` argument
mod <- list(
    lm(mpg ~ hp + factor(cyl), data = mtcars),
    glm(mpg ~ hp + factor(cyl), data = mtcars),
    lm(mpg ~ hp + factor(cyl), data = mtcars),
    glm(mpg ~ hp + factor(cyl), data = mtcars))
res <- modelsummary(mod, "data.frame", 
                    vcov = c("classical", "classical", "robust", "robust"))
expect_true(res[res$term == "F", "(1)"] == res[res$term == "F", "(2)"])
expect_true(res[res$term == "F", "(3)"] == res[res$term == "F", "(4)"])
expect_false(res[res$term == "F", "(1)"] == res[res$term == "F", "(4)"])

# mlm regression test: no error on sandwich
dat <- mtcars
mod <- lm(cbind(cyl, disp, hp) ~ mpg + drat, data = dat) 
est1 <- get_estimates(mod)
est2 <- get_estimates(mod, vcov = "HC3")
expect_true(all(est1$std.error != est2$std.error))

# Issue 473 
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(mod, output = "data.frame")
y <- insight::get_response(mod)
yhat <- insight::get_predicted(mod)
e <- y - yhat
rmse <- sqrt(mean(e^2))
rmse <- sprintf("%.2f", rmse)
expect_equivalent(rmse, tab[["(1)"]][tab$term == "RMSE"])