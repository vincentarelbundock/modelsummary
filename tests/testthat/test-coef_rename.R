library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("rename 2 out of 3 coefficients", {
  cmap <- c('(Intercept)'='Constant', 'drat'='Rear axle ratio')
  mod <- modelsummary(mod, "dataframe", coef_rename=cmap)
  known <- c("Constant", "Constant", "Rear axle ratio", "Rear axle ratio", "qsec")
  expect_equal(mod$term[1:5], known)
})

test_that("coef_rename and coef_map are incompatible", {
  cmap <- c('(Intercept)'='Constant', 'drat'='Rear axle ratio')
  expect_error(modelsummary(mod, coef_rename=cmap, coef_map=cmap))
})


test_that("regression test: coef_rename() function", {
    x <- list(
      lm(mpg ~ factor(cyl) + drat + disp, data = mtcars),
      lm(hp ~ factor(cyl) + drat + disp, data = mtcars))
    expect_error(modelsummary(dvnames(x), coef_rename = coef_rename), NA)
})


test_that("unnamed coef_rename + coef_omit numeric", {
  mod <- lm(mpg ~ hp + wt + disp + carb + gear + vs + cyl + drat + am + hp * wt, data = mtcars)
  tab <- modelsummary(mod, output = "data.frame", coef_omit = c(3:11), coef_rename = c("Constant", "Horsepower"), gof_map = NA)
  expect_equal(nrow(tab), 4)
})


# Q
# pkgload::load_all()
# mod <- lm(mpg ~ factor(cyl), mtcars)
# msummary(mod, coef_rename = TRUE)


# library(modelsummary)

# dat1 <- mtcars
# dat1$mpg <- haven::labelled(dat1$mpg, label = "Miles per gallon")

# dat2 <- mtcars
# dat2$mpg2 <- haven::labelled(dat2$mpg, label = "Miles per gallon")

# # mpg and mpg2 have the same label
# should_warn_across <- list(
#   lm(hp ~ mpg + drat, dat = dat1),
#   lm(hp ~ mpg2 + wt, dat = dat2)
# )

# modelsummary(should_warn_across, output = "markdown", coef_rename = TRUE)

# parameters(should_warn_across[[2]], pretty_names = "labels")

# library(parameters)
# data(efc, package = "datawizard")

# # simple model
# m <- lm(neg_c_7 ~ e42dep + c172code, data = efc)
# modelsummary(m, coef_rename = TRUE)
# mp <- model_parameters(m, pretty_names = "labels")
# print(mp, pretty_names = "labels")
