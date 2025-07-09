source("helpers.R")
requiet("pscl")
requiet("Rdatasets")


mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

# rename 2 out of 3 coefficients
cmap <- c("(Intercept)" = "Constant", "drat" = "Rear axle ratio")
mod <- modelsummary(mod, "dataframe", coef_rename = cmap)
known <- c("Constant", "Constant", "Rear axle ratio", "Rear axle ratio", "qsec")
expect_equivalent(mod$term[1:5], known)

# coef_rename and coef_map are incompatible
cmap <- c("(Intercept)" = "Constant", "drat" = "Rear axle ratio")
expect_error(modelsummary(mod, coef_rename = cmap, coef_map = cmap))

# regression test: coef_rename() function
x <- list(
  lm(mpg ~ factor(cyl) + drat + disp, data = mtcars),
  lm(hp ~ factor(cyl) + drat + disp, data = mtcars)
)
tab <- modelsummary(dvnames(x), output = "dataframe", coef_rename = coef_rename)
expect_inherits(tab, "data.frame")

# unnamed coef_rename + coef_omit numeric
mod <- lm(
  mpg ~ hp + wt + disp + carb + gear + vs + cyl + drat + am + hp * wt,
  data = mtcars
)
tab <- modelsummary(
  mod,
  output = "data.frame",
  coef_omit = c(3:11),
  coef_rename = c("Constant", "Horsepower"),
  gof_map = NA
)
expect_equivalent(nrow(tab), 4)


# Issue #892: not duplicated after rename because we use component
dat <- Rdatasets::rddata("bioChemists", "pscl")
model <- pscl::zeroinfl(art ~ fem + factor(kid5), dat)
tab <- modelsummary(model,
  output = "data.frame",
  shape = component + term + statistic ~ model,
  coef_rename = \(x) gsub("^count_|^zero_", "", x))
expect_inherits(tab, "data.frame")
expect_error(
  modelsummary(model,
    output = "data.frame",
    shape = term + statistic ~ model,
    coef_rename = \(x) gsub("^count_|^zero_", "", x)),
  pattern = "duplicate labels")
