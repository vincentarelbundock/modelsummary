# coef_map must include at least one term per model
mod <- lm(hp ~ mpg + cyl, mtcars)
expect_error(
  modelsummary(mod, coef_map = c("Blah" = "blah")),
  pattern = "At least one of the term names")

# combine different regressors and collapse rows
mod = lm(hp ~ mpg + factor(cyl), mtcars)
tab1 <- modelsummary(mod,
  output = "data.frame",
  coef_map = c("(Intercept)", "factor(cyl)6"))
tab2 <- modelsummary(mod,
  output = "data.frame",
  coef_map = c("(Intercept)" = "blah", "factor(cyl)6" = "blah blah"))
expect_equivalent(tab1[[4]], tab2[[4]])
tab1[[2]][1:4] = c("(Intercept)", "(Intercept)", "factory(cyl)6", "factory(cyl)6")
tab2[[2]][1:4] = c("blah", "blah", "blah blah", "blah blah")

# combine different regressors and collapse rows
cmap <- c("(Intercept)" = "Constant", "drat" = "Combined", "qsec" = "Combined")
mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
raw <- modelsummary(mod, output = "data.frame", coef_map = cmap)
truth <- c("Constant", "Constant", "Combined", "Combined", "Num.Obs.")
expect_equivalent(unname(raw[[2]][1:5]), truth)

# reorder and omit
cmap <- c("qsec" = "qsec", "drat" = "drat")
mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
raw <- modelsummary(mod, coef_map = cmap, output = "dataframe")
truth <- c("qsec", "qsec", "drat", "drat", "Num.Obs.")
expect_equivalent(unname(raw[[2]][1:5]), truth)

# coef_map with multiple vertical statistics

cm <- c(
  "(Intercept)" = "Intercept",
  "factor(cyl)6" = "6-cylinder",
  "factor(cyl)8" = "8-cylinder")
models <- list()
models[["OLS"]] <- lm(mpg ~ factor(cyl), mtcars)
models[["Logit"]] <- glm(am ~ factor(cyl), mtcars, family = binomial)

mat <- modelsummary(models, coef_map = cm, output = "dataframe")
expect_inherits(mat, "data.frame")
expect_equivalent(dim(mat), c(14, 5))

mat <- modelsummary(
  models,
  output = "dataframe",
  statistic = c("std.error", "conf.int"),
  coef_map = cm
)
expect_inherits(mat, "data.frame")
expect_equivalent(dim(mat), c(17, 5))

rows <- read.csv(
  text =
    "term        , OLS , Logit
     4-cylinder  , -   , -
     12-cylinder , -   , -")

mat <- modelsummary(models,
  output = "dataframe",
  statistic = c("std.error", "conf.int"),
  add_rows = rows,
  coef_map = cm)

expect_inherits(mat, "data.frame")
expect_equivalent(dim(mat), c(19, 5))


# Isuee #757
requiet("lme4")
sleepstudy_fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
tab <- modelsummary(
  sleepstudy_fit,
  coef_map = c("SD (Observations)", "(Intercept)"),
  output = "dataframe"
)
expect_equivalent(tab$term[1:3], c("SD (Observations)", "(Intercept)", "(Intercept)"))
expect_equivalent(tab$statistic[1:3], c("estimate", "estimate", "std.error"))
