source("helpers.R")
requiet("tibble")

# gof_map = NA
mod <- lm(mpg ~ hp, mtcars)
tab1 <- modelsummary(mod, output = "data.frame", gof_map = NA)
tab2 <- modelsummary(mod, output = "data.frame", gof_omit = ".*")
expect_equivalent(tab1, tab2)
expect_false("gof" %in% tab1$part)
expect_false("gof" %in% tab2$part)

# gof_map inputs are equivalent
mod <- lm(mpg ~ hp, mtcars)
gm <- tribble(
  ~raw,
  ~clean,
  ~fmt,
  "r.squared",
  "R-Squared",
  "%.7f"
)
w <- modelsummary(mod, output = "data.frame", gof_map = gm)
gm <- tribble(
  ~raw,
  ~clean,
  ~fmt,
  "r.squared",
  "R-Squared",
  function(x) sprintf("%.7f", x)
)
x <- modelsummary(mod, output = "data.frame", gof_map = gm)
gm <- tribble(
  ~raw,
  ~clean,
  ~fmt,
  "r.squared",
  "R-Squared",
  7
)
y <- modelsummary(mod, output = "data.frame", gof_map = gm)
gm <- list(list(raw = "r.squared", clean = "R-Squared", fmt = 7))
z <- modelsummary(mod, output = "data.frame", gof_map = gm)
expect_equivalent(w, x)
expect_equivalent(w, y)
expect_equivalent(w, z)

# character vector
mod <- lm(mpg ~ hp + drat, mtcars)
tab <- modelsummary(
  mod,
  gof_map = c("r.squared", "rmse", "nobs"),
  output = "data.frame"
)
expect_equivalent(tab$term[7:9], c("R2", "RMSE", "Num.Obs."))
tab <- modelsummary(
  mod,
  gof_map = c("junk", "rmse", "nobs"),
  output = "data.frame"
)
expect_inherits(tab, "data.frame")

tab <- modelsummary(
  mod,
  output = "data.frame",
  gof_map = ""
)
expect_equivalent(nrow(tab), 6)
