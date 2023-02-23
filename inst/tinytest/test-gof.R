mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

# Issue #520
tab <- modelsummary(mod, "dataframe")
expect_true("Log.Lik." %in% tab[["term"]])
expect_true(tab$OLS[tab$term == "Log.Lik."] != "")
expect_true(tab$Logit[tab$term == "Log.Lik."] != "")

# gof_omit omits everything
tab <- modelsummary(mod, gof_omit = ".*", output = "data.frame")
expect_equivalent(dim(tab), c(6, 5))

# custom gof_map omits everything by default
# error about factor levels, not clear why
if (getRversion() < "4.0.0") exit_file("old R")
gm <- read.csv(
  text =
    "raw,clean,fmt
nobs,Num.Obs,0
r.squared,R2,2")
tab <- modelsummary(mod, gof_map = gm, output = "dataframe")
expect_equivalent(dim(tab), c(8, 5))

# list of lists
f1 <- function(x) sprintf("%.0f", x)
f2 <- function(x) sprintf("%.1f", x)
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f1),
  list("raw" = "aic", "clean" = "AIC", "fmt" = f2))
tab <- modelsummary(mod, output = "data.frame", gof_map = gm)
tab <- tab[tab$part == "gof", ]
expect_equivalent(tab$OLS, c("32", "28.6"))
expect_equivalent(tab$Logit, c("32", "45.5"))

# omit all gof
gof_map_custom <- modelsummary::gof_map
gof_map_custom$omit <- TRUE
raw <- modelsummary(mod, gof_map = list(), output = "dataframe")
truth <- c("-1.986", "(0.434)", "0.665", "(0.120)", "", "")
expect_equivalent(truth, unname(raw[[4]]))
truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
expect_equivalent(truth, unname(raw[[5]]))

# omit all gof from only a few models
mod1 <- glm(vs ~ mpg, data = mtcars, family = "binomial")
mod2 <- lm(vs ~ mpg, data = mtcars)
gm <- modelsummary::gof_map
gm <- gm[gm$raw %in% c("r2", "r.squared"), ]
tmp <- modelsummary(list(mod1, mod2), gof_map = gm, output = "dataframe")
expect_equivalent(nrow(tmp), 5)
expect_equivalent(ncol(tmp), 5)

# F statistic included for both `lm` and `glm` objects
mod <- list(
  "OLS"   = lm(am ~ drat, data = mtcars),
  "Logit" = glm(am ~ qsec, data = mtcars, family = binomial())
)
tab <- modelsummary(mod, output = "data.frame")
expect_true(tab$OLS[tab$term == "F"] != "")
expect_true(tab$Logit[tab$term == "F"] != "")