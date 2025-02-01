source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

mod <- list()
mod$OLS <- lm(hp ~ drat, data = mtcars)
mod$Logit <- glm(am ~ drat + hp, data = mtcars, family = binomial())

# rounding 5 decimals
raw <- modelsummary(mod, fmt = "%.5f", output = "dataframe")
truth <- c("353.65253", "(76.04873)", "-57.54523", "(20.92205)", "", "")
expect_equivalent(truth, unname(raw[[4]])[1:6])
truth <- c("-29.07608", "(12.41692)", "7.30978", "(3.04660)", "0.01079", "(0.00933)")
expect_equivalent(truth, unname(raw[[5]])[1:6])

# rounding internal function
fun <- fmt_decimal(3)
expect_equivalent(fun(2), "2.000")

# fmt integer respects options(OutDec)
options(OutDec = ",")
known <- c("353,6525", "(76,0487)", "-57,5452", "(20,9221)", "", "", "32", "0,201", "0,175", "359,2", "363,6", "-176,588", "7,565", "60,31")
x <- modelsummary(mod, fmt = 4, output = "data.frame")
expect_equivalent(x$OLS, known)
options(OutDec = ".")

# fmt character
options(OutDec = ".")
known <- c("353.6525", "(76.0487)", "-57.5452", "(20.9221)", "", "", "32", "0.201", "0.175", "359.2", "363.6", "-176.588", "7.565", "60.31")
x <- modelsummary(mod, fmt = "%.4f", output = "data.frame")
expect_equivalent(x$OLS, known)

# fmt function
known <- c("353.6525", "(76.0487)", "-57.5452", "(20.9221)", "", "", "32", "0.201", "0.175", "359.2", "363.6", "-176.588", "7.565", "60.31")
z <- modelsummary(mod, fmt = function(x) sprintf("%.4f", x), output = "data.frame")
expect_equivalent(known, z$OLS)

# significant digits per term
mod <- lm(mpg ~ hp + drat + qsec, data = mtcars)
tab <- modelsummary(mod,
  output = "dataframe",
  fmt = fmt_significant(2),
  statistic = "conf.int",
  gof_map = NA
)
expect_equivalent(tab[["(1)"]], c("17.7", "[-8.9, 44.4]", "-0.058", "[-0.087, -0.029]", "4.4", "[1.8, 7.1]", "-0.28", "[-1.29, 0.72]"))

### FUNCTION FACTORY REFACTOR
mod <- lm(mpg ~ hp + drat + qsec, data = mtcars)

# backward compatibility
tab <- modelsummary(mod, fmt = 3, output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.737")
tab <- modelsummary(mod, fmt = "%.5f", output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.73662")
tab <- modelsummary(mod, fmt = function(x) round(x, 2), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.74")
tab <- modelsummary(mod, fmt = NULL, output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.7366200480466")

# function factories
tab <- modelsummary(mod, fmt = fmt_decimal(4), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.7366")
tab <- modelsummary(mod, fmt = fmt_significant(3), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.7")
expect_equivalent(tab[["(1)"]][3], "-0.0580")
tab <- modelsummary(mod, fmt = fmt_sprintf("%.5f"), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.73662")
tab <- modelsummary(mod, fmt = function(x) round(x, 2), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.74")
tab <- modelsummary(mod, fmt = fmt_statistic(estimate = 4, conf.int = 1), statistic = "conf.int", output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.7366")
expect_equivalent(tab[["(1)"]][2], "[-8.9, 44.4]")
tab <- modelsummary(mod, fmt = fmt_term(hp = 4, drat = 1, default = 2), output = "dataframe")
expect_equivalent(tab[["(1)"]][1], "17.74")
expect_equivalent(tab[["(1)"]][3], "-0.0580")

# complicated
tab <- modelsummary(
  mod,
  output = "data.frame",
  statistic = c("std.error", "statistic", "conf.int"),
  fmt = fmt_statistic(
    estimate = 5,
    std.error = \(x) sprintf("%.3f", x),
    default = fmt_significant(1)
  ),
  gof_map = NA
)
expect_equivalent(tab[["(1)"]], c("17.73662", "(13.020)", "(1.4)", "[-8.93, 44.41]", "-0.05797", "(0.014)", "(-4.1)", "[-0.09, -0.03]", "4.42875", "(1.292)", "(3.4)", "[1.78, 7.07]", "-0.28407", "(0.489)", "(-0.6)", "[-1.29, 0.72]"))

tab <- modelsummary(
  mod,
  output = "data.frame",
  fmt = fmt_term(
    "(Intercept)" = 5,
    "hp" = 3,
    default = fmt_sprintf("%.1f")
  ),
  gof_map = NA
)
expect_equivalent(tab[["(1)"]], c("17.73662", "(13.01979)", "-0.058", "(0.014)", "4.4", "(1.3)", "-0.3", "(0.5)"))


# Issue #439
mod <- lm(mpg ~ hp, mtcars)
tab <- modelsummary(mod,
  output = "data.frame",
  fmt = fmt_statistic(estimate = 3, std.error = 1, r.squared = 7)
)
expect_equivalent(tab[c(1:2, 6), 4], c("30.099", "(1.6)", "0.6024373"))



# Issue #635
requiet("lme4")
set.seed(1024)
ID <- round(runif(30, min = 1, max = 10))
Conc <- runif(30, min = 8.050e+12, max = 8.520e+18)
Year <- round(runif(30, min = 2000, max = 2005))
dat <- data.frame("ID" = ID, "Conc" = Conc, "Year" = Year)
testmod <- lmer(Conc ~ Year + (1 | ID), data = dat)
modelsummary(testmod, output = "markdown")
expect_snapshot_print(
  modelsummary(testmod, fmt = fmt_sci(digits = 2), output = "markdown"),
  "fmt-fmt_sci_2"
)


# Issue #848: sign when rounded to zero
x <- fmt_decimal(2)(c(-1e-5, 1e-5))
expect_equal(x, c("-0.00", "0.00"))
dat <- transform(mtcars, mpg = mpg / 100000)
mod <- lm(mpg ~ hp, dat)
tab <- modelsummary(mod,
  fmt = fmt_statistic(estimate = 2, std.error = 2),
  output = "data.frame"
)
expect_true("-0.00" %in% tab[["(1)"]])
