source("helpers.R")
requiet("tinyviztest")
using("tinyviztest")

# : in interactions become x
mod <- lm(am ~ drat * mpg * vs, mtcars)
expect_snapshot_print(
  modelsummary(mod, "markdown"),
  "interaction-markdown")

# fixest i() becomes =
requiet("fixest")
mod <- suppressMessages(feols(Ozone ~ Solar.R + i(Month), airquality))
expect_snapshot_print(
  modelsummary(mod, "markdown", gof_map = list()),
  "interaction-markdown_no_gof")

# conditional conversion of : to x
mod <- lm(am ~ drat:mpg, mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  coef_rename = c("DRAT" = "drat"))
expect_true("drat:mpg" %in% tab$term)

mod <- lm(mpg ~ disp, mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  coef_rename = c("disp" = "a:b"))
expect_true("a:b" %in% tab$term)

# Issue #684
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm * flipper_length_mm + species,
           data = dat, family = binomial)
mfx <- avg_slopes(mod)
expect_equivalent(mfx$estimate[1], .0278588, tol = 1e-3)
expect_equivalent(mfx$std.error[1], .0059463, tol = 1e-3)

# . use "~/penguins.dta", clear
# . encode species, gen(speciesid)
# . qui logit large_penguin c.bill_length_mm##c.flipper_length_mm i.speciesid
# . margins, dydx(*)
#
# Average marginal effects                                   Number of obs = 342
# Model VCE: OIM
#
# Expression: Pr(large_penguin), predict()
# dy/dx wrt:  bill_length_mm flipper_length_mm 2.speciesid 3.speciesid
#
# -----------------------------------------------------------------------------------
#                   |            Delta-method
#                   |      dy/dx   std. err.      z    P>|z|     [95% conf. interval]
# ------------------+----------------------------------------------------------------
#    bill_length_mm |   .0278588   .0059463     4.69   0.000     .0162043    .0395134
# flipper_length_mm |   .0104927   .0023708     4.43   0.000      .005846    .0151394
#                   |
#         speciesid |
#        Chinstrap  |  -.4127852   .0560029    -7.37   0.000    -.5225488   -.3030216
#           Gentoo  |   .0609265   .1073649     0.57   0.570    -.1495048    .2713578
# -----------------------------------------------------------------------------------
# Note: dy/dx for factor levels is the discrete change from the base level.