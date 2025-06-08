mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

# clean_stars unknown (we should never reach this)
expect_null(modelsummary:::clean_stars(FALSE))

# global stars_note
options(modelsummary_stars_note = FALSE)
tab <- modelsummary(mod, output = "markdown", stars = TRUE)
tab <- paste(tab, collapse = "")
expect_false(grepl("Note", paste(tab, collapse = "\n")))
options(modelsummary_stars_note = NULL)
tab <- modelsummary(mod, output = "markdown", stars = TRUE)
expect_true(grepl("p < 0.1", paste(tab, collapse = "\n")))

# no automatic note with glue stars
tab <- modelsummary(
  mod,
  output = "markdown",
  estimate = "{estimate}{stars}",
  stars = c("+" = .1)
)
expect_false(any(grepl("p < 0.1", as.character(tab))))

tab <- modelsummary(
  mod,
  output = "markdown",
  estimate = "estimate",
  statistic = "{std.error}{stars}",
  stars = c("+" = .1)
)
expect_false(any(grepl("p < 0.1", as.character(tab))))

tab <- modelsummary(
  mod,
  output = "markdown",
  stars = c("+" = .1)
)
expect_true(any(grepl("p < 0.1", as.character(tab))))

# glue stars
tab <- modelsummary(
  mod,
  stars = FALSE,
  output = "data.frame",
  gof_omit = ".*",
  estimate = c(
    "{estimate} ({std.error}){stars}",
    "{estimate} [{conf.low}, {conf.high}]"
  ),
  statistic = c(
    "({std.error}){stars}",
    "[{conf.low}, {conf.high}]"
  )
)
truth <- c(
  "-1.986 (0.434)***",
  "(0.434)***",
  "[-2.873, -1.099]",
  "0.665 (0.120)***",
  "(0.120)***",
  "[0.421, 0.909]",
  "",
  "",
  ""
)
expect_equivalent(truth, tab$OLS)
truth <- c(
  "4.739 [-2.760, 13.501]",
  "(4.045)",
  "[-2.760, 13.501]",
  "",
  "",
  "",
  "-0.288 [-0.784, 0.131]",
  "(0.228)",
  "[-0.784, 0.131]"
)
expect_equivalent(truth, tab$Logit)

# bug: make stars before rounding
m <- lm(vs ~ hp + mpg + factor(cyl), data = mtcars)
st <- c("*" = .49)
tab1 <- modelsummary(
  m,
  stars = st,
  output = "data.frame",
  statistic = NULL,
  fmt = 1,
  gof_omit = ".*"
)
tab2 <- modelsummary(
  m,
  stars = st,
  output = "data.frame",
  statistic = NULL,
  fmt = 3,
  gof_omit = ".*"
)
tab1 <- grepl("\\*", tab1[[4]])
tab2 <- grepl("\\*", tab2[[4]])
expect_equivalent(tab1, tab2)

# same stars with different statistics
m <- lm(dist ~ speed, data = cars)
tab1 <- modelsummary(m, stars = TRUE, output = "dataframe")
tab2 <- modelsummary(
  m,
  statistic = "p.value",
  stars = TRUE,
  output = "dataframe"
)
tab3 <- modelsummary(
  m,
  statistic = c("p.value", "conf.int"),
  stars = TRUE,
  output = "dataframe"
)
tab1 <- tab1[[4]]
tab2 <- tab2[[4]]
tab3 <- tab3[[4]]
expect_equivalent(tab1[c(1, 3)], tab2[c(1, 3)])
expect_equivalent(tab1[c(1, 3)], tab3[c(1, 4)])

# stars = FALSE
raw <- modelsummary(mod, stars = FALSE, output = "dataframe")
truth <- c("-1.986", "(0.434)", "0.665", "(0.120)")
expect_equivalent(truth, unname(raw[[4]][1:4]))
truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
expect_equivalent(truth, unname(raw[[5]][1:6]))

# stars = TRUE
raw <- modelsummary(mod, stars = TRUE, output = "dataframe")

truth <- c("-1.986***", "(0.434)", "0.665***", "(0.120)")
expect_equivalent(truth, unname(raw[[4]][1:4]))

truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")
expect_equivalent(truth, unname(raw[[5]][1:6]))

# custom stars
raw <- modelsummary(mod, stars = c("+" = .8, "*" = .1), output = "dataframe")
truth <- c("-1.986*", "(0.434)", "0.665*", "(0.120)")
expect_equivalent(truth, unname(raw[[4]][1:4]))
truth <- c("4.739+", "(4.045)", "", "", "-0.288+", "(0.228)")
expect_equivalent(truth, unname(raw[[5]][1:6]))

# Issue #532: extraneous stars
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  shape = term + model ~ statistic,
  statistic = c("std.error", "{p.value}{stars}"),
  fmt = fmt_statistic(estimate = 3, p.value = 2)
)
expect_false(any(grepl("\\*", tab[["Est."]])))
expect_true(any(grepl("\\*", tab[[6]])))

# Issue #798
mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
expect_snapshot_print(
  modelsummary(mod, output = "latex", stars = TRUE),
  "stars-issue798"
)
