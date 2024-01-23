source("helpers.R")
requiet("fixest")
requiet("tinysnapshot")
using("tinysnapshot")
fixest::setFixest_nthreads(1)

panels <- list(
"Panel A: MPG" = list(
    "A" = lm(mpg ~ hp, data = mtcars),
    "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
"Panel B: Displacement" = list(
    "A" = lm(disp ~ hp, data = mtcars),
    "C" = lm(disp ~ hp + factor(gear), data = mtcars)))

# (non-)matching models
panels <- list(
    "Panel A: MPG" = list(
        lm(mpg ~ hp, data = mtcars),
        lm(mpg ~ hp + factor(gear), data = mtcars)),
    "Panel B: Displacement" = list(
        lm(disp ~ hp, data = mtcars),
        lm(disp ~ hp + factor(gear), data = mtcars))
)
tab1 <- modelsummary(panels, gof_map = "nobs", output = "dataframe", shape = "rbind")
expect_equivalent(colnames(tab1), c(" ", "(1)", "(2)"))

panels <- list(
    "Panel A: MPG" = list(
        "A" = lm(mpg ~ hp, data = mtcars),
        "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
    "Panel B: Displacement" = list(
        "A" = lm(disp ~ hp, data = mtcars),
        "C" = lm(disp ~ hp + factor(gear), data = mtcars))
)
tab2 <- modelsummary(panels, gof_map = "nobs", output = "dataframe", shape = "rbind")
expect_equivalent(colnames(tab2), c(" ", "A", "B", "C"))

# stars note
p <- suppressWarnings(modelsummary(panels, output = "markdown", stars = TRUE, shape = "rbind"))
expect_true(any(grepl("Note", p)))

# output formats: no validity
p <- modelsummary(panels, output = "gt", shape = "rbind")
expect_inherits(p, "gt_tbl")
p <- modelsummary(panels, output = "latex", shape = "rbind")
expect_inherits(p, "knitr_kable")

# Issue #593: rbind vs rcollapse
panels <- list(
    list(
        lm(mpg ~ hp, data = mtcars),
        lm(mpg ~ hp + am, data = mtcars)),
    list(
        lm(qsec ~ hp, data = mtcars),
        lm(qsec ~ hp + am, data = mtcars)))
tab1 <- modelsummary(panels, shape = "rbind", gof_map = "nobs", output = "dataframe")
tab2 <- modelsummary(panels, shape = "rcollapse", gof_map = "nobs", output = "dataframe")
expect_true(nrow(tab1) == nrow(tab2) + 1)

# Issue #593: models with different FEs do not get collapsed
panels <- list(
    list(
        feols(mpg ~ cyl | gear, data = mtcars, cluster = ~hp),
        feols(mpg ~ cyl | gear + am, data = subset(mtcars, mpg > 20), cluster = ~hp)),
    list(
        feols(disp ~ cyl | gear, data = mtcars, cluster = ~hp),
        feols(disp ~ cyl | gear + carb, data = mtcars, cluster = ~hp))
)
tab <- modelsummary(panels, shape = "rcollapse", output = "dataframe")
expect_equivalent(sum(tab[[1]] == "FE: gear"), 2)

# Issue #593: models with identical FEs get collapsed
panels <- list(
    list(
        feols(mpg ~ cyl | gear, data = mtcars, cluster = ~hp),
        feols(mpg ~ cyl | gear + carb, data = subset(mtcars, mpg > 20), cluster = ~hp)),
    list(
        feols(disp ~ cyl | gear, data = mtcars, cluster = ~hp),
        feols(disp ~ cyl | gear + carb, data = mtcars, cluster = ~hp))
)
tab <- modelsummary(panels, shape = "rcollapse", output = "dataframe")
expect_equivalent(sum(tab[[1]] == "FE: gear"), 1)



# Issue #620
models <- list(
  mpg = lm(mpg ~ cyl + disp, mtcars),
  hp = lm(hp ~ cyl + disp, mtcars))
tab <- modelsummary(models,
  output = "data.frame",
  statistic = NULL,
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ",
  shape = "rcollapse",
  gof_map = c("nobs", "r.squared"))
expect_equivalent(nrow(tab), 9)

# Issue #626: shape="rbind" does not respect with add_rows
rows <- tibble::tribble(
    ~term,          ~OLS,  ~Logit,
    'Info',         '???', 'XYZ')
attr(rows, 'position') <- c(6)
gm <- c("r.squared", "nobs", "rmse")
panels <- list(
  list(
    lm(mpg ~ 1, data = mtcars),
    lm(mpg ~ qsec, data = mtcars)
  ),
  list(
    lm(hp ~ 1, data = mtcars),
    lm(hp ~ qsec, data = mtcars)
  )
)

expect_snapshot_print(
    modelsummary(
        panels,
        fmt = 2, # tolerance
        output = "markdown",
        shape = "rbind",
        gof_map = gm,
        add_rows = rows),
    "rbind-add_rows_rbind"
)
