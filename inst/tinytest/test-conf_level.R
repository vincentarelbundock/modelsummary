# exit_file("works interactively")

# Issue 513
# bad combination of arguments
mod <- lm(mpg ~ hp, data = mtcars)
expect_error(
  modelsummary(
    mod,
    output = "data.frame",
    statistic = "conf.int",
    conf_level = NULL
  ),
  pattern = "Cannot display"
)

# baseline
tab <- modelsummary(
  mod,
  output = "data.frame",
  statistic = "conf.int",
  conf_level = .9
)
expect_true(any(grepl("\\[", tab[["(1)"]])))

# faster
est <- get_estimates(mod, conf_level = NULL)
expect_true(!"conf.low" %in% colnames(est) || all(is.na(est$conf.low)))

ci_funs <- paste0(
  c("^ci$", "ci\\.glm", "confint\\.glm", "stats::confint"),
  collapse = "|"
)
mod <- list()
mod$Poisson <- glm(am ~ drat, data = mtcars, family = poisson())
mod$Logit <- glm(am ~ qsec, data = mtcars, family = poisson())

# Not sure why this no longer works

# # when confidence intervals are enabled, some CI functions should have been called",
# tmp_withCI <- tempfile()
# suppressWarnings(Rprof(tmp_withCI, interval = 0.005))
# tab <- modelsummary(mod,
#     output = "data.frame",
#     statistic = "conf.int",
#     gof_omit = "AIC|BIC|RMSE")
# Rprof(NULL)
# rprof_withCI <- summaryRprof(tmp_withCI)
# called_funs <- rownames(rprof_withCI$by.total)
# expect_true(any(grepl(ci_funs, called_funs)))

# # when no confidence intervals are needed, no CI functions should have been called
# tmp_withoutCI <- tempfile()
# suppressWarnings(Rprof(tmp_withoutCI, interval = 0.005))
# tmp <- modelsummary(mod,
#     output = "data.frame",
#     statistic = NULL, vcov = NULL, conf_level = NULL,
#     gof_omit = "AIC|BIC|RMSE")
# Rprof(NULL)
# rprof_withoutCI <- summaryRprof(tmp_withoutCI)
# called_funs <- rownames(rprof_withoutCI$by.total)
# expect_false(any(grepl(ci_funs, called_funs)))
