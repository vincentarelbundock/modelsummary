source("helpers.R")
using("checkmate")
exit_if_not(requiet("brms"))

# brms: diagnostics and tests
mod <- insight::download_model("brms_1")
tab <- modelsummary(mod, "data.frame", statistic = "conf.int")
expect_inherits(tab, "data.frame")
tab <- modelsummary(mod, "data.frame", diagnostic = "ESS", statistic = "ess")
expect_inherits(tab, "data.frame")
expect_error(modelsummary(mod, "data.frame", statistic = "rope"), pattern = "available")

# modelplot
mod <- marginaleffects:::modelarchive_model("brms_numeric2")
p <- modelplot(mod)
expect_inherits(p, "gg")
p <- modelplot(mod, draw = FALSE)
expect_data_frame(p, nrows = 3)