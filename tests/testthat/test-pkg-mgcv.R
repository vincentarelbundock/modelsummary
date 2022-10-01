# pkgload::load_all()
# requiet("mgcv")

# dat <- gamSim(1, n = 4000, dist = "normal", scale = 2, verbose = FALSE)
# mod <- gam(y ~ s(x0) + s(x1) + s(x2), data = dat)


# tidy_custom.gam <- function(x, ...) {
#     out <- parameters::parameters(x)
#     out <- parameters::standardize_names(out, style = "broom")
#     out$combined <- ifelse(is.na(out$estimate), out$df, out$estimate)
#     out$p <- format.pval(out$p.value)
#     return(out)
# }

# get_estimates(mod)

# modelsummary(
#     mod,
#     output = "markdown",
#     estimate = "combined",
#     coef_omit = "^s\\(",
#     statistic = c(
#         "std.error",
#         "statistic",
#         "p.value"),
#     shape = term ~ model + statistic)
#     # gof_map = NA)


# rm("tidy_custom.gam")