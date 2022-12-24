mod <- lm(mpg ~ hp + drat + qsec, data = mtcars)

Q
pkgload::load_all()

modelsummary(
    mod,
    statistic = c("std.error", "conf.int"),
    fmt = fmt_statistic(
        estimate = 5,
        std.error = \(x) sprintf("%.3f", x),
        default = 1),
    gof_map = NA)

modelsummary(
    mod,
    statistic = c("std.error", "conf.int"),
    fmt = fmt_term(
        `(Intercept)` = 5,
        hp = 3,
        default = \(x) sprintf("%.1f", x)),
    gof_map = NA)


# Examples

# backward comptibility
modelsummary(mod, fmt = 3)
modelsummary(mod, fmt = "%.5f")
modelsummary(mod, fmt = function(x) round(x, 2))
modelsummary(mod, fmt = NULL)

# function factories
modelsummary(mod, fmt = fmt_significant(3))
modelsummary(mod, fmt = fmt_decimal(4))
modelsummary(mod, fmt = fmt_sprintf("%.5f"))
modelsummary(mod, fmt = fmt_function(fun))
modelsummary(mod, fmt = fmt_statistic(estimate = 3, conf.int = 2))
modelsummary(mod, fmt = fmt_term(hp = 3, drat = 2))
modelsummary(mod, fmt = fmt_identity())