rename_statistics <- function(x, conf_level = 0.95) {
    lb <- (1 - conf_level) / .02
    ub <- (conf_level + (1 - conf_level) / 2) * 100
    lb <- sprintf("%.1f %%", lb)
    ub <- sprintf("%.1f %%", ub)
    dict <- c(
        "conf.low" = lb,
        "conf.high" = ub,
        "estimate" = "Est.",
        "std.error" = "S.E.",
        "p.value" = "p",
        "statistic" = "t")
    out <- replace_dict(x, dict)
    return(out)
}
