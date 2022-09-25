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
    # glue string
    out <- gsub("\\{estimate\\}", "Est. ", out)
    out <- gsub("\\{std.error\\}", "S.E. ", out)
    out <- gsub("\\{p.value\\}", "p ", out)
    out <- gsub("\\{statistic\\}", "t", out)
    out <- gsub("\\{stars\\}", "", out)
    return(out)
}
