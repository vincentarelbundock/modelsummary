rename_statistics <- function(
  x,
  conf_level = 0.95,
  statistic = NULL,
  estimate = NULL
) {
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
    "statistic" = "t",
    "{estimate}" = "Est.",
    "{std.error}" = "S.E.",
    "{p.value}" = "p",
    "{statistic}" = "t",
    "{stars}" = ""
  )

  if (!is.null(names(estimate))) {
    for (i in seq_along(estimate)) {
      dict[[estimate[[i]]]] <- names(estimate)[i]
    }
  }

  if (!is.null(names(statistic))) {
    # Otherwise model indentifiers get flatted in things like
    # `statistic = c("Confidence interval" = "conf.int")`
    names(statistic)[names(statistic) == ""] <- "\u00a0"
    for (i in seq_along(statistic)) {
      dict[[statistic[[i]]]] <- names(statistic)[i]
    }
  }

  out <- replace_dict(x, dict)
  return(out)
}
