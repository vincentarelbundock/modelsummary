source("helpers.R")
requiet("survival")

# Issue #547
bladder1 <- bladder[bladder$enum < 5, ]
mod <- coxph(
  Surv(stop, event) ~ (rx + size + number) * strata(enum),
  cluster = id,
  bladder1,
  robust = TRUE
)
tab <- modelsummary(
  mod,
  estimate = "p.value",
  fmt = 10,
  statistic = NULL,
  gof_map = NA,
  output = "dataframe"
)
p <- coef(summary(mod))[, "Pr(>|z|)"]
expect_equivalent(as.numeric(tab[["(1)"]]), p, ignore_attr = TRUE)
