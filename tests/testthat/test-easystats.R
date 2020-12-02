context("easystats extractors (unsupported by broom)")

library(modelsummary)
library(lme4)

test_that("lme4 is supported by easystats but not broom", {
  d <- as.data.frame(ChickWeight)
  colnames(d) <- c("y", "x", "subj", "tx")
  mod <- lmer(y ~ tx * x + (x | subj), data = d)
  tab <- modelsummary(mod, output="dataframe")
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(22, 4))
})
