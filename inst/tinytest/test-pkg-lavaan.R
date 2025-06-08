source("helpers.R")
requiet("lavaan")

# Issue #502
model <- 'ind60 =~ x1 + x2 + x3'
fit <- lavaan::cfa(model, data = PoliticalDemocracy)
void <- capture.output({
  tab <- modelsummary(fit, output = "data.frame", standardize = "all")
})
expect_inherits(tab, "data.frame")
expect_true(nrow(tab) > 7)
expect_true(ncol(tab) > 3)
