context("statistic_override")

library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model
models <- list()
models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[['Poisson 1']] <- glm(Literacy ~ Crime_prop + Donations, dat, family = poisson())
models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[['Poisson 2']] <- glm(Desertion ~ Crime_prop + Donations, dat, family = poisson())
models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())

# reference tables
results <- list()

results[['one sandwich']] <- msummary(
  models,
  output = "data.frame",
  statistic_override = vcov,
  statistic = 'p.value',
  fmt = "%.7f")

results[['many sandwiches']] <- msummary(
  models,
  output = "data.frame",
  statistic_override = list(vcov, vcov, vcov, vcov, vcov),
  fmt = "%.7f")

results[['list of matrices']] <- msummary(
  models,
  output = "data.frame",
  statistic_override = lapply(models, vcov),
  fmt = "%.7f")

results[['hardcoded numerical']] <- msummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  statistic_override = list(
    `OLS 1` = c('(Intercept)' = 2, Crime_prop = 3, Infants = 4),
    `NBin 1` = c('(Intercept)' = 3, Crime_prop = -5, Donations = 3),
    `OLS 2` = c('(Intercept)' = 7, Crime_prop = -6, Infants = 9),
    `NBin 2` = c('(Intercept)' = 4, Crime_prop = -7, Donations = -9),
    `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2)))

results[['hardcoded arbitrary']] <- msummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  statistic_override = list(
    `OLS 1` = c('(Intercept)' = "!!", Crime_prop = "!!!", Infants = "!!!!"),
    `NBin 1` = c('(Intercept)' = "[1, 5]", Crime_prop = "[-5, +5]", Donations = "95% CI[-2, -1]"),
    `OLS 2` = c('(Intercept)' = "7", Crime_prop = "-6", Infants = "9"),
    `NBin 2` = c('(Intercept)' = "\U03B5", Crime_prop = "\U2135", Donations = "\U0414"),
    `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2)) )

# we are not interested in GOFs in this test
for (i in seq_along(results)) {
  results[[i]] <- results[[i]] %>%
    dplyr::filter(group == "estimates") %>%
    dplyr::select(-group, -statistic)
}

# save known values (comment out until new manual check)
# saveRDS(results, file="known_output/statistic-override.rds")

# load reference values (comment out when updating)
reference <- readRDS(file = "known_output/statistic-override.rds")


test_that("bad function", {
  expect_error(msummary(models, statistic_override = na.omit))
})

test_that("bad matrix", {
  mat <- matrix(1, ncol = 2, nrow = 2)
  expect_error(msummary(models, statistic_override = mat))
})

test_that("vector must be named", {
  vec <- as.numeric(1:3)
  expect_error(msummary(models[[1]], statistic = "std.error", statistic_override = vec))
})

test_that("statistic_override content", {
  expect_equivalent(results[["one sandwich"]], reference[["one sandwich"]])
  expect_equivalent(results[["many sandwiches"]], reference[["many sandwiches"]])
  expect_equivalent(results[["list of matrices"]], reference[["list of matrices"]])
  expect_equivalent(results[["hardcoded numerical"]], reference[["hardcoded numerical"]])
  expect_equivalent(results[["hardcoded arbitrary"]], reference[["hardcoded arbitrary"]])
})

test_that("useless: function but no ci needed", {
  expect_error(modelsummary(models, statistic_override=vcov, conf_level=NULL), NA)
})
