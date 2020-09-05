# context('mice')

## load
# library(modelsummary)
# library(mice)

## impute
# set.seed(10393983)
# dat <- mtcars
# dat$wt[sample(1:nrow(dat), 4)] <- NA
# dat$hp[sample(1:nrow(dat), 3)] <- NA
# dat <- mice(dat, m = 5, printFlag = FALSE, seed = 10)

## fit
# f <- am ~ wt + hp
# mod <- list()
# mod$OLS <- with(dat, lm(am ~ wt + hp))
# mod$Logit <- with(dat, glm(am ~ wt + hp, family = binomial()))

## test
# test_that("ols and logit", {

# raw <- modelsummary:::extract_models(mod)

# truth <- c("1.568", "(0.231)", "0.002", "(0.001)", "-0.449", "(0.097)", "32", "0.536", "0.504", "5")
# expect_equal(truth, unname(raw[[4]]))

# truth <- c("18.234", "(7.553)", "0.029", "(0.016)", "-7.599", "(3.103)", "32", "", "", "5")
# expect_equal(truth, unname(raw[[5]]))

# })
