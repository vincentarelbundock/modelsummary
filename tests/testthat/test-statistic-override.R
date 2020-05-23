context("statistic_override")

library(gt)
library(MASS)
library(dplyr)
library(sandwich)
library(modelsummary)
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model
models <- list()
models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[['NBin 1']] <- glm.nb(Literacy ~ Crime_prop + Donations, dat)
models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[['NBin 2']] <- glm.nb(Desertion ~ Crime_prop + Donations, dat)
models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())

test_that("sandwich::vcovHC p.value", {
    raw <- modelsummary::extract(models, statistic_override = sandwich::vcovHC, statistic = 'p.value', fmt = '%.7f')

    truth <- c("64.1141636", "(0.0000000)", "-0.0015826", "(0.0005236)",
               "-0.0006533", "(0.0030738)", "", "", "86", "0.237", "0.218",
               "718.8", "728.6", "-355.382")
    expect_equal(raw[[4]], truth)

    truth <- c("4.2180730", "(0.0000000)", "-0.0000596", "(0.0000026)", "", "",
               "-0.0000138", "(0.2264781)", "86", "", "", "720.2", "730.0",
               "-356.106")
    expect_equal(raw[[5]], truth)

    truth <- c("57.3311254", "(0.0000000)", "-0.0022986", "(0.0105710)",
               "0.0002203", "(0.5507619)", "", "", "86", "0.073", "0.051",
               "797.9", "807.8", "-394.974")
    expect_equal(raw[[6]], truth)

    truth <- c("4.3841816", "(0.0000000)", "-0.0000719", "(0.0174976)", "", "",
               "-0.0000091", "(0.6003393)", "86", "", "", "803.2", "813.1",
               "-397.620")
    expect_equal(raw[[7]], truth)

    truth <- c("1.0061258", "(0.2043004)", "-0.0000506", "(0.5717742)",
               "-0.0000246", "(0.5278440)", "", "", "86", "", "", "123.0",
               "130.4", "-58.498")
    expect_equal(raw[[8]], truth)

})

test_that("list of functions", {

    raw <- modelsummary::extract(models, statistic_override = list(vcov, vcovHC, vcovHAC, vcovHC, vcov), fmt = '%.5f')

    truth <- c("64.11416", "(5.24690)", "-0.00158", "(0.00057)", "-0.00065",
               "(0.00020)", "", "", "86", "0.237", "0.218", "718.8", "728.6",
               "-355.382")
    expect_equal(raw[[4]], truth)

    truth <- c("4.21807", "(0.12184)", "-0.00006", "(0.00001)", "", "",
               "-0.00001", "(0.00001)", "86", "", "", "720.2", "730.0",
               "-356.106")
    expect_equal(raw[[5]], truth)

    truth <- c("57.33113", "(7.78405)", "-0.00230", "(0.00077)", "0.00022",
               "(0.00029)", "", "", "86", "0.073", "0.051", "797.9", "807.8",
               "-394.974")
    expect_equal(raw[[6]], truth)

    truth <- c("4.38418", "(0.25900)", "-0.00007", "(0.00003)", "", "",
               "-0.00001", "(0.00002)", "86", "", "", "803.2", "813.1",
               "-397.620")
    expect_equal(raw[[7]], truth)

    truth <- c("1.00613", "(0.70988)", "-0.00005", "(0.00008)", "-0.00002",
               "(0.00003)", "", "", "86", "", "", "123.0", "130.4", "-58.498")
    expect_equal(raw[[8]], truth)

})

test_that("list of vcov matrices", {

    vcov_matrices <- lapply(models, vcovHC)

    raw <- modelsummary::extract(models, statistic_override = vcov_matrices, fmt = '%.5f')

    truth <- c("64.11416", "(4.50309)", "-0.00158", "(0.00044)", "-0.00065",
               "(0.00021)", "", "", "86", "0.237", "0.218", "718.8", "728.6",
               "-355.382")
    expect_equal(raw[[4]], truth)

    truth <- c("4.21807", "(0.12184)", "-0.00006", "(0.00001)", "", "",
               "-0.00001", "(0.00001)", "86", "", "", "720.2", "730.0",
               "-356.106")
    expect_equal(raw[[5]], truth)

    truth <- c("57.33113", "(8.70411)", "-0.00230", "(0.00088)", "0.00022",
               "(0.00037)", "", "", "86", "0.073", "0.051", "797.9", "807.8",
               "-394.974")
    expect_equal(raw[[6]], truth)

    truth <- c("4.38418", "(0.25900)", "-0.00007", "(0.00003)", "", "",
               "-0.00001", "(0.00002)", "86", "", "", "803.2", "813.1",
               "-397.620")
    expect_equal(raw[[7]], truth)

    truth <- c("1.00613", "(0.79260)", "-0.00005", "(0.00009)", "-0.00002",
               "(0.00004)", "", "", "86", "", "", "123.0", "130.4", "-58.498")
    expect_equal(raw[[8]], truth)

})

test_that("list of hardcoded numerical values", {

    custom_stats <- list(`OLS 1` = c('(Intercept)' = 2, Crime_prop = 3, Infants = 4),
                         `NBin 1` = c('(Intercept)' = 3, Crime_prop = -5, Donations = 3),
                         `OLS 2` = c('(Intercept)' = 7, Crime_prop = -6, Infants = 9),
                         `NBin 2` = c('(Intercept)' = 4, Crime_prop = -7, Donations = -9),
                         `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2))

    raw <- modelsummary::extract(models, statistic_override = custom_stats)

    truth <- c("64.114", "(2.000)", "-0.002", "(3.000)", "-0.001", "(4.000)",
               "", "", "86", "0.237", "0.218", "718.8", "728.6", "-355.382")
    expect_equal(raw[[4]], truth)

    truth <- c("4.218", "(3.000)", "-0.000", "(-5.000)", "", "", "-0.000",
               "(3.000)", "86", "", "", "720.2", "730.0", "-356.106")
    expect_equal(raw[[5]], truth)

    truth <- c("57.331", "(7.000)", "-0.002", "(-6.000)", "0.000", "(9.000)",
               "", "", "86", "0.073", "0.051", "797.9", "807.8", "-394.974")
    expect_equal(raw[[6]], truth)

    truth <- c("4.384", "(4.000)", "-0.000", "(-7.000)", "", "", "-0.000",
               "(-9.000)", "86", "", "", "803.2", "813.1", "-397.620")
    expect_equal(raw[[7]], truth)

    truth <- c("1.006", "(1.000)", "-0.000", "(-5.000)", "-0.000", "(-2.000)",
               "", "", "86", "", "", "123.0", "130.4", "-58.498")
    expect_equal(raw[[8]], truth)

})

test_that("list of hardcoded character and possibly numeric values", {

    custom_stats <- list(`OLS 1` = c('(Intercept)' = "!!", Crime_prop = "!!!", Infants = "!!!!"),
                         `NBin 1` = c('(Intercept)' = "[1, 5]", Crime_prop = "[-5, +5]", Donations = "95% CI[-2, -1]"),
                         `OLS 2` = c('(Intercept)' = "7", Crime_prop = "-6", Infants = "9"),
                         `NBin 2` = c('(Intercept)' = "\U03B5", Crime_prop = "\U2135", Donations = "\U0414"),
                         `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2))

    raw <- modelsummary::extract(models, statistic_override = custom_stats)

    truth <- c("64.114", "!!", "-0.002", "!!!", "-0.001", "!!!!",
               "", "", "86", "0.237", "0.218", "718.8", "728.6", "-355.382")
    expect_equal(unname(raw[[4]]), truth)

    truth <- c("4.218", "[1, 5]", "-0.000", "[-5, +5]", "", "", "-0.000",
               "95% CI[-2, -1]", "86", "", "", "720.2", "730.0", "-356.106")
    expect_equal(unname(raw[[5]]), truth)

    truth <- c("57.331", "7", "-0.002", "-6", "0.000", "9",
               "", "", "86", "0.073", "0.051", "797.9", "807.8", "-394.974")
    expect_equal(unname(raw[[6]]), truth)

    truth <- c("4.384", "\U03B5", "-0.000", "\U2135", "", "", "-0.000",
               "\U0414", "86", "", "", "803.2", "813.1", "-397.620")
    expect_equal(unname(raw[[7]]), truth)

    truth <- c("1.006", "(1.000)", "-0.000", "(-5.000)", "-0.000", "(-2.000)",
               "", "", "86", "", "", "123.0", "130.4", "-58.498")
    expect_equal(unname(raw[[8]]), truth)

})
