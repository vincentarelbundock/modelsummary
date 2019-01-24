context("Test known output")

library(gt)
library(MASS)
library(gtsummary)
library(tidyverse)
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model
models <- list()
models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[['NBin 1']] <- glm.nb(Literacy ~ Crime_prop + Donations, dat)
models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[['NBin 2']] <- glm.nb(Desertion ~ Crime_prop + Donations, dat)
models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())

test_that("html_output: complex table", {
	cm <- c('Crime_prop' = 'Crime / Population',
		'Donations' = 'Donations',
		'Infants' = 'Infants',
		'(Intercept)' = 'Constant')
	raw <- gtsummary(models,
		   coef_map = cm,
 		   stars = TRUE,
 		   gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
		   title = 'Summarizing 5 statistical models using the `gtsummary` package for `R`.',
		   subtitle = 'Models estimated using the Guerry dataset.',
		   notes = c('First custom note to contain text.',
		             'Second custom note with different content.')) %>%
		   gt::tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
		   gt::tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
           gt::tab_spanner(label = 'Clergy', columns = 'Logit 1') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/complex_table.html")
})

test_that("html_output: uncertainty estimates", {
    raw <- gtsummary(models, statistic = 'std.error') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/std.error.html")

    raw <- gtsummary(models, statistic = 'p.value') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/p.value.html")

    raw <- gtsummary(models, statistic = 'statistic') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/statistic.html")

    raw <- gtsummary(models, statistic = 'conf.int', conf_level = .99) %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/conf.int.html")
})

test_that("html_output: title and subtitle", {
    raw <- gtsummary(models,
                     title = 'This is a title for my table.') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/title.html")
    raw <- gtsummary(models,
                     subtitle = 'And this is the subtitle.') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/subtitle.html")
    raw <- gtsummary(models,
                     title = 'This is a title for my table.',
                     subtitle = 'And this is the subtitle.') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/title_subtitle.html")
})

test_that("html_output: rounding", {
    raw <- gtsummary(models, fmt = '%.7f') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/rounding.html")
})

test_that("html_output: background color", {
    raw <- gtsummary(models) %>%
           tab_style(style = cells_styles(bkgd_color = "lightcyan",
                                          text_weight = "bold"),
                     locations = cells_data(columns = vars(`OLS 1`))) %>%
           tab_style(style = cells_styles(bkgd_color = "#F9E3D6",
                                          text_style = "italic"),
                     locations = cells_data(columns = vars(`NBin 2`),
                                            rows = 2:6)) %>%
           as_raw_html()
    expect_known_output(cat(raw), "known_output/background_color.html")
})
