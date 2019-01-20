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

test_that("html output", {
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
