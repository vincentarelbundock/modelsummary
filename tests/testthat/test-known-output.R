context("Test known output")

if ('gt' %in% rownames(utils::installed.packages())) {

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

    test_that("html_output: coef_omit", {
        raw <- msummary(models, coef_omit = 'Crime_prop|Infants|Donations', title = 'coef_omit expect intercept only') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/coef_omit.html")
    })

    test_that("html_output: complex table", {
        cm <- c('Crime_prop' = 'Crime / Population',
            'Donations' = 'Donations',
            'Infants' = 'Infants',
            '(Intercept)' = 'Constant')
        raw <- msummary(models,
               coef_map = cm,
               stars = TRUE,
               gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
               title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
               subtitle = 'Models estimated using the Guerry dataset.',
               notes = c('First custom note to contain text.',
                         'Second custom note with different content.')) %>%
               gt::tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
               gt::tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
               gt::tab_spanner(label = 'Clergy', columns = 'Logit 1') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/complex_table.html")
    })

    test_that("html_output: coef_map -- collapse coefficients", {
        set.seed(1024)
        x <- rnorm(100)
        y <- rnorm(100)
        z <- rnorm(100)
        mod <- list()
        mod[[1]] <- lm(y ~ x)
        mod[[2]] <- lm(y ~ z)
        raw <- msummary(mod, coef_map = c('x' = 'single variable', 'z' = 'single variable')) %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/coef_map_collapse_coefficients.html")
    })

    test_that("html_output: significance stars", {
        raw <- msummary(models, stars =TRUE) %>% 
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/stars_default.html")

        raw <- msummary(models, stars = c('+' = .8, '*' = .1)) %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/stars_custom.html")
    })

    test_that("html_output: uncertainty estimates", {
        raw <- msummary(models, statistic = 'std.error',
                         title = 'statistic = standard errors') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/std.error.html")

        raw <- msummary(models, statistic = 'p.value',
                         title = 'statistic = p.value') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/p.value.html")

        raw <- msummary(models, statistic = 'statistic',
                         title = 'statistic = statistic') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/statistic.html")

        raw <- msummary(models, statistic = 'conf.int', conf_level = .99,
                         title = 'statistic = conf.int, conf_level = .99') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/conf.int.html")

       
    })

    test_that("html_output: statistic_override", {

        models <- list()
        models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
        models[['NBin 1']] <- glm.nb(Literacy ~ Crime_prop + Donations, dat)
        models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
        models[['NBin 2']] <- glm.nb(Desertion ~ Crime_prop + Donations, dat)
        models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())


        raw <- msummary(models, statistic_override = sandwich::vcovHC, statistic = 'p.value',
                         title = 'statistic_override = sandwich::vcovHC, statistic = p.value') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), 'known_output/statistic_override_1.html')

        raw <- msummary(models, 
                         statistic_override = list(vcov, vcovHC, vcovHAC, vcovHC, vcov),
                         title = 'statistic_override = list of functions') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), 'known_output/statistic_override_2.html')

        vcov_matrices <- lapply(models, vcovHC)
        raw <- msummary(models, statistic_override = vcov_matrices,
                         title = 'statistic_override = vcov_matrices') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), 'known_output/statistic_override_3.html')

        custom_stats <- list(`OLS 1` = c('(Intercept)' = 2, Crime_prop = 3, Infants = 4), 
                             `NBin 1` = c('(Intercept)' = 3, Crime_prop = -5, Donations = 3),
                             `OLS 2` = c('(Intercept)' = 7, Crime_prop = -6, Infants = 9), 
                             `NBin 2` = c('(Intercept)' = 4, Crime_prop = -7, Donations = -9),
                             `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2))
        raw <- msummary(models, statistic_override = custom_stats,
                         title = 'statistic_override = list of vectors') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), 'known_output/statistic_override_4.html')
    })

    test_that("html_output: title and subtitle", {

        raw <- msummary(models,
                         title = 'This is a title for my table.') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/title.html")

        raw <- msummary(models,
                         title = 'This is a title for my table.',
                         subtitle = 'And this is the subtitle.') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/title_subtitle.html")

        expect_error(msummary(models, subtitle = 'And this is the subtitle.'))
    })

    test_that("html_output: rounding", {
        raw <- msummary(models, fmt = '%.7f',
                         title = 'rounding') %>%
               gt::as_raw_html()
        expect_known_output(cat(raw), "known_output/rounding.html")
    })

    test_that("html_output: background color", {

        raw <- msummary(models, title = 'colors') %>%
               tab_style(style = cell_text(color = "lightcyan", weight = "bold"),
                         locations = cells_body(columns = vars(`OLS 1`))) %>%
               tab_style(style = cell_text(color = "#F9E3D6", style = "italic"),
                         locations = cells_body(columns = vars(`NBin 2`), rows = 2:6)) %>%
               as_raw_html()
        expect_known_output(cat(raw), "known_output/background_color.html")

    })

}
