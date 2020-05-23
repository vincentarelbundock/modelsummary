context("known output")

library(gt)
library(MASS)
library(dplyr)
library(sandwich)
library(modelsummary)

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['NBin 1']] <- glm.nb(hp ~ mpg + drat, mtcars)
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())


test_that("html_output: complex table", {

    cm <- c('hp' = 'Horsepower',
            'mpg' = 'Miles/Gallon',
            'wt' = 'Weight',
            'drat' = 'Rear axle ratio',
            'disp' = 'Displacement',
            '(Intercept)' = 'Constant')
    raw <- msummary(models,
           coef_map = cm,
           stars = TRUE,
           gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
           notes = c('First custom note to contain text.',
                     'Second custom note with different content.')) %>%
           gt::tab_spanner(label = 'Horsepower', columns = c('OLS 1', 'NBin 1')) %>%
           gt::tab_spanner(label = 'V-Shape', columns = c('OLS 2', 'Logit 1')) %>%
           gt::tab_spanner(label = 'Transmission', columns = 'Logit 2') %>%
           gt::tab_header(title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
                          subtitle = 'Models estimated using the mtcars dataset.') %>%
           gt::as_raw_html()

    expect_known_output(cat(raw), "known_output/complex_table.html")
})

test_that("html_output: title and subtitle", {

    raw <- msummary(models, title = 'This is a title for my table.') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/title.html")

    raw <- msummary(models) %>%
           gt::tab_header(title = 'This is a title for my table.',
                          subtitle = 'And this is the subtitle.') %>%
           gt::as_raw_html()
    expect_known_output(cat(raw), "known_output/title_subtitle.html")

    # subtitle deprecated
    expect_error(msummary(models, title = 'blah', subtitle = 'blahblah'))
    expect_error(msummary(models, subtitle = 'And this is the subtitle.'))

})

test_that("html_output: background color", {

    raw <- msummary(models, title = 'colors') %>%
           tab_style(style = cell_text(weight = "bold"),
                     locations = cells_body(columns = vars(`OLS 1`))) %>%
           tab_style(style = cell_text(style = "italic"),
                     locations = cells_body(columns = vars(`NBin 1`), rows = 2:6)) %>%
           tab_style(style = cell_fill(color = "lightcyan"),
                     locations = cells_body(columns = vars(`OLS 1`))) %>%
           tab_style(style = cell_fill(color = "#F9E3D6"),
                     locations = cells_body(columns = vars(`Logit 2`), rows = 2:6)) %>%
           as_raw_html()

    expect_known_output(cat(raw), "known_output/background_color.html")
})
