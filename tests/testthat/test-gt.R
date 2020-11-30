context("gt")

library(gt)
library(modelsummary)

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['Poisson 1']] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())


test_that("gof_omit='.*' used to produce an error", {

  mod <- lm(mpg ~ wt, mtcars)
  expect_error(modelsummary(mod, output = "gt", gof_omit = ".*"), NA)

})

test_that("complex html table", {

  cm <- c(
    'hp' = 'Horsepower',
    'mpg' = 'Miles/Gallon',
    'wt' = 'Weight',
    'drat' = 'Rear axle ratio',
    'disp' = 'Displacement',
    '(Intercept)' = 'Constant')

  raw <-
    modelsummary(
      models,
      output = "gt",
      coef_map = cm,
      stars = TRUE,
      gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
      notes = c('First custom note to contain text.',
        'Second custom note with different content.')
    ) %>%
    gt::tab_spanner(label = 'Horsepower', columns = c('OLS 1', 'Poisson 1')) %>%
    gt::tab_spanner(label = 'V-Shape', columns = c('OLS 2', 'Logit 1')) %>%
    gt::tab_spanner(label = 'Transmission', columns = 'Logit 2') %>%
    gt::tab_header(title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
      subtitle = 'Models estimated using the mtcars dataset.') %>%
    gt::as_raw_html()

  expect_known_output(cat(raw), "known_output/complex_table.html", update=FALSE)

})

test_that("title", {

  raw <- modelsummary(models, output = "gt", title = 'This is a title for my table.') %>%
    gt::as_raw_html()
  expect_known_output(cat(raw), "known_output/title.html", update=FALSE)

})

test_that("background color", {

  raw <- modelsummary(models, output = "gt", title = 'colors') %>%
    tab_style(style = cell_text(weight = "bold"),
      locations = cells_body(columns = vars(`OLS 1`))) %>%
    tab_style(style = cell_text(style = "italic"),
      locations = cells_body(columns = vars(`Poisson 1`), rows = 2:6)) %>%
    tab_style(style = cell_fill(color = "lightcyan"),
      locations = cells_body(columns = vars(`OLS 1`))) %>%
    tab_style(style = cell_fill(color = "#F9E3D6"),
      locations = cells_body(columns = vars(`Logit 2`), rows = 2:6)) %>%
    as_raw_html()

  expect_known_output(cat(raw), "known_output/background_color.html", update=FALSE)

})
