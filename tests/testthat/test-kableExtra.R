models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['Poisson 1']] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())

test_that("knitr::kable_latex ignores bad arguments passed through ...", {
  expect_error(modelsummary(models, output="latex", badarg=TRUE), NA)
})


test_that('output="html" returns raw html', {
    tab <- modelsummary(models, output = "html")
    expect_identical(class(tab), c("modelsummary_string", "kableExtra", "knitr_kable"))
})

test_that("kable markdown: complex table", {

  cm <- c(
    'hp' = 'Horsepower',
    'mpg' = 'Miles/Gallon',
    'wt' = 'Weight',
    'drat' = 'Rear axle ratio',
    'disp' = 'Displacement',
    '(Intercept)' = 'Constant')

  expect_known_output(
    modelsummary(
      models,
      coef_map = cm,
      stars = TRUE,
      gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
      title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
      notes = c('First custom note to contain text.',
        'Second custom note with different content.'),
      output = 'markdown'),
    print = TRUE,
    file = "known_output/kableExtra_markdown_complex.md",
    update = FALSE)

})

test_that("kable markdown: rouding + custom stars", {

  expect_known_output(
    modelsummary(
      models,
      stars = c('+' = .1, '*' = .01),
      fmt = '%.8f',
      output = 'markdown'),
    print = TRUE,
    file = "known_output/kableExtra_markdown_rounding_stars.md",
    update = FALSE)

})
