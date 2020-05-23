context("kableExtra")

library(MASS)
library(dplyr)
library(modelsummary)
library(kableExtra)

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['NBin 1']] <- glm.nb(hp ~ mpg + drat, mtcars)
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())

test_that('kable: bad arguments', {

    expect_error(msummary(models, output = 'markdown', title = 'test title',
                          subtitle = 'test'))

})

test_that("kable markdown: complex table", {

    cm <- c('hp' = 'Horsepower',
            'mpg' = 'Miles/Gallon',
            'wt' = 'Weight',
            'drat' = 'Rear axle ratio',
            'disp' = 'Displacement',
            '(Intercept)' = 'Constant')
    tab <- msummary(models,
           coef_map = cm,
           stars = TRUE,
           gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
           title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
           notes = c('First custom note to contain text.',
                     'Second custom note with different content.'),
           output = 'markdown')

    expect_known_output(tab, 'known_output/kableExtra_markdown_complex.md')

})

test_that("kable markdown: rouding + custom stars", {

    tab <- msummary(models,
           stars = c('+' = .1, '*' = .01),
           fmt = '%.8f',
           output = 'markdown')

    expect_known_output(tab, 'known_output/kableExtra_markdown_rounding_stars.md')
    
})

