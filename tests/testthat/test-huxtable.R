# flextable is not installed on CRAN's M1 machine
skip_if_not_installed("flextable")

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['Poisson 1']] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())

test_that("markdown caption and notes", {
  # minor UTF8 encoding issue on CRAN and Windows
  skip_on_os("windows")
  skip_on_cran()
  unknown <- expect_warning(
    modelsummary(models, "huxtable", title = "test title", notes = "test note",
      stars = TRUE) %>%
      huxtable::to_md())
  expect_known_output(cat(unknown),
                      "known_output/huxtable-title-notes.md", update = FALSE)
})


test_that("save to file", {

  options(modelsummary_html = 'huxtable')
  options(modelsummary_rtf = 'huxtable')
  options(modelsummary_latex = 'huxtable')
  options(modelsummary_word = 'huxtable')
  options(modelsummary_powerpoint = 'huxtable')

  random <- random_string()

  filename <- paste0(random, '.html')
  expect_error(modelsummary(models, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.rtf')
  expect_error(modelsummary(models, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.tex')
  expect_error(modelsummary(models, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.docx')
  expect_error(modelsummary(models, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.pptx')
  expect_error(modelsummary(models, filename), NA)
  unlink(filename)

  options(modelsummary_html = 'kableExtra')
  options(modelsummary_rtf = 'gt')
  options(modelsummary_latex = 'kableExtra')
  options(modelsummary_word = 'flextable')
  options(modelsummary_powerpoint = 'flextable')

})
