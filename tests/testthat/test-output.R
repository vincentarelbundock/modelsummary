context("output types")

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}


library(modelsummary)

mod <- list()
mod[[1]] <- lm(hp ~ mpg, mtcars)
mod[[2]] <- lm(hp ~ mpg + drat, mtcars)


test_that("table objects from different packages", {
  expect_error(msummary(mod), NA)
  expect_error(msummary(mod, 'gt'), NA)
  expect_error(msummary(mod, 'kableExtra'), NA)
  expect_error(msummary(mod, 'flextable'), NA)
  expect_error(msummary(mod, 'huxtable'), NA)
})

test_that("simple code", {
  expect_error(msummary(mod, 'html'), NA)
  expect_error(msummary(mod, 'latex'), NA)
  expect_error(msummary(mod, 'markdown'), NA)
})

test_that("unsupported output", {
  expect_error(msummary(mod, 'bad'))
  expect_error(msummary(mod, 'htm'))
  expect_error(msummary(mod, 't.est'))
})

test_that("supported global options", {

  random <- random_string()

  # RTF
  filename <- paste0(random, '.rtf')
  options(modelsummary_rtf = 'gt')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  # HTML
  filename <- paste0(random, '.html')
  options(modelsummary_html = 'gt')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'kableExtra')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'flextable')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'huxtable')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

})

test_that("unsupported global options", {

  options(modelsummary_rtf = 'kableExtra')
  expect_error(msummary(mod, 'test.rtf'))
  options(modelsummary_rtf = 'gt')

  options(modelsummary_word = 'kableExtra')
  expect_error(msummary(mod, 'test.docx'))
  options(modelsummary_word = 'gt')
  expect_error(msummary(mod, 'test.docx'))
  options(modelsummary_word = 'flextable')

  options(modelsummary_powerpoint = 'kableExtra')
  expect_error(msummary(mod, 'test.pptx'))
  options(modelsummary_powerpoint = 'gt')
  expect_error(msummary(mod, 'test.pptx'))
  options(modelsummary_powerpoint = 'flextable')

  options(modelsummary_png = 'huxtable')
  expect_error(msummary(mod, 'test.png'))
  unlink("test.png")
  options(modelsummary_png = 'flextable')

  options(modelsummary_jpg = 'huxtable')
  expect_error(msummary(mod, 'test.jpg'))
  options(modelsummary_jpg = 'gt')
  expect_error(msummary(mod, 'test.jpg'))
  options(modelsummary_jpg = 'flextable')

})

test_that("save to file", {

  random <- random_string()

  filename <- paste0(random, '.html')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.rtf')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.tex')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.md')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.docx')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.pptx')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.jpg')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.png')
  expect_error(msummary(mod, filename), NA)
  unlink(filename)
})


test_that("overwrite file", {

  random <- random_string()

  filename <- paste0(random, '.html')
  expect_error(msummary(mod, filename), NA)
  expect_error(msummary(mod, filename), NA)
  unlink(filename)

})
