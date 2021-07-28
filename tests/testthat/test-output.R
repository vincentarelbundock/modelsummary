# CRAN fails on artefacts
skip_on_cran()
skip_if_not_installed("estimatr")
skip_if_not_installed("flextable")

random_string <- function() {
  paste(sample(letters, 30, replace = TRUE), collapse = "")
}

mod <- list(
  lm(hp ~ mpg, mtcars),
  lm(hp ~ mpg + drat, mtcars))

test_that("output='table.tex'", {
  fn <- paste0(random_string(), ".tex")
  modelsummary(mod, output = fn)
  known <- readLines("known_output/output_1.tex")
  unknown <- readLines(fn)
  expect_identical(known, unknown)
  unlink(fn)
})


test_that("table objects from different packages", {
  expect_error(modelsummary(mod), NA)
  expect_error(modelsummary(mod, 'gt'), NA)
  expect_error(modelsummary(mod, 'kableExtra'), NA)
  expect_error(modelsummary(mod, 'flextable'), NA)
  expect_error(modelsummary(mod, 'huxtable'), NA)
})

test_that("simple code", {
  expect_error(modelsummary(mod, 'html'), NA)
  expect_error(modelsummary(mod, 'latex'), NA)
  expect_error(modelsummary(mod, 'markdown'), NA)
})

test_that("unsupported output", {
  expect_error(modelsummary(mod, 'bad'))
  expect_error(modelsummary(mod, 'htm'))
  expect_error(modelsummary(mod, 't.est'))
})

test_that("supported global options", {

  random <- random_string()

  # RTF
  filename <- paste0(random, '.rtf')
  options(modelsummary_rtf = 'gt')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  # HTML
  filename <- paste0(random, '.html')
  options(modelsummary_html = 'gt')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'kableExtra')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'flextable')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'huxtable')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)
  options(modelsummary_html = 'kableExtra')

})

test_that("unsupported global options", {

  options(modelsummary_rtf = 'kableExtra')
  expect_error(modelsummary(mod, 'test.rtf'))
  options(modelsummary_rtf = 'gt')

  options(modelsummary_word = 'kableExtra')
  expect_error(modelsummary(mod, 'test.docx'))
  options(modelsummary_word = 'gt')
  expect_error(modelsummary(mod, 'test.docx'))
  options(modelsummary_word = 'flextable')

  options(modelsummary_powerpoint = 'kableExtra')
  expect_error(modelsummary(mod, 'test.pptx'))
  options(modelsummary_powerpoint = 'gt')
  expect_error(modelsummary(mod, 'test.pptx'))
  options(modelsummary_powerpoint = 'flextable')

  options(modelsummary_png = 'huxtable')
  expect_error(modelsummary(mod, 'test.png'))
  unlink("test.png")
  options(modelsummary_png = 'kableExtra')

  options(modelsummary_jpg = 'huxtable')
  expect_error(modelsummary(mod, 'test.jpg'))
  options(modelsummary_jpg = 'gt')
  expect_error(modelsummary(mod, 'test.jpg'))
  options(modelsummary_jpg = 'kableExtra')

})

test_that("save to file", {

  random <- random_string()

  filename <- paste0(random, '.html')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.rtf')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.tex')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.md')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.docx')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.pptx')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.jpg')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

  filename <- paste0(random, '.png')
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)
})


test_that("overwrite file", {

  random <- random_string()

  filename <- paste0(random, '.html')
  expect_error(modelsummary(mod, filename), NA)
  expect_error(modelsummary(mod, filename), NA)
  unlink(filename)

})



######################################
context("output: datasummary_balance")
######################################

test_that('output formats do not produce errors', {
  tmp <- mtcars
  tmp$cyl <- as.character(tmp$cyl)
  tmp$vs <- as.logical(tmp$vs)
  expect_error(datasummary_balance(~am, tmp, output = 'huxtable'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'flextable'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'kableExtra'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'huxtable'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'dataframe'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'markdown'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'latex'), NA)
  expect_error(datasummary_balance(~am, tmp, output = 'html'), NA)
})

#  add_columns output formats work  #
tmp <- mtcars
tmp$cyl <- as.character(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
custom <- data.frame('a' = 1:2, 'b' = 1:2)
output_formats <- c('gt', 'kableExtra', 'flextable', 'huxtable', 'latex',
  'markdown', 'html')
for (o in output_formats) {
  testname <- paste('add_columns with', o)
  test_that(testname, {
    expect_warning(datasummary_balance(~am, tmp, add_columns = custom, output = o), NA)
  })
}

#  save to file  #
tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext) {
  msg <- paste('save to', ext)
  test_that(msg, {
    random <- random_string()
    filename <- paste0(random, ext)
    expect_error(datasummary_balance(~am, data = tmp, output = filename), NA)
    unlink(filename)
  })
}
for (ext in c('.html', '.tex', '.rtf', '.docx', '.pptx', '.jpg', '.png')) {
  save_to_file(ext)
}


###################################
context("output: datasummary_skim")
###################################

dat <- mtcars
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)

test_that("write to file", {

  expect_error(datasummary_skim(dat, output="test.jpg"), NA)
  expect_error(datasummary_skim(dat, type="categorical", output="test.jpg"), NA)
  expect_error(datasummary_skim(dat, output="test.png"), NA)
  expect_error(datasummary_skim(dat, type="categorical", output="test.png"), NA)
  expect_error(datasummary_skim(dat, output="test.html"), NA)
  expect_error(datasummary_skim(dat, type="categorical", output="test.html"), NA)
  expect_warning(datasummary_skim(dat, output="test.tex"))

  unlink("test.png")
  unlink("test.jpg")
  unlink("test.html")
  unlink("test.tex")

})

test_that("unsupported formats", {

  expect_warning(datasummary_skim(dat, output="flextable"))
  expect_warning(datasummary_skim(dat, output="flextable", histogram=FALSE), NA)

  expect_warning(datasummary_skim(dat, output="gt"))
  expect_warning(datasummary_skim(dat, output="gt", histogram=FALSE), NA)

  expect_warning(datasummary_skim(dat, output="huxtable"))
  expect_warning(datasummary_skim(dat, output="huxtable", histogram=FALSE), NA)

  expect_warning(datasummary_skim(dat, output="latex"))
  expect_warning(datasummary_skim(dat, output="latex", histogram=FALSE), NA)

})


##########################################
context("output: datasummary_correlation")
##########################################

test_that('output format do not produce errors', {

  # output formats do not produce errors
  expect_error(datasummary_correlation(mtcars, output = 'huxtable'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'flextable'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'huxtable'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'kableExtra'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'dataframe'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'markdown'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'latex'), NA)
  expect_error(datasummary_correlation(mtcars, output = 'html'), NA)

})

tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext = '.html') {
  msg <- paste('datasummary_correlation: save to', ext)
  test_that(msg, {
    random <- random_string()
    filename <- paste0(random, ext)
    expect_error(datasummary_correlation(tmp, output = filename), NA)
    unlink(filename)
  })

}

for (ext in c('.html', '.tex', '.rtf', '.docx', '.pptx', '.jpg', '.png')) {
  save_to_file(ext)
}
