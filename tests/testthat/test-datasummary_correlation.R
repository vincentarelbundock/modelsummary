library(modelsummary)
context('datasummary_correlation')

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}


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
