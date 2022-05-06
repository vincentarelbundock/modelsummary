penguins <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")

test_that("regression test: gt duplicate span labels", {
    skip_if_not_installed("gt", minimum_version = "0.5.0")
    expect_error(datasummary(All(mtcars) ~ Factor(vs) * (Factor(am) * (Mean + SD)),
                             data = mtcars,
                             output = "gt"),
                NA)
})


test_that("tables::All() warning does not accept tibbles yet", {
  # https://github.com/vincentarelbundock/modelsummary/issues/395
  requiet("tibble")
  dat <- tibble::as_tibble(mtcars)
  expect_warning(datasummary(All(dat) ~ Mean + SD, data = dat), regexp = "does not accept tibbles")
  expect_warning(datasummary(All(mtcars) ~ Mean + SD, data = mtcars), NA)
})


test_that("big.mark formatting", {
            set.seed(10)
            rock <- data.frame(
                               area = runif(100, 1000, 10000),
                               shape = runif(100, 0, 1))
            f <- area * Arguments(fmt = 1, big.mark = ",") +
              shape * Arguments(fmt = 3) ~
              Min + Mean + Max
            expect_snapshot(datasummary(f, data = rock, output = "data.frame"))
})

test_that("cannot nest two continuous variables", {
  expect_error(suppressMessages(datasummary(mpg * hp ~ Mean + SD, mtcars)))
})

test_that('numeric content of simple tables', {

  # 2 rows 2 cols
  truth <- data.frame(c('mpg', 'hp'),
    c("20.09", "146.69"),
    c("6.03", "68.56"))
  tab <- datasummary(mpg + hp ~ mean + sd,
    output = 'dataframe',
    data = mtcars)
  expect_true(all(truth == tab))

  # nested cols: 1 level
  truth <- data.frame(rows = c('mpg', 'hp'),
    `0 mean` = c("17.15", "160.26"),
    `0 sd` = c("3.83", "53.91"),
    `1 mean` = c("24.39", "126.85"),
    `1 sd` = c("6.17", "84.06"))
  tab <- datasummary(mpg + hp ~ Factor(am) * (mean + sd),
    output = 'dataframe',
    data = mtcars,
    fmt = "%.2f")
  expect_true(all(truth == tab))

  # nested cols: 2 levels
  tab <- datasummary(mpg + hp ~ Factor(cyl) * Factor(am) * (mean + sd),
    output = 'dataframe',
    data = mtcars)
  expect_equal(dim(tab), c(2, 13))

  # nested rows: 1 level
  truth <- data.frame(am = c("0", "", "1", ""),
    b = c("mpg", "hp", "mpg", "hp"),
    mean = c("17.15", "160.26", "24.39", "126.85"),
    sd = c("3.83", "53.91", "6.17", "84.06"))
  tab <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd,
    output = 'dataframe',
    data = mtcars,
    fmt = "%.2f")
  expect_true(all(truth == tab))

  # nested rows: 2 levels
  tab <- datasummary(Factor(cyl) * Factor(am) * mpg + hp ~ mean + sd,
    output = 'dataframe',
    data = mtcars)
  expect_equal(dim(tab), c(7, 5))

})

test_that('Header carry-forward', {
  coln <- c(" ", "4 / mean", "4 / sd", "6 / mean", "6 / sd", "8 / mean", "8 / sd", "median")
  tab <- datasummary(mpg + hp ~ Factor(cyl) * (mean + sd) + median,
    data = mtcars,
    output = 'dataframe')
  expect_equal(colnames(tab), coln)
})

test_that('Factor() is equivalent to assign', {
  tab1 <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd,
    output = 'dataframe',
    data = mtcars)
  tmp <- mtcars
  tmp$am <- factor(tmp$am)
  tab2 <- datasummary(am * (mpg + hp) ~ mean + sd,
    output = 'dataframe',
    data = tmp)
  expect_identical(tab1, tab2)
})


test_that('logical and characters converted to factors automatically', {
  tmp <- mtcars
  tmp$am <- ifelse(tmp$am == 0, FALSE, TRUE)
  tmp$cyl <- as.character(tmp$cyl)
  tab <- datasummary(cyl * am * (mpg + hp) ~ mean + sd,
    output = 'dataframe',
    data = tmp)
  expect_equal(dim(tab), c(12, 5))
})

test_that('datasummary: output format do not produce errors', {
  requiet("huxtable")
  requiet("kableExtra")
  # output formats do not produce errors
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'huxtable'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'kableExtra'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'huxtable'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'dataframe'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'markdown'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'latex'), NA)
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'html'), NA)
})

test_that('datasummary: output format do not produce errors (flextable)', {
  requiet("flextable")
  expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output = 'flextable'), NA)
})

test_that('failure after move from tables Depends to re-export', {
  tmp <- datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
    output = "data.frame",
    data = penguins)
  expect_equal(dim(tmp), c(2, 3))
})


test_that('datasummary: return numeric values', {
  tmp <- datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
    output = "data.frame",
    fmt = NULL,
    data = penguins)
  expect_true(is.numeric(tmp$female))
  expect_true(is.numeric(tmp$male))
})


test_that("labelled and All warning: Issue #433", {
    x <- structure(list(working_hours = structure(c( 28, 40, 11, NA, 40, 38, 10, 40, 43, 0), label = "Total hours normally worked per week in main job overtime included", format.stata = "%14.0g", labels = c( `Not applicable` = NA_real_, Refusal = NA_real_, `Don't know` = NA_real_, `No answer` = NA_real_), class = c("haven_labelled", "vctrs_vctr", "double")), age = structure(c( 53, 49, 16, 75, 41, 49, 64, 80, 62, 50), label = "Age of respondent, calculated", format.stata = "%13.0g", labels = c(`Not available` = NA_real_), class = c( "haven_labelled", "vctrs_vctr", "double")), group = structure(c( 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor")), class = "data.frame", row.names = c( NA, -10L))
    expect_warning(expect_error(datasummary_balance(~group, x)), regexp = "not safe")
})


test_that("regression test", {
    url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
    penguins <- read.csv(url)
    expect_error(datasummary(island * species * body_mass_g ~ sex * (Mean + SD),
                             data = penguins),
                 NA)
    expect_error(datasummary(
                     island * species * body_mass_g ~ sex * (Mean + SD) * DropEmpty(),
                     data = penguins),
                 NA)
})

