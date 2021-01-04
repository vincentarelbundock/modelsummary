skip_if_not_installed("estimatr")

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}

test_that("errors and warnings", {

  # missing RHS variable
  tmp <- data.frame(ID = as.character(1:100),
    Y = rnorm(100),
    Z_comp = sample(0:20, 100, replace = TRUE))
  expect_error(datasummary_balance(~k, tmp),
               regexp = "must be in data")

  # too many factor levels in condition variable
  expect_error(datasummary_balance(~Z_comp, tmp),
               regexp = "wide to be readable")

  # warn about drops
  tmp <- data.frame(
    Y = rnorm(100),
    K = NA_real_,
    Z_comp = sample(0:1, 100, replace = TRUE))
  expect_warning(datasummary_balance(~Z_comp, tmp),
                 regexp = "entirely missing")

  tmp <- data.frame(
    Y = rnorm(100),
    K = as.character(1:100),
    Z_comp = sample(0:1, 100, replace = TRUE))
  expect_warning(datasummary_balance(~Z_comp, tmp),
                 regexp = "include more than 50")
})


test_that("column percentages sum to 100 within factors", {
  dat <- mtcars[, c("vs", "cyl", "gear")]
  dat$cyl <- factor(dat$cyl)
  dat$gear <- factor(dat$gear)
  tab <- datasummary_balance(~vs, dat, output="dataframe")
  idx <- c(rep("cyl", 3), rep("gear", 3))
  f <- function(x) round(sum(x)) == 100
  expect_true(all(tapply(as.numeric(tab[[4]]), idx, f)))
  expect_true(all(tapply(as.numeric(tab[[6]]), idx, f)))
})


test_that("palmer penguins was once broken with kableExtra", {
  penguins <- "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv"
  penguins <- read.csv(penguins)
  raw <- datasummary_balance(~sex, penguins, output="html")
  expect_known_output(cat(raw), "known_output/datasummary_balance_penguins.html", update=FALSE)
})


test_that('variable name with spaces', {
  tmp <- mtcars
  colnames(tmp)[1] <- "testing spaces"
  tab <- expect_error(datasummary_balance(~vs, data=tmp, output="dataframe"), NA)
  expect_equal(dim(tab), c(10, 7))
})


test_that('add column', {
  tmp <- mtcars
  tmp$gear <- as.factor(tmp$gear)
  tmp$vs <- as.logical(tmp$vs)
  tab <- datasummary_balance(~vs, 
    data=tmp, 
    output='dataframe',
    add_columns=data.frame(k=letters[1:13]),
                           z=1:13)
  expect_equal(dim(tab), c(13, 9))
})


test_that('only numeric', {
  tab <- datasummary_balance(~vs, mtcars, output = 'dataframe')
  truth <- c(" ", "0 Mean", "0 Std. Dev.", "1 Mean",
    "1 Std. Dev.", "Diff. in Means", "Std. Error")
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(10, 7))
  expect_equal(colnames(tab), truth)
})


test_that('only factors', {
  tmp <- mtcars
  tmp$cyl <- factor(tmp$cyl)
  tmp$gear <- factor(tmp$gear)
  tmp$vs <- as.logical(tmp$vs)
  tmp <- tmp[, c('am', 'vs', 'cyl', 'gear')]
  tab <- datasummary_balance(~am, tmp, output = 'dataframe')
  truth <- c(" ", "  ", "0 N", "0 %", "1 N", "1 %")
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(8, 6))
  expect_equal(colnames(tab), truth)
})


test_that('both factors and numerics', {
  tmp <- mtcars
  tmp$cyl <- factor(tmp$cyl)
  tmp$gear <- factor(tmp$gear)
  tmp$vs <- as.logical(tmp$vs)
  tab <- datasummary_balance(~am, tmp, output = 'dataframe')
  truth <- c(" ", "  ", "0 Mean", "0 Std. Dev.", "1 Mean",
    "1 Std. Dev.", "Diff. in Means", "Std. Error")
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(16, 8))
  expect_equal(colnames(tab), truth)
})


test_that('more than two conditions', {
  tmp <- mtcars
  tmp$cyl <- factor(tmp$cyl)
  tmp$vs <- as.logical(tmp$vs)
  tab <- datasummary_balance(~gear, tmp, output = 'dataframe', dinm = FALSE)
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(14, 8))
  tab <- expect_warning(datasummary_balance(~gear, tmp, output = 'dataframe', dinm = TRUE))
  expect_equal(dim(tab), c(14, 8))
})


test_that('single numeric', {
  tmp <- mtcars[, c('am', 'mpg')]
  tab <- datasummary_balance(~am, data = tmp, output = 'dataframe')
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(1, 7))
  expect_equal(tab[[1]], 'mpg')
})


test_that('single factor', {
  tmp <- mtcars[, c('am', 'gear')]
  tmp$gear <- factor(tmp$gear)
  tab <- datasummary_balance(~am, data = tmp, output = 'dataframe')
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(3, 6))
  expect_equal(tab[[2]][1], '3')
})


test_that('dinm=FALSE', {
  tab <- datasummary_balance(~vs, mtcars, dinm = FALSE, output = 'dataframe')
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(10, 5))
  expect_equal(tab[[1]][1], 'mpg')
})


test_that('dinm_statistic = "p.value"', {
  tab <- datasummary_balance(~vs, mtcars, dinm_statistic = 'p.value',
    output = 'dataframe')
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(10, 7))
  expect_equal(tab[[1]][1], 'mpg')
  expect_equal(colnames(tab)[ncol(tab)], 'p')

  # sanity checks prevent other than p.value or std.error
  expect_error(
    datasummary_balance(~vs, mtcars, dinm_statistic = 'bad', output = 'dataframe'))
})


test_that('fmt', {
  tmp <- mtcars[, c('am', 'mpg', 'gear')]
  tmp$gear <- factor(tmp$gear)
  truth <- c("17.15", "N", "15", "4", "0")
  tab <- datasummary_balance(~am, tmp, fmt = "%.2f", output = 'dataframe')
  expect_equal(tab[[3]], truth)
})




test_that('too many levels in row variable', {
  set.seed(10)
  dat <- data.frame(ID = as.character(1:100),
    Y = rnorm(100),
    Z_comp = sample(0:1, 100, replace = TRUE))
  expect_warning(datasummary_balance(~Z_comp, dat))
})


test_that('estimatr: clusters, blocks, weights', {

  set.seed(286342)
  # clusters
  dat <- data.frame(ID = as.character(1:100),
    Y = rnorm(100),
    Z_comp = sample(0:1, 100, replace = TRUE))
  dat$clusters <- sample(20, size = nrow(dat), replace = TRUE)
  idx <- sample(unique(dat$clusters), 12)
  dat$Z_clust <- as.numeric(dat$clusters %in% idx)
  dat$ID <- NULL

  truth <- estimatr::difference_in_means(Y ~ Z_clust, clusters = clusters, data = dat)
  truth <- estimatr::tidy(truth)

  tab <- datasummary_balance(~Z_clust, dat, fmt = "%.6f", output = 'dataframe')
  expect_equal(tab[1, ncol(tab)], sprintf("%.6f", truth$std.error))

  # blocks
  dat$block <- rep(1:5, each = 20) # hardcoded name in estimatr

  dat$Z_block <- unlist(tapply(dat$block, dat$block, function(z) rbinom(length(z), 1, .5)))

  dat$blocks <- dat$block # hardcoded name in datasummary_balance
  dat$clusters <- NULL

  truth <- estimatr::difference_in_means(Y ~ Z_block, blocks = block, data = dat)
  truth <- sprintf("%.6f", tidy(truth)$std.error)

  tab <- datasummary_balance(~Z_block, dat, fmt = "%.6f", output = 'dataframe')
  expect_equal(tab[1, ncol(tab)], truth)

})


######################
#  various datasets  #
######################
test_that('datasummary_balance: various datasets', {
  data(PlantGrowth)
  tab <- datasummary_balance(~group, PlantGrowth, output = 'dataframe', dinm = FALSE)
  expect_equal(tab[1, 2], '5.0')
  expect_equal(tab[1, 3], '0.6')
  expect_equal(tab[1, 4], '4.7')
  expect_equal(tab[1, 5], '0.8')
  expect_equal(tab[1, 6], '5.5')
  expect_equal(tab[1, 7], '0.4')
})
