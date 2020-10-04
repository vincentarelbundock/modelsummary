library(modelsummary)
library(estimatr)

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}


context('datasummary_balance')

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
})

test_that('fmt', {
  tmp <- mtcars[, c('am', 'mpg', 'gear')]
  tmp$gear <- factor(tmp$gear)
  truth <- c("17.15", "N", "15", "4", "0")
  tab <- datasummary_balance(~am, tmp, fmt = "%.2f", output = 'dataframe')
  expect_equal(tab[[3]], truth)
})

test_that('too many factor levels', {
  set.seed(10)
  dat <- data.frame(ID = as.character(1:100),
    Y = rnorm(100),
    Z_comp = sample(0:20, 100, replace = TRUE))
  expect_error(datasummary_balance(~Z_comp, dat))
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

  truth <- difference_in_means(Y ~ Z_clust, clusters = clusters, data = dat)
  truth <- estimatr::tidy(truth)

  tab <- datasummary_balance(~Z_clust, dat, fmt = "%.6f", output = 'dataframe')
  expect_equal(tab[1, ncol(tab)], sprintf("%.6f", truth$std.error))

  # blocks
  dat$block <- rep(1:5, each = 20) # hardcoded name in estimatr
  dat <- dat %>%
    dplyr::group_by(block) %>%
    dplyr::mutate(Z_block = rbinom(dplyr::n(), 1, .5))
  dat$blocks <- dat$block # hardcoded name in datasummary_balance
  dat$clusters <- NULL

  truth <- difference_in_means(Y ~ Z_block, blocks = block, data = dat)
  truth <- sprintf("%.6f", tidy(truth)$std.error)

  tab <- datasummary_balance(~Z_block, dat, fmt = "%.6f", output = 'dataframe')
  expect_equal(tab[1, ncol(tab)], truth)

})


test_that('works with tibbles', {
  res <- dplyr::starwars %>%
    dplyr::filter(species == 'Human') %>%
    dplyr::select(height:gender) %>%
    datasummary_balance(~gender, data = ., output = "data.frame")
  expect_equal(dim(res), c(28, 8))
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
