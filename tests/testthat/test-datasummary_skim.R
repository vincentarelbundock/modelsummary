dat <- mtcars
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)

penguins <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")

test_that("basic", {

  tmp <- expect_warning(datasummary_skim(dat, output="data.frame"))
  expect_equal(dim(tmp), c(9, 8))

  tmp <- datasummary_skim(dat, type="categorical", output="data.frame")
  expect_equal(dim(tmp), c(5, 4))

})

test_that("display NA in categorical", {
    tmp <- mtcars
    tmp$vs <- as.logical(tmp$vs)
    tmp$vs[1:3] <- NA
    
    tab <- datasummary_skim(tmp, "categorical", output = "data.frame")
    expect_equal(nrow(tab), 3)
    
    tmp$vs <- factor(tmp$vs)
    tab <- datasummary_skim(tmp, "categorical", output = "data.frame")
    expect_equal(nrow(tab), 2)
})


test_that("simple categorical", {
  tab <- datasummary_skim(dat, type = "categorical", output = "data.frame")
  expect_s3_class(tab, "data.frame")
  expect_equal(dim(tab), c(5, 4))
})

test_that("errors and warnings: numeric", {

  # histograms only possible in default, html, or kableExtra
  expect_warning(datasummary_skim(dat, output="markdown"),
                 regexp="histogram argument")

  # no numeric variables
  tmp <- data.frame(a = sample(letters, 20),
                    b = as.character(sample(1:2000, 20, replace=TRUE)))
  expect_error(datasummary_skim(tmp),
               regexp="no numeric")

  # all fully missing
  tmp <- data.frame(a = rep(NA_real_, 20),
                    b = rep(NA_character_, 20))
  expect_error(datasummary_skim(tmp),
               regexp="missing")

  # too many columns
  tmp <- data.frame(matrix(rnorm(52*3), ncol = 52))
  expect_error(datasummary_skim(tmp),
               regexp = "more than 50 variables")
})

test_that("errors and warnings: categorical", {

  # no categorical
  tmp <- data.frame(a = rnorm(10))
  expect_error(datasummary_skim(tmp, "categorical"),
               regexp = "contains no logical")

  # too many columns
  tmp <- data.frame(matrix(as.character(rnorm(52*3)), ncol = 52))
  expect_error(datasummary_skim(tmp, "categorical"),
               regexp = "more than 50 variables")

})

# # should be fine, but R-CMD-check on github breaks 
# test_that("tibble input does not error", {
#   dat <- tibble::as_tibble(penguins)
#   expect_error(datasummary_skim(dat), NA)
# })


test_that("fmt", {
  known <- c("33.9000", "8.0000", "472.0000", "335.0000", "4.9300", "5.4240", "22.9000", "1.0000", "8.0000")
  tmp <- datasummary_skim(dat, output="data.frame", fmt="%.4f", histogram=FALSE)
  expect_equal(tmp$Max, known)

  known <- c("56.2500", "43.7500", "46.8750", "37.5000", "15.6250")
  tmp <- datasummary_skim(dat, output="data.frame", fmt="%.4f", type="categorical")
  expect_equal(tmp[[4]], known)
})


test_that("empty string factor level (tricky for tables::tabular)", {
  tmp <- data.frame(a = c(rep("", 3), rep("b", 5), rep("c", 10)))
  tmp <- datasummary_skim(tmp, "categorical", output="data.frame")
  expect_equal(dim(tmp), c(3, 3))
})


test_that("too many factor levels", {
  N <- 20000
  tmp <- data.frame(a = sample(letters, N, replace = TRUE),
                    b = as.character(sample(1:100, N, replace = TRUE)))
  tmp <- expect_warning(datasummary_skim(tmp, type="categorical", output="data.frame"))
  expect_equal(ncol(tmp), 3)
})


test_that("completely missing variables are dropped", {
  tmp <- dat
  tmp$junk <- rep(NA_character_, nrow(dat))
  tmp <- expect_warning(datasummary_skim(tmp, type="categorical", output="data.frame", histogram=FALSE))
  expect_false("junk" %in% tmp[[1]])
})


test_that("simple dataset", {
  tab <- datasummary_skim(mtcars, type="dataset", output="data.frame")
  expect_equal(dim(tab), c(4, 2))
})


# # RDatasets tests: must be commented out
# any_categorical <- function(x) {
#   # datasummary_skim ignores characters with more than 10 levels
#   for (n in colnames(x)) {
#     if (is.character(x[[n]]) | is.logical(x[[n]])) {
#       bad1 <- length(unique(x[[n]])) > 20
#       bad2 <- all(is.na(x[[n]]))
#       flag <- bad1 | bad2
#       if (flag) {
#         x[[n]] <- NULL
#       }
#     }
#   }
#   idx <- sapply(x, function(x) 
#                 is.factor(x) | is.logical(x) | is.character(x))
#   any(idx) & (ncol(x) < 40)
# }
# any_numeric <- function(x) {
#   any(sapply(x, function(x) is.numeric(x))) & (ncol(x) < 51)
# }
# csv_files <- Sys.glob("~/Downloads/Rdatasets/csv/*/*.csv")
# for (f in csv_files) {
#   dat <- read.csv(f)
#   if (any_numeric(dat)) {
#     test_that(paste("numeric:", f), {
#       expect_error(datasummary_skim(dat, type="numeric"), NA)
#     })
#   }
#   if (any_categorical(dat)) {
#     test_that(paste("categorical:", f), {
#       expect_error(datasummary_skim(dat, type="categorical"), NA)
#     })
#   }
# }
