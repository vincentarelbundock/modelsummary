library(modelsummary)
context('datasummary_skim')

test_that('datasummary_skim type', {

  tmp_num <- mtcars[, c('mpg', 'hp')]
  tmp_cat <- mtcars[, c('cyl', 'am')]
  tmp_cat$cyl <- as.factor(tmp_cat$cyl)
  tmp_cat$am <- as.logical(tmp_cat$am)

  expect_error(datasummary_skim(tmp_num, type = 'categorical'))
  expect_error(datasummary_skim(tmp_cat, type = 'categorical'), NA)
  expect_error(datasummary_skim(tmp_num, type = 'numeric'), NA)
  expect_error(datasummary_skim(tmp_cat, type = 'numeric'))

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
