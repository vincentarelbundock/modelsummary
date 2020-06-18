library(modelsummary)

test_that("warning when trying to nest character or logical variables", {

dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.character(dat$cyl)

expect_warning(datasummary(disp + drat ~ mean, data = dat), NA)
expect_warning(datasummary(disp + drat ~ am * mean, data = dat))
expect_error(expect_warning(datasummary(disp + drat ~ cyl * mean, data = dat)))

})
