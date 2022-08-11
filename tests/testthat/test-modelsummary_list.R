
test_that("Issue 507: extraneous error about not supported", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    ml <- modelsummary(mod, output = "modelsummary_list")
    msg <- capture.output({tab <- modelsummary(ml, output = "data.frame")})
    expect_equal(length(msg), 0)
})


test_that('output="modelsummary_list" and back to data.frame', {
    mod <- list(
        lm(mpg ~ hp, mtcars),
        lm(mpg ~ hp + drat, mtcars))
    tab <- modelsummary(mod, "modelsummary_list")
    expect_true(all(sapply(tab, inherits, "modelsummary_list")))
    expect_true(class(tab) == "list")
    tab <- modelsummary(tab, "data.frame")
    expect_s3_class(tab, "data.frame")
    expect_equal(dim(tab), c(14, 5))
})


test_that("tidiers empty", {
  mod <- lm(mpg ~ hp + drat + vs, mtcars)
  ml <- list(tidy = modelsummary:::get_estimates(mod))
  class(ml) <- "modelsummary_list"
  gl <- generics::glance(ml)
  expect_s3_class(gl, "data.frame")
  expect_equal(dim(gl), c(1, 0))
  ml <- list(glance = modelsummary:::get_gof(mod))
  class(ml) <- "modelsummary_list"
  expect_error(tidy(ml))
})


# this is tricky because get_estimates doesn't always return predictable results depending on whether parameters, broom, or broom.mixed are loaded. this is a problem.

# test_that("tidiers", {
#   ml <- list(
#     glance = modelsummary::get_gof(mod),
#     tidy = modelsummary::get_estimates(mod))
#   class(ml) <- "modelsummary_list"
#   gl <- generics::glance(ml)
#   ti <- generics::tidy(ml)
#   expect_s3_class(gl, "data.frame")
#   expect_s3_class(ti, "data.frame")
#   # # broom
#   # expect_equal(dim(ti), c(4, 5))
#   # expect_equal(dim(gl), c(1, 12))
#   # easystats
#   expect_equal(dim(ti), c(4, 6))
#   expect_equal(dim(gl), c(1, 9))
# })


test_that("modelsummary: use variable labels by default", {
  data(trees)

  models_wo_labs <- list(
    "Bivariate" = lm(Girth ~ Height, data = trees),
    "Multivariate" = lm(Girth ~ Height + Volume, data = trees)
  )

  # give vars some random label
  trees$Height <- haven::labelled(trees$Height, label = "Height (in feet)")
  trees$Volume <- haven::labelled(trees$Volume, label = "Volume (in liters)")

  models_with_labs <- list(
    "Bivariate" = lm(Girth ~ Height, data = trees),
    "Multivariate" = lm(Girth ~ Height + Volume, data = trees)
  )
  with_labs <- modelsummary(models_with_labs, "dataframe")
  wo_labs <- modelsummary(models_wo_labs, "dataframe")

  # labs correctly applied
  expect_equal(
    unique(with_labs[1:6, "term"]),
    c("(Intercept)", "Height (in feet)", "Volume (in liters)")
  )

  # the rest of the table is unaffected
  expect_equal(with_labs[, -2], wo_labs[, -2])
})


test_that("modelsummary: don't use labels with 'coef_rename' and 'coef_map'", {
  data(trees)

  # give vars some random label
  trees$Height <- haven::labelled(trees$Height, label = "Height (in feet)")
  trees$Volume <- haven::labelled(trees$Volume, label = "Volume (in liters)")

  models <- list(
    "Bivariate" = lm(Girth ~ Height, data = trees),
    "Multivariate" = lm(Girth ~ Height + Volume, data = trees)
  )

  cr1 <- modelsummary(models, coef_rename = c('Volume' = 'Large', 'Height' = 'Tall'), output = "dataframe")
  cr2 <- modelsummary(models, coef_rename = toupper, output = "dataframe")
  cr3 <- modelsummary(models, coef_rename = coef_rename, output = "dataframe")

  expect_equal(
    unique(cr1[1:6, "term"]),
    c("(Intercept)", "Tall", "Large")
  )
  expect_equal(
    unique(cr2[1:6, "term"]),
    c("(INTERCEPT)", "HEIGHT", "VOLUME")
  )
  expect_equal(
    unique(cr3[1:6, "term"]),
    c("(Intercept)", "Height", "Volume")
  )

  cm1 <- modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'), output = "dataframe")
  cm2 <-modelsummary(models, coef_map = c('Volume', 'Height'), output = "dataframe")
  cm3 <- modelsummary(models, coef_rename = coef_rename, output = "dataframe")

  expect_equal(
    unique(cm1[1:4, "term"]),
    c("Large", "Tall")
  )
  expect_equal(
    unique(cm2[1:4, "term"]),
    c("Volume", "Height")
  )
})

#
# test_that("modelsummary: correctly applies the rest of formatting when using var labels", {
#   dat <- mtcars
#   dat$mpg <- haven::labelled(dat$mpg, label = "Miles per gallon")
#   dat$mpg2 <- haven::labelled(dat$mpg, label = "Miles per gallon")
#   dat$cyl <- as.factor(dat$cyl)
#   dat$cyl <- haven::labelled(dat$cyl, label = "Number of cylinders")
#
#   mod <- list(
#     lm(hp ~ mpg + drat:mpg + cyl, data = dat),
#     lm(hp ~ mpg2 * drat + cyl, data = dat)
#   )
#   out1 <- modelsummary(mod, "dataframe")
#   out2 <- modelsummary(mod, "dataframe", fmt = 1,
#                        estimate  = "{estimate} [{conf.low}, {conf.high}]",
#                        statistic = NULL,
#                        coef_omit = "Intercept")
#
#   expect_equal(
#     unique(out1[1:10, "term"]),
#     c("(Intercept)", "Miles per gallon", "Number of cylinders",
#       "Miles per gallon × drat", "drat")
#   )
#   expect_equal(
#     unique(out2[1:4, "term"]),
#     c("Miles per gallon", "Number of cylinders",
#       "Miles per gallon × drat", "drat")
#   )
# })

test_that("modelsummary: also applies variable labels for depvar", {
  dat <- mtcars
  dat$mpg <- haven::labelled(dat$mpg, label = "Miles per gallon")
  dat$mpg2 <- haven::labelled(dat$mpg, label = "Miles per gallon")
  dat$cyl <- as.factor(dat$cyl)
  dat$cyl <- haven::labelled(dat$cyl, label = "Number of cylinders")

  x <- list(
    lm(mpg ~ cyl + drat + disp, data = dat),
    lm(hp ~ cyl + drat + disp, data = dat)
  )

  out <- modelsummary(dvnames(x), "dataframe")
  expect_equal(names(out)[4:5], c("Miles per gallon", "hp"))
})
