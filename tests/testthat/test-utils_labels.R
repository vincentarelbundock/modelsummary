test_that("modelsummary: use variable labels by default", {
  data(trees)
  dat <- trees

  # no labels
  mod <- list(
    "Bivariate" = lm(Girth ~ Height, data = dat),
    "Multivariate" = lm(Girth ~ Height + Volume, data = dat))
  tab <- modelsummary(mod, "dataframe")
  expect_true(all(c("Height", "Volume") %in% tab$term))

  # with labels
  dat$Height <- haven::labelled(dat$Height, label = "Height (in feet)")
  dat$Volume <- haven::labelled(dat$Volume, label = "Volume (in liters)")
  mod <- list(
    "Bivariate" = lm(Girth ~ Height, data = dat),
    "Multivariate" = lm(Girth ~ Height + Volume, data = dat))
  tab <- modelsummary(mod, "dataframe")
  expect_true(all(c("Height (in feet)", "Volume (in liters)") %in% tab$term))
})


test_that("interaction", {
  data(trees)
  dat <- trees
  dat$Height <- haven::labelled(dat$Height, label = "Height (in feet)")
  dat$Volume <- haven::labelled(dat$Volume, label = "Volume (in liters)")
  mod <- lm(Girth ~ Height*Volume, data = dat)
  tab <- modelsummary(mod, "dataframe")
  expect_true("Height (in feet) × Volume (in liters)" %in% tab$term)
})


test_that("coef_rename ignores labels", {
  data(trees)
  dat <- trees
  dat$height_2 <- haven::labelled(dat$Height, label = "Height (in feet)")
  dat$volume_2 <- haven::labelled(dat$Volume, label = "Volume (in liters)")
  mod <- list(
    "Bivariate" = lm(Girth ~ height_2, data = dat),
    "Multivariate" = lm(Girth ~ height_2 + volume_2, data = dat))
  tab <- modelsummary(mod, "dataframe", coef_rename = coef_rename)
  expect_true(all(c("Height 2", "Volume 2") %in% tab$term))
})


test_that("modelsummary: also applies variable labels for depvar", {
  dat <- mtcars
  dat$mpg <- haven::labelled(dat$mpg, label = "Miles per gallon")
  dat$cyl <- as.factor(dat$cyl)
  dat$cyl <- haven::labelled(dat$cyl, label = "Number of cylinders")
  mod <- list(
    lm(mpg ~ cyl + drat + disp, data = dat),
    lm(hp ~ cyl + drat + disp, data = dat))
  tab <- modelsummary(dvnames(mod), "dataframe")
  expect_equal(names(tab)[4:5], c("Miles per gallon", "hp"))
  expect_true("Number of cylinders" %in% tab$term)
})



test_that("formatting + labels", {
  dat <- mtcars
  dat$mpg <- haven::labelled(dat$mpg, label = "Miles per gallon")
  dat$mpg2 <- haven::labelled(dat$mpg, label = "Miles per gallon")
  dat$cyl <- as.factor(dat$cyl)
  dat$cyl <- haven::labelled(dat$cyl, label = "Number of cylinders")

  mod <- list(
    lm(hp ~ mpg + drat:mpg + cyl, data = dat),
    lm(hp ~ mpg2 * drat + cyl, data = dat)
  )
  out1 <- modelsummary(mod, "dataframe")
  out2 <- modelsummary(mod, "dataframe", fmt = 1,
                       estimate  = "{estimate} [{conf.low}, {conf.high}]",
                       statistic = NULL,
                       coef_omit = "Intercept")

  expect_equal(
    unique(out1[1:10, "term"]),
    c("(Intercept)", "Miles per gallon", "Number of cylinders",
      "Miles per gallon × drat", "drat")
  )
  expect_equal(
    unique(out2[1:4, "term"]),
    c("Miles per gallon", "Number of cylinders",
      "Miles per gallon × drat", "drat")
  )
})


