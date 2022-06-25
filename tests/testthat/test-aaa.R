# we no longer raise this warning
# test_that("first time stars=TRUE produces a warning (run first)", {
#   mod <- lm(mpg ~ hp, mtcars)
#   expect_warning(modelsummary(mod, "data.frame", stars = TRUE))
# })

test_that("first time output='latex' produces a warning (run first)", {
  mod <- lm(mpg ~ hp, mtcars)
  expect_warning(modelsummary(mod, "latex"))
})


test_that("first call raises a warning about `performance` metrics.", {
    requiet("brms")
    mod <- insight::download_model("brms_1")
    expect_warning(modelsummary(mod, statistic = "conf.int"))
})
