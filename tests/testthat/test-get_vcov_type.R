requiet("lfe")
requiet("fixest")
requiet("estimatr")
fixest::setFixest_nthreads(1) # avoids warning pollution


test_that("automatic standard errors labelling", {
    mod <- lm(hp ~ mpg, mtcars)
    vcov <- list("iid", "robust", "stata", ~cyl, ~vs + am, ~vs:am)
    tab <- modelsummary(mod, vcov = vcov, output = "dataframe")
    expect_true("IID" %in% tab[[4]])
    expect_true("HC3" %in% tab[[5]])
    expect_true("HC1" %in% tab[[6]])
    expect_true("by: cyl" %in% tab[[7]])
    expect_true("by: vs & am" %in% tab[[8]])
    expect_true("by: vs & am" %in% tab[[9]])
    vcov <- list("iid", "robust")
    tab <- modelsummary(mod, vcov = vcov, output = "dataframe")
    expect_true("Std.Errors" %in% tab[[2]])
    expect_true("IID" %in% tab[[4]])
    expect_true("HC3" %in% tab[[5]])
})


test_that("consistent display of clustered SEs", {
    mod_feols <- feols(mpg ~ wt, mtcars, vcov = ~ am + cyl)
    mod_felm <- felm(mpg ~ wt | 0 | 0 | am + cyl, mtcars, cmethod = "cgm2")
    gm <- modelsummary::gof_map
    gm <- gm[gm$clean == "Std.Errors", ]
    tab1 <- modelsummary(mod_feols, output = "dataframe", gof_map = gm)
    tab2 <- modelsummary(mod_felm, output = "dataframe", gof_map = gm)
    tab3 <- modelsummary(mod_feols, vcov = ~ am + cyl, output = "dataframe", gof_map = gm)
    expect_equal(tab1, tab2)
    expect_equal(tab1, tab3)
})


test_that("consistent gof std error display fixest/lfe/estimatr", {
  testthat::skip_if_not_installed(c("fixest", "lfe", "estimatr"))
  fixest_mod <- fixest::feols(hp ~ mpg + drat, mtcars, vcov = ~vs)
  mod <- list(
    "feols" = fixest_mod,
    "felm" = felm(hp ~ mpg + drat |0 | 0 | vs, mtcars),
    "estimatr" = lm_robust(hp ~ mpg + drat, mtcars, se_type = 'stata', clusters = vs))
  tab <- modelsummary(mod, output = "data.frame")
  expect_equal(tab$feols[nrow(tab)], "by: vs")
  expect_equal(tab$felm[nrow(tab)], "by: vs")
  expect_equal(tab$estimatr[nrow(tab)], "by: vs")
})
