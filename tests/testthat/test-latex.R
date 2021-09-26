models <- list(
  lm(hp ~ mpg, mtcars),
  lm(hp ~ mpg + drat, mtcars))

test_that("latex threeparttable argument", {
    tab1 <- modelsummary(models, output = "latex", stars = TRUE)
    tab2 <- modelsummary(models, output = "latex", threeparttable = TRUE, stars = TRUE)
    expect_false(grepl("threeparttable", tab1))
    expect_true(grepl("threeparttable", tab2))
    expect_equal(sum(grepl("threeparttable", strsplit(tab2, "\n")[[1]])), 2)


    ## kableExtra::footnote has a bug with multiple footnotes and threeparttable, so we combine notes.
    ft <- "Here is a very very very very very very very very very very very very very very very very very long footnote"
    tab3 <- modelsummary(models,
                         output = "latex",
                         title = "Regression output",
                         notes = ft, 
                         stars = TRUE,
                         threeparttable = TRUE)
    expect_equal(sum(grepl("threeparttable", strsplit(tab3, "\n")[[1]])), 2)
})


test_that("stars_note < are protected by $ in latex", {
  tab <- modelsummary(models, stars=TRUE, output="latex")
  expect_true(grepl("p $<$ 0.1", tab, fixed=TRUE))
})


## dcolumn is no longer officially supported
## test_that("model names protected by multicolumn with dcolumn",{
##   tab <- modelsummary(
##     models, 
##     stars = TRUE, 
##     output = "latex",
##     align = c("l", "D{.}{.}{-1}", "D{.}{.}{-1}"))
##   expect_true(grepl("multicolumn....c..Model", tab))
## })

test_that("output = latex_tabular", {
    expect_snapshot(modelsummary(models, output = "latex_tabular"))
})
