models <- list(
  lm(hp ~ mpg, mtcars),
  lm(hp ~ mpg + drat, mtcars))


test_that("stars_note < are protected by $ in latex", {
  tab <- modelsummary(models, stars=TRUE, output="latex")
  expect_true(grepl("p $<$ 0.1", tab, fixed=TRUE))
})


test_that("model names protected by multicolumn with dcolumn",{
  tab <- modelsummary(
    models, 
    stars = TRUE, 
    output = "latex",
    align = c("l", "D{.}{.}{-1}", "D{.}{.}{-1}"))
  expect_true(grepl("multicolumn....c..Model", tab))
})

test_that("output = latex_tabular", {
  tab <- modelsummary(
    models,
    output = "latex_tabular")
  truth = "\n\\begin{tabular}[t]{lcc}\n\\toprule\n  & Model 1 & Model 2\\\\\n\\midrule\n(Intercept) & 324.082 & 278.515\\\\\n & (27.433) & (55.415)\\\\\nmpg & -8.830 & -9.985\\\\\n & (1.310) & (1.792)\\\\\ndrat &  & 19.126\\\\\n &  & (20.198)\\\\\n\\midrule\nNum.Obs. & 32 & 32\\\\\nR2 & 0.602 & 0.614\\\\\nR2 Adj. & 0.589 & 0.588\\\\\nAIC & 336.9 & 337.9\\\\\nBIC & 341.3 & 343.7\\\\\nLog.Lik. & -165.428 & -164.940\\\\\nF & 45.460 & 23.100\\\\\n\\bottomrule\n\\end{tabular}"
  expect_identical(as.character(truth), as.character(tab))
})
