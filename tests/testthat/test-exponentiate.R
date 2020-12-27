library(modelsummary)

fit = glm(am  ~ mpg + factor(cyl), 
          data = mtcars , 
          family = binomial ) 

test_that("logit coefficients exponentiate", {
  tab = modelsummary(fit, gof_omit=".*", statistic=NULL, output="dataframe",
                     exponentiate=TRUE)
  tab = unname(as.numeric(tab[[4]]))
  truth = unname(round(exp(coef(fit)), 3))
  expect_equal(truth, tab)
})

