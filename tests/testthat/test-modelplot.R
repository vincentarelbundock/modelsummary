library(vdiffr)
library(ggplot2)

context("modelplot")


test_that("modelplot: single model", {

  mod <- lm(hp ~ mpg + drat, data = mtcars)
  p <- modelplot(mod)
  vdiffr::expect_doppelganger("vanilla", p)

  mod <- lm(hp ~ mpg + drat, data = mtcars)
  p <- modelplot(mod, coef_omit = 'Interc')
  vdiffr::expect_doppelganger("coef_omit", p)

  params <- list(
    geom_vline(xintercept = 0, color = 'orange'),
    annotate("rect", alpha = .1, xmin = -.5, xmax = .5, ymin = -Inf, ymax = Inf),
    geom_point(aes(y = term, x = estimate), alpha = .3,
               size = 10, color = 'red', shape = 'square')
  )
  p <- modelplot(mod, 
                 coef_map=c("drat"="Rear axle ratio", "mpg"="Miles / gallon"),
                 color="green", shape="square", background=params) 
  vdiffr::expect_doppelganger("coef_map + color + shape + background", p)

})

test_that("modelplot: multiple model", {

  mod <- list(lm(hp ~ mpg + drat, data = mtcars),
              lm(hp ~ mpg, data = mtcars))
  p <- modelplot(mod)
  vdiffr::expect_doppelganger("multiple plots vanilla", p)
 
  mod <- list(lm(hp ~ mpg + drat, data = mtcars),
              lm(hp ~ mpg, data = mtcars))
  p <- modelplot(mod, facet=TRUE)
  vdiffr::expect_doppelganger("multiple plots facet", p)

})
