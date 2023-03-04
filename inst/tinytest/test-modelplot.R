source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")
requiet("sandwich")

# single model
mod <- lm(hp ~ mpg + drat, data = mtcars)
p <- modelplot(mod)
expect_snapshot_plot(p, "modelplot-vanilla")

mod <- lm(hp ~ mpg + drat, data = mtcars)
p <- modelplot(mod, coef_omit = "Interc")
expect_snapshot_plot(p, "modelplot-coef_omit")

params <- list(
  ggplot2::geom_vline(xintercept = 0, color = "orange"),
  ggplot2::annotate("rect", alpha = .1, xmin = -.5, xmax = .5, ymin = -Inf, ymax = Inf),
  ggplot2::geom_point(ggplot2::aes(y = term, x = estimate),
    alpha = .3,
    size = 10, color = "red", shape = "square")
)
p <- modelplot(mod,
  coef_map = c("drat" = "Rear axle ratio", "mpg" = "Miles / gallon"),
  color = "green", background = params)
expect_snapshot_plot(p, "modelplot-coef_map_color_shape_background")

# multiple models
mod <- list(
  lm(hp ~ mpg + drat, data = mtcars),
  lm(hp ~ mpg, data = mtcars))
p <- modelplot(mod)
expect_snapshot_plot(p, "modelplot-multiple_plots_vanilla")

mod <- list(
  lm(hp ~ mpg + drat, data = mtcars),
  lm(hp ~ mpg, data = mtcars))
p <- modelplot(mod, facet = TRUE)
expect_snapshot_plot(p, "modeplot-multiple_plots_facet")

# preserve model order
mod <- list(
  "C" = lm(mpg ~ hp, data = mtcars),
  "A" = lm(mpg ~ hp + drat + vs, data = mtcars),
  "B" = lm(mpg ~ hp + drat, data = mtcars))
p <- modelplot(mod, draw = TRUE)
expect_snapshot_plot(p, "modeplot-model_order")

# conf_level=NULL
mod <- lm(hp ~ mpg + drat, data = mtcars)
p <- modelplot(mod, draw = FALSE, conf_level = NULL)
expect_inherits(p, "data.frame")
expect_equivalent(nrow(p), 3)

# vcov
mod <- list(
  lm(hp ~ mpg + drat, data = mtcars),
  lm(hp ~ mpg + drat, data = mtcars))

so <- list(vcov, sandwich::vcovHC)
p1 <- modelplot(mod, vcov = so, draw = FALSE)
p2 <- modelplot(mod, vcov = "HC3", draw = FALSE)

known <- c(
  165.179327669237, 182.406373565931, -13.6502180401172,
  -15.0390897152001, -22.1832974370102, -28.1858724755655)
expect_equivalent(p1$conf.low, known)

known <- c(
  182.406373565931, 182.406373565931, -15.0390897152001,
  -15.0390897152001, -28.1858724755655, -28.1858724755655)

known <- c(
  182.406373565931, 182.406373565931, -15.0390897152001,
  -15.0390897152001, -28.1858724755655, -28.1858724755655)
expect_equivalent(p2$conf.low, known)

# single model without ci
mod <- lm(hp ~ mpg + drat, data = mtcars)
p <- modelplot(mod, conf_level = NULL)

# multiple models without ci
mod <- list(
  lm(hp ~ mpg, data = mtcars),
  lm(hp ~ mpg + drat, data = mtcars),
  lm(hp ~ mpg + drat + am, data = mtcars))
p <- modelplot(mod, facet = TRUE, conf_level = NULL)
p <- modelplot(mod, facet = FALSE, conf_level = NULL)

# car::deltaMethod: Issue #491
requiet("car")
fit <- lm(mpg ~ disp + hp, mtcars)
fit_dm <- car::deltaMethod(fit, "disp / hp", rhs = 1)
p <- modelplot(fit_dm)

# Issue #529
requiet("survey")
dat <- mtcars
dat$weights <- dat$mpg / dat$disp
des <- svydesign(ids = ~1, data = dat, weights = dat$weights)
mod <- svyglm(vs ~ hp + drat + mpg + disp, design = des, family = quasibinomial())
p <- suppressWarnings(modelplot(mod, color = "red"))
expect_inherits(p, "gg")