source("helpers.R")
requiet("flextable")

models <- list()
models[['OLS 1']] <- lm(hp ~ mpg + wt, mtcars)
models[['Poisson 1']] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[['OLS 2']] <- lm(vs ~ hp + wt, mtcars)
models[['Logit 1']] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[['Logit 2']] <- glm(am ~ hp + disp, mtcars, family = binomial())

# no error with caption and notes
tab <- modelsummary(
  models,
  "flextable",
  title = "test title",
  notes = "test note",
  stars = TRUE
)
expect_inherits(tab, "flextable")

# Issue #974: shape="rbind" places panel labels as group rows in the body,
# not in the header. A flextable stores its body and header as data.frames,
# so we assert on those rather than on a fragile rendered snapshot.
panels <- list(
  "Panel A" = lm(mpg ~ hp, mtcars),
  "Panel B" = lm(disp ~ hp, mtcars)
)
tab <- modelsummary(
  panels,
  shape = "rbind",
  output = "flextable",
  gof_omit = ".*"
)
expect_inherits(tab, "flextable")
body_stub <- trimws(as.character(tab$body$dataset[[1]]))
header_cells <- trimws(as.character(unlist(tab$header$dataset)))
# panel labels are group rows at the top of each panel in the body
expect_true("Panel A" %in% body_stub)
expect_true("Panel B" %in% body_stub)
expect_true(which(body_stub == "Panel A") < which(body_stub == "Panel B"))
# panel labels do not leak into the header
expect_false("Panel A" %in% header_cells)
expect_false("Panel B" %in% header_cells)

# # Issue #761
# d <- data.frame(x = rnorm(100), y = rnorm(100))
# res <- list(
#   lm(y ~ x, data = d),
#   lm(y ~ x + I(x^2), data = d)
# )
# modelsummary(
#   res,
#   output = "flextable",
#   estimate = c(ABC = "estimate"),
#   shape = term ~ model + statistic
# )
