source("helpers.R")

# Issue 507: extraneous error about not supported
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
ml <- modelsummary(mod, output = "modelsummary_list")
msg <- capture.output({
  tab <- modelsummary(ml, output = "data.frame")
})
expect_equivalent(length(msg), 0)

# output="modelsummary_list" and back to data.frame
mod <- list(
  lm(mpg ~ hp, mtcars),
  lm(mpg ~ hp + drat, mtcars)
)
tab <- modelsummary(mod, "modelsummary_list")
expect_true(all(sapply(tab, inherits, "modelsummary_list")))
expect_true(class(tab)[1] == "list")
tab <- modelsummary(tab, "data.frame")
expect_inherits(tab, "data.frame")
expect_equivalent(dim(tab), c(14, 5))

# tidiers empty
mod <- lm(mpg ~ hp + drat + vs, mtcars)
ml <- list(tidy = modelsummary:::get_estimates(mod))
class(ml) <- "modelsummary_list"
gl <- generics::glance(ml)
expect_inherits(gl, "data.frame")
expect_equivalent(dim(gl), c(1, 0))
ml <- list(glance = modelsummary:::get_gof(mod))
class(ml) <- "modelsummary_list"
expect_error(tidy(ml))

# backend estractor
mod1 <- lm(mpg ~ wt + cyl, data = mtcars)
mod2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
res <- modelsummary::get_estimates(mod1)
expect_equivalent(attr(res, "backend"), "parameters")

# get_estimates
options(modelsummary_get="broom")
res <- modelsummary::get_estimates(mod1)
expect_equivalent(attr(res, "backend"), "broom")

options(modelsummary_get="parameters")
res <- modelsummary::get_estimates(mod1)
expect_equivalent(attr(res, "backend"), "parameters")

# get_gof
options(modelsummary_get="broom")
res <- modelsummary::get_gof(mod1)
expect_equivalent(attr(res, "backend"), "broom")

options(modelsummary_get="parameters")
res <- modelsummary::get_gof(mod1)
expect_equivalent(attr(res, "backend"), "parameters")

# modelsummary
res <- modelsummary::modelsummary(list(mod1=mod1, mod2=mod2))
attr(res, "backend")
expect_equivalent(attr(res, "backend"), list(mod1=list(est="parameters", gof="parameters"), mod2=list(est="parameters", gof="parameters")))