source("helpers.R")
requiet("marginaleffects")

# no error
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
dat <<- dat
mod <- lm(mpg ~ am + cyl + hp, data = dat)
rm("dat")

mfx <- avg_slopes(mod)
cmp <- avg_comparisons(mod)

tab <- modelsummary(mfx, shape = term + contrast ~ model, output = "data.frame")
expect_inherits(tab, "data.frame")
tab <- modelsummary(cmp, shape = term + contrast ~ model, output = "data.frame")
expect_inherits(tab, "data.frame")

# multiple groups
mod <- lm(mpg ~ am + vs + factor(cyl), data = mtcars)
mod2 <- lm(mpg ~ drat + am + vs + factor(cyl), data = mtcars)
cmp <- avg_comparisons(mod, variables = c("am", "vs"), by = "cyl", cross = TRUE)
cmp2 <- avg_comparisons(mod2, variables = c("am", "vs"), by = "cyl", cross = TRUE)

tab <- modelsummary(
    list(cmp, cmp2),
    output = "dataframe",
    gof_map = NA,
    shape = cyl + contrast_am + contrast_vs ~ model)
expect_equivalent(dim(tab), c(6, 7))

tab <- modelsummary(
    cmp,
    output = "dataframe",
    gof_map = NA,
    shape = contrast_vs + contrast_am + cyl ~ model)
expect_equivalent(dim(tab), c(6, 6))

tab <- modelsummary(
    cmp,
    output = "dataframe",
    gof_map = NA,
    shape = contrast_vs + contrast_am ~ cyl + model)
expect_equivalent(dim(tab), c(2, 7))

expect_error(
    modelsummary(
        cmp,
        shape = term + cyl + trash + contrast_am + contrast_vs ~ model),
    pattern = "not found")
