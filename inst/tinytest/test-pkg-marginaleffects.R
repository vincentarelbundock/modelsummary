source("helpers.R")
requiet("marginaleffects")

# no error
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
dat <<- dat
mod <- lm(mpg ~ am + cyl + hp, data = dat)
rm("dat")

mfx <- marginaleffects(mod)
cmp <- comparisons(mod)
mm <- marginalmeans(mod)

tab <- modelsummary(mm, shape = term + statistic + value ~ model, output = "data.frame")
expect_inherits(tab, "data.frame")
tab <- modelsummary(mfx, shape = term + contrast ~ model, output = "data.frame")
expect_inherits(tab, "data.frame")
tab <- modelsummary(cmp, shape = term + contrast ~ model, output = "data.frame")
expect_inherits(tab, "data.frame")

# multiple groups
requiet("marginaleffects")

mod <- lm(mpg ~ am + vs + factor(cyl), data = mtcars)
mod2 <- lm(mpg ~ drat + am + vs + factor(cyl), data = mtcars)
cmp <- comparisons(mod, variables = c("am", "vs"), by = "cyl", cross = TRUE)
cmp2 <- comparisons(mod2, variables = c("am", "vs"), by = "cyl", cross = TRUE)

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
