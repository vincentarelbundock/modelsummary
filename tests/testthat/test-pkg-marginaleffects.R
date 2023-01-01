requiet("marginaleffects")

test_that("no error", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    dat <<- dat
    mod <- lm(mpg ~ am + cyl + hp, data = dat)

    mfx <- marginaleffects(mod)
    cmp <- comparisons(mod)
    mm <- marginalmeans(mod)

    expect_error(modelsummary(mm,
                              shape = term + statistic + value ~ model,
                              output = "data.frame"),
                 NA)

    expect_error(modelsummary(mfx,
                              shape = term + contrast ~ model,
                              output = "data.frame"),
                 NA)
    expect_error(modelsummary(cmp,
                              shape = term + contrast ~ model,
                              output = "data.frame"),
                 NA)
})




test_that("multiple groups", {
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
    expect_equal(dim(tab), c(6, 7))

    tab <- modelsummary(
        cmp,
        output = "dataframe",
        gof_map = NA,
        shape = contrast_vs + contrast_am + cyl ~ model)
    expect_equal(dim(tab), c(6, 6))

    tab <- modelsummary(
        cmp,
        output = "dataframe",
        gof_map = NA,
        shape = contrast_vs + contrast_am ~ cyl + model)
    expect_equal(dim(tab), c(2, 7))

    expect_error(
        modelsummary(
            cmp,
            shape = term + cyl + trash + contrast_am + contrast_vs ~ model),
        regexp = "not found")
})