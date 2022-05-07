requiet("marginaleffects")

test_that("no error", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    mod <- lm(mpg ~ am + cyl + hp, data = dat)

    mfx <- marginaleffects(mod)
    cmp <- comparisons(mod)
    mm <- marginalmeans(mod)

    expect_error(modelsummary(mm,
                              shape = term + value ~ model,
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
