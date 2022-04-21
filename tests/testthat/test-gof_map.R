requiet("tibble")

test_that("gof_map = NA", {
    mod <- lm(mpg ~ hp, mtcars)
    tab1 <- modelsummary(mod, output = "data.frame", gof_map = NA)
    tab2 <- modelsummary(mod, output = "data.frame", gof_omit = ".*")
    expect_equal(tab1, tab2)
    expect_false("gof" %in% tab1$part)
    expect_false("gof" %in% tab2$part)
})
   
test_that("gof_map inputs are equivalent", {
    mod <- lm(mpg ~ hp, mtcars)
    gm <- tribble(
        ~raw, ~clean, ~fmt,
        "r.squared", "R-Squared", "%.7f")
    w <- modelsummary(mod, output = "data.frame", gof_map = gm)
    gm <- tribble(
        ~raw, ~clean, ~fmt,
        "r.squared", "R-Squared", function(x) sprintf("%.7f", x))
    x <- modelsummary(mod, output = "data.frame", gof_map = gm)
    gm <- tribble(
        ~raw, ~clean, ~fmt,
        "r.squared", "R-Squared", 7)
    y <- modelsummary(mod, output = "data.frame", gof_map = gm)
    gm <- list(list(raw = "r.squared", clean = "R-Squared", fmt = 7))
    z <- modelsummary(mod, output = "data.frame", gof_map = gm)
    expect_equal(w, x)
    expect_equal(w, y)
    expect_equal(w, z)
})


test_that("character vector", {
    mod <- lm(mpg ~ hp + drat, mtcars)
    tab <- modelsummary(mod,
                        gof_map = c("r.squared", "rmse", "nobs"),
                        output = "data.frame")
    expect_equal(tab$term[7:9], c("R2", "RMSE", "Num.Obs."))
    expect_error(modelsummary(mod, gof_map = c("junk", "rmse", "nobs"), output = "data.frame"), NA)

    tab <- modelsummary(
        mod,
        output = "data.frame",
        gof_map = "")
    expect_equal(nrow(tab), 6)
})

