requiet("tibble")

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
