

test_that("duplicate names are padded", {
    mod <- list(
        "a" = lm(hp ~ mpg, mtcars),
        "a" = lm(hp ~ mpg + drat, mtcars)
    )

    tab <- msummary(mod, output = "data.frame")
    expect_true(all(c("a", "a ") %in% colnames(tab)))

    tab <- msummary(dvnames(mod), output = "data.frame")
    expect_true(all(c("hp", "hp ") %in% colnames(tab)))
})
