requiet("fixest")

panels <- list(
    "Panel A: MPG" = list(
        lm(mpg ~  hp, data = mtcars),
        lm(mpg ~  hp + factor(gear), data = mtcars)),
    "Panel B: Displacement" = list(
        lm(disp ~ hp, data = mtcars),
        lm(disp ~ hp + factor(gear), data = mtcars))
)
panelsummary(panels, gof_map = "nobs")


mod <- feols(mpg ~ csw(hp, vs, drat), data = mtcars, split = ~am)
panels <- list("am = 0" = mod[1:3], "am = 1" = mod[4:6])

# TODO: weird significance mismatch in intercept for different models
pkgload::load_all()
panelsummary(
    panels,
    fmt = fmt_significant(2),
    gof_map = c("nobs", "r.squared"))



dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")

mod <- feols(
    flipper_length_mm ~ csw0(body_mass_g, bill_depth_mm),
    split = ~species,
    data = dat)
panels <- list(
    "Adelie" = mod[1:3],
    "Chinstrap" = mod[1:3],
    "Gentoo" = mod[1:3]
)
panelsummary(panels, gof_map = c("nobs", "r.squared"))

panels <- list()
for (s in c("Adelie", "Gentoo", "Chinstrap")) {
    panels[[s]][[1]] <- lm(
        flipper_length_mm ~ body_mass_g,
        data = subset(dat, species == s))
    panels[[s]][[2]] <- lm(
        flipper_length_mm ~ body_mass_g + bill_depth_mm,
        data = subset(dat, species == s))
update_modelsummary()

pkgload::load_all()
panelsummary(
    panels,
    align = "ldd",
    gof_map = c("nobs", "r.squared"))

test_that("informative errors", {
    expect_error(panelsummary(panels, shape = term ~ model), regexp = "shape.*not supported")
    expect_error(panelsummary(panels, group_map = list()), regexp = "group_map.*not supported")
})


test_that("stars note", {
    p <- suppressWarnings(panelsummary(panels, output = "markdown", stars = TRUE))
    expect_true(any(grepl("Note", p)))
})


test_that("output formats: no validity", {
    p <- panelsummary(panels, output = "gt")
    expect_s3_class(p, "gt_tbl")
    p <- panelsummary(panels, output = "latex")
    expect_s3_class(p, "knitr_kable")
})



# panels <- list(
#     "Panel 1: MPG" = list(
#         "I" = lm(mpg ~  factor(cyl), data = mtcars),
#         "II" = lm(mpg ~  factor(cyl) + am, data = mtcars)),
#     "Panel 2: Displacement" = list(
#         "I" = lm(disp ~ factor(cyl), data = mtcars),
#         "II" = lm(disp ~ factor(cyl) + am, data = mtcars))
# )
# panelsummary(
#     panels,
#     title = "Four regression models in two panels.",
#     coef_rename = TRUE,
#     gof_map = c("nobs", "r.squared"))

# panels <- list(
#     list(
#         lm(mpg ~  factor(cyl), data = mtcars),
#         lm(mpg ~  factor(cyl) + am, data = mtcars)),
#     list(
#         lm(disp ~ factor(cyl), data = mtcars),
#         lm(disp ~ factor(cyl) + am, data = mtcars))
# )
# panelsummary(
#     panels,
#     title = "Four regression models in two panels.",
#     coef_rename = TRUE,
#     gof_map = c("nobs", "r.squared"))
