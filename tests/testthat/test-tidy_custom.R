## This does not work in devtools::test or check

# test_that("tidy.custom", {

#   tidy.custom <- function(x) {
#     suppressWarnings(broom:::tidy.lm(x))
#   }

#   tidy_custom.custom <- function(x) {
#     out <- suppressWarnings(broom:::tidy.lm(x))
#     out$estimate <- letters[1:nrow(out)]
#     out
#   }

#   mod <- lm(hp ~ mpg, mtcars)
#   class(mod) <- c("custom", class(mod))

#   tab <- modelsummary(
#     mod,
#     output = "data.frame",
#     gof_omit = ".*",
#     statistic = NULL)
#    expect_equal(tab[[4]], c("a", "b"))

#   rm("tidy.custom")
#   rm("tidy_custom.custom")
# })

