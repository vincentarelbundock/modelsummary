# This does not work in devtools::test or check

test_that("tidy.custom", {

  tidy.custom <- function(x) {
    suppressWarnings(broom:::tidy.lm(x))
  }

  tidy_custom.custom <- function(x) {
    out <- suppressWarnings(broom:::tidy.lm(x))
    out$estimate <- letters[1:nrow(out)]
    out
  }

  mod <- lm(hp ~ mpg, mtcars)
  class(mod) <- c("custom", class(mod))

  tab <- modelsummary(
    mod,
    output = "data.frame",
    gof_omit = ".*",
    statistic = NULL)
   expect_equal(tab[[4]], c("a", "b"))

  rm("tidy.custom")
  rm("tidy_custom.custom")
})



test_that("tidy.custom p values in polr models", {

    skip_if_not_installed("AER")
    skip_if_not_installed("MASS")
    library(MASS)
    library(AER)

    tidy_custom.polr <- function(x, ...) {
    s <- coeftest(x)
    out <- data.frame(
        term = row.names(s),
        p.value = s[, "Pr(>|z|)"])
    out
    }

    mod = list(
      "LM" = lm(gear ~ hp + mpg, data = mtcars),
      "POLR" = polr(as.ordered(gear) ~ hp + mpg, data = mtcars))

    tab = modelsummary(mod, stars = TRUE, output = "data.frame")
    expect_equal(tab$LM[c(1, 3, 5)],
                 c("0.339", "0.007**", "0.118***"))

    rm("tidy_custom.polr")
})
