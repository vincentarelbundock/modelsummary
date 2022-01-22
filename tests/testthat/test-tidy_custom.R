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
   expect_equal(tab[["Model 1"]], c("a", "b"))
  rm("tidy.custom")
  rm("tidy_custom.custom")
})


test_that("bugfix: tidy_custom w/ new column and insufficient rows", {
  requiet("MASS")
  mod <- polr(as.ordered(gear) ~ mpg + drat, data = mtcars)
  # default has 4 rows
  tmp <- suppressMessages(get_estimates(mod))
  expect_equal(nrow(tmp), 4)
  # custom has 2 rows
  tidy_custom.polr <- function(x, ...) {
    s <- lmtest::coeftest(x)
    out <- data.frame(
      term = row.names(s),
      p.value = s[, "Pr(>|t|)"])
    out
  }
  tmp <- suppressMessages(tidy_custom(mod))
  expect_equal(nrow(tmp), 2)
  # combined has 4 rows
  tmp <- get_estimates(mod)
    expect_equal(nrow(tmp), 4)
  # modelsummary works
  expect_error(modelsummary(mod), NA)
  # clean
  rm("tidy_custom.polr")
})


test_that("tidy.custom p values in polr models", {
    requiet("MASS")
    requiet("AER")
    tidy_custom.polr <- function(x, ...) {
    s <- coeftest(x)
    out <- data.frame(
        term = row.names(s),
        p.value = s[, "Pr(>|z|)"])
    out
    }
    mod <- list(
      "LM" = lm(gear ~ hp + mpg, data = mtcars),
      "POLR" = polr(as.ordered(gear) ~ hp + mpg, data = mtcars))
    tab <- modelsummary(mod, stars = TRUE, output = "data.frame")
    truth <- c("", "", "0.020**", "(0.010)", "0.373***", "(0.123)", "10.158***",
               "(3.660)", "12.798***")
    expect_equal(tab$POLR[1:9], truth)
    rm("tidy_custom.polr")
})
