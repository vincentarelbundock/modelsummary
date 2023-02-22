source("helpers.R")
exit_file("works interactively")

# tidy.custom
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
expect_equivalent(tab[["(1)"]], c("a", "b"))
rm("tidy.custom")
rm("tidy_custom.custom")

# tidy.custom p values in polr models
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
tab <- suppressMessages(modelsummary(mod, stars = TRUE, output = "data.frame"))
truth <- c("", "", "0.020*", "(0.010)", "0.373**", "(0.123)", "10.158**", "(3.660)", "12.798**")
expect_equivalent(tab$POLR[1:9], truth)
rm("tidy_custom.polr")
