# dvnames adds names
d <- data.frame(x = 1:10, y = 2:11)
m1 <- lm(y ~ x, data = d)
m2 <- lm(x ~ y, data = d)
dvnout <- dvnames(list(m1, m2))
nondvn <- list("y" = m1, "x" = m2)
expect_identical(dvnout, nondvn)

# dvnames with single input
d <- data.frame(x = 1:10, y = 2:11)
m1 <- lm(y ~ x, data = d)
dvnout <- dvnames(m1)
nondvn <- list("y" = m1)
expect_identical(dvnout, nondvn)

# dvnames numbering
d <- data.frame(x = 1:10, y = 2:11)
m1 <- lm(y ~ x, data = d)
m2 <- lm(x ~ y, data = d)
dvnout <- dvnames(list(m1, m2), number = TRUE)
nondvn <- list("y (1)" = m1, "x (2)" = m2)
expect_identical(dvnout, nondvn)

# dvnames fill
d <- data.frame(x = 1:10, y = 2:11)
m1 <- lm(y ~ x, data = d)
m2 <- lm(x ~ y, data = d)
dvnout <- dvnames(list(m1, m2, 1))
nondvn <- list("y" = m1, "x" = m2, "Model" = 1)
expect_identical(dvnout, nondvn)

# dvnames transformation
m1 <- lm(mpg ~ cyl + hp, data = mtcars)
m2 <- lm(log(mpg+1) ~ cyl + hp, data = mtcars)
a <- modelsummary(dvnames(list(m1, m2)), output = "data.frame")
b <- modelsummary(dvnames(list(m1, m2), strip = TRUE), output = "data.frame")
expect_true("log(mpg + 1)" %in% colnames(a))
expect_false("log(mpg + 1)" %in% colnames(b))
