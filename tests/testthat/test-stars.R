#context('stars')

#library(modelsummary)

#mod <- list()
#mod$OLS <- lm(am ~ drat, data = mtcars)
#mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

#test_that("default stars", {

#modelsummary::extract(mod[[2]], stars = c('+' = .8, '*' = .1))

#msummary(mod, stars = TRUE)

#test_that("custom stars", {

    #raw <- msummary(models, stars =TRUE) %>% 
           #gt::as_raw_html()
    #expect_known_output(cat(raw), "known_output/stars_default.html")

    #raw <- msummary(models, stars = ) %>%
           #gt::as_raw_html()
    #expect_known_output(cat(raw), "known_output/stars_custom.html")


    #raw <- modelsummary::extract(mod, coef_omit = c('drat|qsec'))

    #truth <- c('(Intercept)', '(Intercept)', 'Num.Obs.')
    #expect_equal(unname(raw[[2]][1:3]), truth)

#})
