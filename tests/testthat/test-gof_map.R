context('gof_map')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that('omit all gof', {

    gof_map_custom <- modelsummary::gof_map
    gof_map_custom$omit <- TRUE
    raw <- modelsummary::extract(mod, gof_map = gof_map_custom)

    truth <- c("-1.986", "(0.434)", "0.665", "(0.120)", "", "")
    expect_equal(truth, unname(raw[[4]]))

    truth <- c("4.739", "(4.045)", "", "", "-0.288", "(0.228)")  
    expect_equal(truth, unname(raw[[5]]))

})


test_that('omit all gof from only a few models', {

    mod1 <- glm(vs ~ mpg, data = mtcars, family = "binomial")
    mod2 <- lm(vs ~ mpg, data = mtcars)
    gof_map <- modelsummary::gof_map
    gof_map$omit <- TRUE
    gof_map$omit[gof_map$raw == "r.squared"] <- FALSE

    tmp <- modelsummary::extract(list(mod1, mod2), gof_map = gof_map)

    expect_equal(nrow(tmp), 5)
    expect_equal(ncol(tmp), 5)

})
