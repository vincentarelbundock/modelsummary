test_that("lm F tests conform to `vcov` argument", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    res <- modelsummary(mod, "data.frame", vcov = c("HC1", "stata", "HC2", "classical"))
    expect_true(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 2"])
    expect_false(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 3"])
    expect_false(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 4"])
    expect_false(res[res$term == "F", "Model 2"] == res[res$term == "F", "Model 3"])
    expect_false(res[res$term == "F", "Model 2"] == res[res$term == "F", "Model 4"])
})


test_that("lm & glm F tests conform to `vcov` argument", {
    mod <- list(
        lm(mpg ~ hp + factor(cyl), data = mtcars),
        glm(mpg ~ hp + factor(cyl), data = mtcars),
        lm(mpg ~ hp + factor(cyl), data = mtcars),
        glm(mpg ~ hp + factor(cyl), data = mtcars))
    res <- modelsummary(mod, "data.frame", 
                        vcov = c("classical", "classical", "robust", "robust"))
    expect_true(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 2"])
    expect_true(res[res$term == "F", "Model 3"] == res[res$term == "F", "Model 4"])
    expect_false(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 4"])
})


mod <- lm(cbind(cyl, disp, hp) ~ mpg + drat, data = mtcars) 

tab <- modelsummary(
    mod,
    output = "data.frame",
    shape = term + response ~ model,
    vcov = list(NULL, "HC3"),
    gof_omit = "response")
tab

get_estimates(mod, vcov = "HC3")
get_estimates(mod)
get_vcov(mod, vcov = "HC3")
get_vcov(mod)
