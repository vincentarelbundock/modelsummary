dat <- mtcars
colnames(dat)[1] <- "under_score"
colnames(dat)[2] <- "oh&yeah<sup>2</sup>"


test_that("escaped tables", {
    # unicode minus signs break on windows
    skip_on_os("windows")
    mod <- list(
        "First&Second" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat),
        "Third_Fourth" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat))
    expect_snapshot(modelsummary(mod, output = "latex"))
    expect_snapshot(modelsummary(mod, output = "latex"))
    ## <td style="text-align:left;"> `oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt;` </td>
    expect_snapshot(modelsummary(mod, output = "html"))
    ## <td style="text-align:left;"> `oh&amp;yeah<sup>2</sup>` </td>
    expect_snapshot(modelsummary(mod, output = "html", escape = FALSE))
})


test_that("manual escape", {
    expect_equal(escape_latex("$&_"), "\\$\\&\\_")
    expect_equal(escape_html("<&"), "&lt;&amp;")
})


test_that("datasummary escape colnames and stub", {
    # TRUE
    expect_snapshot(
        datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
                    data = dat,
                    output = "latex"))
    # FALSE
    expect_snapshot(
        datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
                    data = dat,
                    output = "latex",
                    escape = FALSE))
})


test_that("datasummary_crosstab escape colnames and stub", {
    tmp <- mtcars
    tmp$under_score1 <- tmp$vs
    tmp$under_score2 <- tmp$gear
    tmp$under_score3 <- tmp$am
    ## TRUE
    expect_snapshot(
        datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
                             data = tmp,
                             output = "latex"))
    ## FALSE
    expect_snapshot(
        datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
                             data = tmp,
                             output = "latex",
                             escape = FALSE))
})


test_that("datasummary_correlation escape rownames and colnames", {
    # unicode minus signs break on windows
    skip_on_os("windows")
    ## TRUE
    expect_snapshot(datasummary_correlation(dat, output = "latex"))
    ## FALSE
    expect_snapshot( datasummary_correlation(dat, output = "latex", escape = FALSE))
    ## TRUE
    expect_snapshot(datasummary_correlation(dat, output = "html"))
    ## FALSE
    expect_snapshot( datasummary_correlation(dat, output = "html", escape = FALSE))
})


test_that("Bugfix: escape & latex & coef_map", {
    # Bug reported here:
    # https://github.com/vincentarelbundock/modelsummary/issues/378
    mtcars2 <- mtcars
    mtcars2$disp_x <- mtcars2$disp
    mtcars2$wt_x <- mtcars2$wt

    # Problem 1:  When there is a variable with underscore raw name not
    # displayed in output; not in expected order 
    reg_prob1 <- list()
    reg_prob1[[1]] <- lm(data = mtcars2, mpg ~ disp_x)
    reg_prob1[[2]] <- lm(data = mtcars2, mpg ~  disp_x + wt)
    reg_prob1[[3]] <- lm(data = mtcars2, mpg ~  disp_x + wt + am)
    expect_snapshot(
        msummary(reg_prob1, coef_map = c("disp_x", "wt"), output = "latex"))

    # Problem 2: When there are two or more variables with underscores rows for
    # coefficients and std errors are not in the right place and names are not
    # displayed
    reg_prob2 <- list()
    reg_prob2[[1]] <- lm(data = mtcars2, mpg ~ disp_x)
    reg_prob2[[2]] <- lm(data = mtcars2, mpg ~  disp_x + wt_x)
    reg_prob2[[3]] <- lm(data = mtcars2, mpg ~  disp_x + wt_x + am)
    expect_snapshot(
        msummary(reg_prob2, coef_map = c("disp_x", "wt_x"), output = "latex"))
})


test_that("bugs stay dead: escape=FALSE w/ coef_map", {
    url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
    dat <- read.csv(url)
    cm <- c('Literacy'    = 'Literacy (\\%)',
            'Commerce'    = 'Patents per capita',
            '(Intercept)' = 'Constant')
    dat <- read.csv(url)
    models <- list("OLS 1" = lm(Donations ~ Literacy + Commerce, data = dat))
    expect_snapshot(modelsummary(models, coef_map = cm, output = "latex_tabular", escape = FALSE))
})


test_that("column headers are not escaped with `escape=FALSE`", {
    # unicode minus signs break on windows
    skip_on_os("windows")
    mod <- list(
        "<code>lm()</code>" = lm(mpg ~ hp + drat, mtcars),
        "<code>lm_robust()</code>" = lm(mpg ~ hp + drat, mtcars))
    expect_snapshot(modelsummary(mod, 
                                 vcov = c("classical", "HC1"), 
                                 escape = FALSE, 
                                 output = "html"))
})
