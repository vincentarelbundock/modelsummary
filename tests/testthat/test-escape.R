dat <- mtcars
colnames(dat)[1] <- "under_score"
colnames(dat)[2] <- "oh&yeah<sup>2</sup>"

test_that("escaped tables", {
    mod <- list(
        "First&Second" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat),
        "Third_Fourth" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat))

    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/escape_1.tex",
                        print = TRUE,
                        update = TRUE)
    expect_known_output(modelsummary(mod, output = "latex", escape = FALSE),
                        file = "known_output/escape_2.tex",
                            print = TRUE,
                        update = TRUE)
    ## <td style="text-align:left;"> `oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt;` </td>
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/escape_3.html",
                        print = TRUE,
                        update = TRUE)
    ## <td style="text-align:left;"> `oh&amp;yeah<sup>2</sup>` </td>
    expect_known_output(modelsummary(mod, output = "html", escape = FALSE),
                        file = "known_output/escape_4.html",
                        print = TRUE,
                        update = TRUE)
})


test_that("manual escape", {
    expect_equal(escape_latex("$&_"), "\\$\\&\\_")
    expect_equal(escape_html("<&"), "&lt;&amp;")
})


test_that("datasummary escape colnames and stub", {
    # TRUE
    expect_known_output(
        datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
                    data = dat,
                    output = "latex"),
        file = "known_output/escape_5.tex",
        print = TRUE,
        update = TRUE)

    # FALSE
    expect_known_output(
        datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
                    data = dat,
                    output = "latex",
                    escape = FALSE),
        file = "known_output/escape_6.tex",
        print = TRUE,
        update = TRUE)
})


test_that("datasummary_crosstab escape colnames and stub", {

    tmp <- mtcars
    tmp$under_score1 <- tmp$vs
    tmp$under_score2 <- tmp$gear
    tmp$under_score3 <- tmp$am

    ## TRUE
    expect_known_output(
        datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
                             data = tmp,
                             output = "latex"),
        file = "known_output/escape_7.tex",
        print = TRUE,
        update = TRUE)

    ## FALSE
    expect_known_output(
        datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
                             data = tmp,
                             output = "latex",
                             escape = FALSE),
        file = "known_output/escape_8.tex",
        print = TRUE,
        update = TRUE)

})


test_that("datasummary_correlation escape rownames and colnames", {

    ## TRUE
    expect_known_output(
        datasummary_correlation(dat,
                                output = "latex"),
        file = "known_output/escape_9.tex",
        print = TRUE,
        update = TRUE)

    ## FALSE
    expect_known_output(
        datasummary_correlation(dat,
                                output = "latex",
                                escape = FALSE),
        file = "known_output/escape_10.tex",
        print = TRUE,
        update = TRUE)

    ## TRUE
    expect_known_output(
        datasummary_correlation(dat,
                                output = "html"),
        file = "known_output/escape_9.html",
        print = TRUE,
        update = TRUE)

    ## FALSE
    expect_known_output(
        datasummary_correlation(dat,
                                output = "html",
                                escape = FALSE),
        file = "known_output/escape_10.html",
        print = TRUE,
        update = TRUE)

})
