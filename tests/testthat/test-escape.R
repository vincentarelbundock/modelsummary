test_that("escaped tables", {
    dat <- mtcars
    colnames(dat)[1] <- "under_score"
    colnames(dat)[2] <- "oh&yeah<sup>2</sup>"
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
