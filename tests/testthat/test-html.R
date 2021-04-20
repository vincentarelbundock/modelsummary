test_that("raw html output", {
    truth <- structure("<table class=\"table\" style=\"width: auto !important; margin-left: auto; margin-right: auto;\">\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   </th>\n   <th style=\"text-align:center;\"> Model 1 </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> (Intercept) </td>\n   <td style=\"text-align:center;\"> 324.082 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> mpg </td>\n   <td style=\"text-align:center;\"> -8.830 </td>\n  </tr>\n</tbody>\n</table>", format = "html", class = c("modelsummary_string", "kableExtra", "knitr_kable"))
    mod <- lm(hp ~ mpg, data = mtcars)
    raw_html <- modelsummary(mod,
                            output = "html",
                            statistic = NULL,
                            gof_omit = ".*")
    expect_equal(raw_html, truth)
})
