source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

dat <- mtcars
colnames(dat)[1] <- "under_score"
colnames(dat)[2] <- "oh&yeah<sup>2</sup>"

# escaped tables
# unicode minus signs break on windows
mod <- list(
    "First&Second" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat),
    "Third_Fourth" = lm(hp ~ under_score + `oh&yeah<sup>2</sup>` + drat, dat))
expect_snapshot_print(
    modelsummary(mod, output = "latex"),
    "escape-latex")
## <td style="text-align:left;"> `oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt;` </td>
expect_snapshot_print(
    modelsummary(mod, output = "html"),
    "escape-html")
## <td style="text-align:left;"> `oh&amp;yeah<sup>2</sup>` </td>
expect_snapshot_print(
    modelsummary(mod, output = "html", escape = FALSE),
    "escape-html_escape_FALSE")

# manual escape
expect_equivalent(modelsummary:::escape_latex("$&_"), "\\$\\&\\_")
expect_equivalent(modelsummary:::escape_html("<&"), "&lt;&amp;")

# datasummary escape colnames and stub
# TRUE
expect_snapshot_print(
    datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
                data = dat, output = "latex"),
    "escape-datasummary_escape_colnames")
    
# FALSE
expect_snapshot_print(
    datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
        data = dat,
        output = "latex",
        escape = FALSE),
    "escape-datasummary_escape_colnames_FALSE")

# datasummary_crosstab escape colnames and stub
tmp <- mtcars
tmp$under_score1 <- tmp$vs
tmp$under_score2 <- tmp$gear
tmp$under_score3 <- tmp$am
## TRUE
expect_snapshot_print(
    datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
        data = tmp, output = "latex"),
    "escape-crosstab_latex")
## FALSE
expect_snapshot_print(
    datasummary_crosstab(under_score1 * under_score2 ~ under_score3,
        data = tmp,
        output = "latex",
        escape = FALSE),
    "escape-crosstab_latex_FALSE")

# datasummary_correlation escape rownames and colnames
# unicode minus signs break on windows
## TRUE
expect_snapshot_print(datasummary_correlation(dat, output = "latex"),
    "escape-correlation_latex")
## FALSE
expect_snapshot_print(datasummary_correlation(dat, output = "latex", escape = FALSE),
    "escape-correlation_latex_FALSE")

# TODO: Why does this print to console?
## TRUE
# expect_snapshot_print(datasummary_correlation(dat, output = "html"),
#     "escape-correlation_html")
# ## FALSE
# expect_snapshot_print(print(datasummary_correlation(dat, output = "html", escape = FALSE)),
#     "escape-correlation_html_FALSE")

# Bugfix: escape & latex & coef_map
# Bug reported here:
# https://github.com/vincentarelbundock/modelsummary/issues/378
mtcars2 <- mtcars
mtcars2$disp_x <- mtcars2$disp
mtcars2$wt_x <- mtcars2$wt

# Problem 1:  When there is a variable with underscore raw name not
# displayed in output; not in expected order
reg_prob1 <- list()
reg_prob1[[1]] <- lm(data = mtcars2, mpg ~ disp_x)
reg_prob1[[2]] <- lm(data = mtcars2, mpg ~ disp_x + wt)
reg_prob1[[3]] <- lm(data = mtcars2, mpg ~ disp_x + wt + am)
expect_snapshot_print(
    msummary(reg_prob1, coef_map = c("disp_x", "wt"), output = "latex"),
    "escape-modelsummary_latex")

# Problem 2: When there are two or more variables with underscores rows for
# coefficients and std errors are not in the right place and names are not
# displayed
reg_prob2 <- list()
reg_prob2[[1]] <- lm(data = mtcars2, mpg ~ disp_x)
reg_prob2[[2]] <- lm(data = mtcars2, mpg ~ disp_x + wt_x)
reg_prob2[[3]] <- lm(data = mtcars2, mpg ~ disp_x + wt_x + am)
expect_snapshot_print(
    msummary(reg_prob2, coef_map = c("disp_x", "wt_x"), output = "latex"),
    "escape-modelsummary_latex2")

# bugs stay dead: escape=FALSE w/ coef_map
url <- "https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv"
dat <- read.csv(url)
cm <- c(
    "Literacy" = "Literacy (\\%)",
    "Commerce" = "Patents per capita",
    "(Intercept)" = "Constant")
dat <- read.csv(url)
models <- list("OLS 1" = lm(Donations ~ Literacy + Commerce, data = dat))
expect_snapshot_print(modelsummary(models, coef_map = cm, output = "latex_tabular", escape = FALSE),
    "escape-modelsummary_latex_tabular_FALSE")

# column headers are not escaped with `escape=FALSE`
# unicode minus signs break on windows
mod <- list(
    "<code>lm()</code>" = lm(mpg ~ hp + drat, mtcars),
    "<code>lm_robust()</code>" = lm(mpg ~ hp + drat, mtcars))
expect_snapshot_print(modelsummary(mod,
    vcov = c("classical", "HC1"),
    escape = FALSE,
    output = "html"),
    "escape-modelsummary_html")

# Issue 546: escape gof names
requiet("fixest")
dat <- mtcars
dat$cyl_new <- dat$cyl
mod <- feols(mpg ~ hp | cyl_new, data = dat)
tab <- modelsummary(mod, "latex")
expect_true(grepl("cyl\\_new", tab, fixed = TRUE))


# Issue #622: shape = 'rbind' with escape TRUE/FALSE
tmp <- transform(mtcars, a_b = qsec, b_c = hp)
panels <- list(
    "Panel B: $\\log(3)^2$" = list(
        "A_B" = lm(mpg ~ a_b, data = tmp),
        "B_C" = lm(mpg ~ a_b + b_c, data = tmp)),
    "Panel B: $\\log(3)_2$" = list(
        "A_B" = lm(disp ~ a_b, data = tmp),
        "B_C" = lm(disp ~ a_b + b_c, data = tmp)))
expect_snapshot_print(
    modelsummary(panels, output = "latex_tabular", shape = "rbind", title = "$\\beta_1$", escape = FALSE),
    "escape-panel_escape_FALSE")
expect_snapshot_print(
    modelsummary(panels, output = "latex_tabular", shape = "rbind", title = "$\\beta_1$", escape = TRUE),
    "escape-panel_escape_TRUE")


# Issue #594: escape LaTeX label
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")
mod <- lm(mpg ~ hp, mtcars)
expect_snapshot_print(
    modelsummary(mod, "latex", title = "Blah_blah \\label{tab:blah-blah}"),
    "test-escape_label_title")

