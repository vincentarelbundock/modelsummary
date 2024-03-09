source("helpers.R")
requiet("estimatr")
requiet("tinysnapshot")
using("tinysnapshot")

# escape group names
dat <- mtcars
dat$vs <- ifelse(dat$vs == 1, "yes_yes", "no_no")
expect_snapshot_print(
  datasummary_balance(~vs, data = dat, output = "latex", escape = TRUE),
  "datasummary_balance-escape_TRUE")
expect_snapshot_print(
  datasummary_balance(~vs, data = dat, output = "latex", escape = FALSE),
  "datasummary_balance-escape_FALSE")

# stub is escaped in latex
tmp <- data.frame(
  under_score = rnorm(100),
  X = sample(0:1, 100, replace = TRUE))
tab <- datasummary_balance(~X, data = tmp, output = "latex")
expect_true(grepl("under\\\\_score", tab))

# errors and warnings
# missing RHS variable
tmp <- data.frame(ID = as.character(1:100),
Y = rnorm(100),
Z_comp = sample(0:20, 100, replace = TRUE))
expect_error(datasummary_balance(~k, tmp),
         pattern = "must be in data")

# too many factor levels in condition variable
expect_error(datasummary_balance(~Z_comp, tmp),
         pattern = "wide to be readable")

# warn about drops
tmp <- data.frame(
Y = rnorm(100),
K = NA_real_,
Z_comp = sample(0:1, 100, replace = TRUE))
expect_warning(datasummary_balance(~Z_comp, tmp),
           pattern = "entirely missing")

tmp <- data.frame(
Y = rnorm(100),
K = as.character(1:100),
Z_comp = sample(0:1, 100, replace = TRUE))
expect_warning(datasummary_balance(~Z_comp, tmp),
           pattern = "include more than 50")

# column percentages sum to 100 within factors
dat <- mtcars[, c("vs", "cyl", "gear")]
dat$cyl <- factor(dat$cyl)
dat$gear <- factor(dat$gear)
tab <- datasummary_balance(~vs, dat, output="dataframe")
idx <- c(rep("cyl", 3), rep("gear", 3))
f <- function(x) round(sum(x)) == 100
expect_true(all(tapply(as.numeric(tab[[4]]), idx, f)))
expect_true(all(tapply(as.numeric(tab[[6]]), idx, f)))

# factor formatting
dat <- mtcars[, c("vs", "cyl", "gear")]
dat$cyl <- factor(dat$cyl)
dat$gear <- factor(dat$gear)
tab <- datasummary_balance(~vs, dat, output="dataframe")
expect_equivalent(tab[[4]][1:2], c("5.6", "16.7"))

# palmer penguins was once broken with kableExtra
options(modelsummary_factory_html = "kableExtra")
penguins <- "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv"
penguins <- read.csv(penguins)
raw <- datasummary_balance(~sex, penguins, output = "html", dinm = FALSE)
expect_inherits(raw, "knitr_kable")
options(modelsummary_factory_html = NULL)

# variable name with spaces
tmp <- mtcars
colnames(tmp)[1] <- "testing spaces"
tab <- datasummary_balance(~vs, data = tmp, output = "dataframe")
expect_inherits(tab, "data.frame")
tab <- datasummary_balance(~vs, data = tmp, output = "dataframe")
expect_equivalent(dim(tab), c(10, 7))

# add column
tmp <- mtcars
tmp$gear <- as.factor(tmp$gear)
tmp$vs <- as.logical(tmp$vs)
tab <- datasummary_balance(~vs, 
data = tmp, 
output = 'dataframe',
add_columns = data.frame(k=letters[1:13]), z=1:13)
expect_equivalent(dim(tab), c(13, 9))

# only numeric
tab <- datasummary_balance(~vs, mtcars, output = 'dataframe')
truth <- c(" ", "0 / Mean", "0 / Std. Dev.", "1 / Mean",
"1 / Std. Dev.", "Diff. in Means", "Std. Error")
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(10, 7))
expect_equivalent(colnames(tab), truth)

# only factors
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
tmp$gear <- factor(tmp$gear)
tmp$vs <- as.logical(tmp$vs)
tmp <- tmp[, c('am', 'vs', 'cyl', 'gear')]
tab <- datasummary_balance(~am, tmp, output = 'dataframe')
truth <- c("", "", "0 / N", "0 / Pct.", "1 / N", "1 / Pct.")
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(8, 6))
expect_equivalent(trimws(colnames(tab)), truth)

# both factors and numerics
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
tmp$gear <- factor(tmp$gear)
tmp$vs <- as.logical(tmp$vs)
tab <- datasummary_balance(~am, tmp, output = 'dataframe')
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(16, 8))
## col order 
truth <- c(" ", "  ", "0 / Mean", "0 / Std. Dev.", "1 / Mean", "1 / Std. Dev.", "Diff. in Means", "Std. Error")
expect_equivalent(colnames(tab), truth)
## row order with mid header
expect_equivalent(tab[[1]], c("mpg", "disp", "hp", "drat", "wt", "qsec", "carb", "", "cyl", "", "", "vs", "", "gear", "", ""))
expect_equivalent(tab[[2]], c("", "", "", "", "", "", "", "", "4", "6", "8", "FALSE", "TRUE", "3", "4", "5"))

# more than two conditions
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tab <- datasummary_balance(~gear, tmp, output = 'dataframe', dinm = FALSE)
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(14, 8))
expect_warning(datasummary_balance(~gear, tmp, output = 'dataframe', dinm = TRUE))
tab <- suppressWarnings(datasummary_balance(~gear, tmp, output = 'dataframe', dinm = TRUE))
expect_equivalent(dim(tab), c(14, 8))

# no conditions - grand summary
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tab <- datasummary_balance(~1, tmp, output = 'dataframe', dinm = FALSE)
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(15, 4))
tab <- suppressWarnings(datasummary_balance(~1, tmp, output = 'dataframe', dinm = TRUE))
expect_equivalent(dim(tab), c(15, 4))


# single numeric
tmp <- mtcars[, c('am', 'mpg')]
tab <- datasummary_balance(~am, data = tmp, output = 'dataframe')
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(1, 7))
expect_equivalent(tab[[1]], 'mpg')



# single factor has two stub columns
tmp <- mtcars[, c('am', 'gear')]
tmp$gear <- factor(tmp$gear)
tab <- datasummary_balance(~am, data = tmp, output = 'dataframe')
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(3, 6))
expect_equivalent(tab[[2]][1], '3')



# dinm=FALSE
tab <- datasummary_balance(~vs, mtcars, dinm = FALSE, output = 'dataframe')
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(10, 5))
expect_equivalent(tab[[1]][1], 'mpg')



# dinm_statistic = "p.value"
tab <- datasummary_balance(
~vs,
data = mtcars,
dinm_statistic = 'p.value',
output = 'dataframe')
expect_inherits(tab, 'data.frame')
expect_equivalent(dim(tab), c(10, 7))
expect_equivalent(tab[[1]][1], 'mpg')
expect_equivalent(colnames(tab)[ncol(tab)], 'p')

# sanity checks prevent other than p.value or std.error
expect_error(
datasummary_balance(~vs, mtcars, dinm_statistic = 'bad', output = 'dataframe'))



# fmt
tmp <- mtcars[, c('am', 'mpg', 'gear')]
tmp$gear <- factor(tmp$gear)
truth <- c("17.15", "N", "15", "4", "0")
tab <- datasummary_balance(~am, tmp, fmt = "%.2f", output = 'dataframe')
expect_equivalent(tab[[3]], truth)



# too many levels in row variable
set.seed(10)
dat <- data.frame(ID = as.character(1:100),
Y = rnorm(100),
Z_comp = sample(0:1, 100, replace = TRUE))
expect_warning(datasummary_balance(~Z_comp, dat))



# estimatr: clusters, blocks, weights

set.seed(286342)
# clusters
dat <- data.frame(ID = as.character(1:100),
Y = rnorm(100),
Z_comp = sample(0:1, 100, replace = TRUE))
dat$clusters <- sample(20, size = nrow(dat), replace = TRUE)
idx <- sample(unique(dat$clusters), 12)
dat$Z_clust <- as.numeric(dat$clusters %in% idx)
dat$ID <- NULL

truth <- estimatr::difference_in_means(Y ~ Z_clust, clusters = clusters, data = dat)
truth <- estimatr::tidy(truth)

tab <- datasummary_balance(~Z_clust, dat, fmt = "%.6f", output = 'dataframe')
expect_equivalent(tab[1, ncol(tab)], sprintf("%.6f", truth$std.error))

# blocks
dat$block <- rep(1:5, each = 20) # hardcoded name in estimatr

dat$Z_block <- unlist(tapply(dat$block, dat$block, function(z) rbinom(length(z), 1, .5)))

dat$blocks <- dat$block # hardcoded name in datasummary_balance
dat$clusters <- NULL

truth <- estimatr::difference_in_means(Y ~ Z_block, blocks = block, data = dat)
truth <- sprintf("%.6f", tidy(truth)$std.error)

tab <- datasummary_balance(~Z_block, dat, fmt = "%.6f", output = 'dataframe')
expect_equivalent(tab[1, ncol(tab)], truth)

#############
#  weights  #
#############

# error on missing data in weights
set.seed(1024)
dat <- datw <- mtcars
datw$weights <- runif(nrow(datw))
datw$weights[2] <- NA
expect_error(datasummary_balance(~ am, data = datw), pattern = "include missing data")

# warning: weights not supported for categorical
set.seed(1024)
datw <- mtcars
datw$weights <- runif(nrow(datw))
datw$am <- factor(datw$am)
datw$cyl <- factor(datw$cyl)
expect_warning(datasummary_balance(~ am, data = datw), pattern = "However")

# numeric weights
set.seed(1024)
dat <- datw <- mtcars
datw$weights <- runif(nrow(datw))
tab <- datasummary_balance(~ am, data = datw, output = "data.frame", fmt = NULL)
tab_noweights <- datasummary_balance(~ am, data = dat, output = "data.frame", fmt = NULL)

# weights and no weights give different results
for (i in 2:ncol(tab)) {
  expect_true(all(tab[[i]] != tab_noweights[[i]]))
}

# mean: manual
for (am in 0:1) {
  idx <- datw$am == am
  for (v in tab[[1]]) {
    unknown <- weighted.mean(datw[[v]][idx], datw$weights[idx])
    expect_equivalent(unknown, tab[tab[[1]] == v, sprintf("%s / Mean", am)])
  }
}

# sd: manual
for (am in 0:1) {
  idx <- datw$am == am
  for (v in tab[[1]]) {
    unknown <- modelsummary:::weighted_sd(datw[[v]][idx], datw$weights[idx])
    expect_equivalent(unknown, tab[tab[[1]] == v, sprintf("%s / Std. Dev.", am)])
  }
}

######################
#  various datasets  #
######################
# datasummary_balance: various datasets
data(PlantGrowth)
tab <- datasummary_balance(~group, PlantGrowth, output = 'dataframe', dinm = FALSE)
expect_equivalent(tab[1, 2], '5.0')
expect_equivalent(tab[1, 3], '0.6')
expect_equivalent(tab[1, 4], '4.7')
expect_equivalent(tab[1, 5], '0.8')
expect_equivalent(tab[1, 6], '5.5')
expect_equivalent(tab[1, 7], '0.4')

# Issue #711: stars and d-columns
requiet("dplyr")
training <- 'https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Treatment.csv'
training <- read.csv(training, na.strings = "")
training <- training %>%
  dplyr::mutate(`Earnings Before` = re75 / 1000,
         `Earnings After` = re78 / 1000,
         Treatment = ifelse(treat == TRUE, 'Treatment', 'Control'),
         Married = ifelse(married == TRUE, 'Yes', 'No')) %>%
  dplyr::select(`Earnings Before`,
         `Earnings After`,
         Treatment,
         Ethnicity = ethn,
         Age = age,
         Education = educ,
         Married)
tab <- datasummary_balance(~ Treatment, training, fmt = 3, stars = TRUE, align = "lllllldl", output = "latex")
expect_snapshot_print(tab, "datasummary_balance-issue711")
