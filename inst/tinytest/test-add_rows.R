source("helpers.R")
requiet("tibble")

mod <- list()

# exit_file("<<-")

dat <- mtcars
dat$cyl <- factor(dat$cyl)
# dat <<- dat
mod$OLS <- lm(am ~ cyl, data = dat)
mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

# data.frame
rows = read.csv(
  text = "term      , OLS , Logit
   cyl4      , -   , -
   NEW GOF 1 , ?   , ?
   NEW GOF 2 , X   , X
   NEW GOF 3 , Y   , Y"
)
attr(rows, "position") <- c(3, 8, 9, 12)
tab <- modelsummary(mod, add_rows = rows, output = "data.frame")
expect_equivalent(ncol(tab), 5)
expect_true(nrow(tab) > 14)

# add_rows numeric are formatted by fmt
tmp <- data.frame(a = 1:2, b = 2:3, c = 3:4)
tab <- datasummary(
  hp + mpg ~ mean + sd,
  data = mtcars,
  add_rows = tmp,
  fmt = "%.0f",
  output = "dataframe"
)
expect_equivalent(tab$sd, c("69", "6", "3", "4"))


# Issue #875: misplaced hrule
models <- list()
models[['OLS']] <- lm(mpg ~ factor(cyl), mtcars)
models[['Logit']] <- glm(am ~ factor(cyl), mtcars, family = binomial)
rows <- tribble(
  ~term,
  ~OLS,
  ~Logit,
  'factor(cyl)4',
  '-',
  '-',
  'Info',
  '???',
  'XYZ'
)
attr(rows, 'position') <- c(3, 9)
sty <- modelsummary(models, add_rows = rows)@style
sty <- subset(sty, line == "b")
expect_true(all(sty$i == 7))
