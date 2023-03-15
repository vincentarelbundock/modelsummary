source("helpers.R")
requiet("estimatr")
requiet("flextable")
if (ON_CI) exit_file("CI")

mod <- list(
lm(hp ~ mpg, mtcars),
lm(hp ~ mpg + drat, mtcars))

# output='table.tex'
fn <- paste0(random_string(), ".tex")
modelsummary(mod, output = fn)
known <- readLines("known_output/output_1.tex")
unknown <- readLines(fn)
expect_identical(known, unknown)
unlink(fn)

# table objects from different packages: no error
otp <- c("gt", "kableExtra", "flextable", "huxtable", "html", "latex", "markdown")
for (o in otp) {
  tab <- modelsummary(mod, output = o)
}

# unsupported output
expect_error(modelsummary(mod, 'bad'))
expect_error(modelsummary(mod, 'htm'))
expect_error(modelsummary(mod, 't.est'))

# supported global options

random <- random_string()

# RTF
filename <- paste0(random, '.rtf')
options(modelsummary_factory_rtf = 'gt')
tab <- modelsummary(mod, filename)
unlink(filename)

# HTML
filename <- paste0(random, '.html')
options(modelsummary_factory_html = 'gt')
tab <- modelsummary(mod, filename)
unlink(filename)
options(modelsummary_factory_html = 'kableExtra')
tab <- modelsummary(mod, filename)
unlink(filename)
options(modelsummary_factory_html = 'flextable')
tab <- modelsummary(mod, filename)
unlink(filename)
options(modelsummary_factory_html = 'huxtable')
tab <- modelsummary(mod, filename)
unlink(filename)
options(modelsummary_factory_html = 'kableExtra')



# unsupported global options

options(modelsummary_factory_rtf = 'kableExtra')
expect_error(modelsummary(mod, 'test.rtf'))
options(modelsummary_factory_rtf = 'gt')

options(modelsummary_factory_word = 'kableExtra')
expect_error(modelsummary(mod, 'test.docx'))
options(modelsummary_factory_word = 'gt')
expect_error(modelsummary(mod, 'test.docx'))
options(modelsummary_factory_word = 'flextable')

options(modelsummary_factory_powerpoint = 'kableExtra')
expect_error(modelsummary(mod, 'test.pptx'))
options(modelsummary_factory_powerpoint = 'gt')
expect_error(modelsummary(mod, 'test.pptx'))
options(modelsummary_factory_powerpoint = 'flextable')

options(modelsummary_factory_png = 'huxtable')
expect_error(modelsummary(mod, 'test.png'))
unlink("test.png")
options(modelsummary_factory_png = 'kableExtra')

options(modelsummary_factory_jpg = 'huxtable')
expect_error(modelsummary(mod, 'test.jpg'))
options(modelsummary_factory_jpg = 'gt')
expect_error(modelsummary(mod, 'test.jpg'))
options(modelsummary_factory_jpg = 'kableExtra')



# save to file
random <- random_string()

filename <- paste0(random, '.html')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.rtf')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.tex')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.md')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.docx')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.pptx')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.jpg')
tab <- modelsummary(mod, filename)
unlink(filename)

filename <- paste0(random, '.png')
tab <- modelsummary(mod, filename)
unlink(filename)

# overwrite file
random <- random_string()
filename <- paste0(random, '.html')
modelsummary(mod, filename)
modelsummary(mod, filename)
unlink(filename)


######################################
# datasummary_balance #
######################################

# output formats do not produce errors
tmp <- mtcars
tmp$cyl <- as.character(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
otp <- c("huxtable", "flextable", "kableExtra", "dataframe", "markdown", "latex", "html")
for (o in otp) {
  tab <- datasummary_balance(~am, tmp, output = o)
}


#  add_columns output formats work  #
tmp <- mtcars
tmp$cyl <- as.character(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
custom <- data.frame('a' = 1:2, 'b' = 1:2)
output_formats <- c('gt', 'kableExtra', 'flextable', 'huxtable', 'latex',
'markdown', 'html')
for (o in output_formats) {
  testname <- paste("add_columns with", o)
  tab <- datasummary_balance(~am, tmp, add_columns = custom, output = o)
}

#  save to file  #
tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext) {
  msg <- paste("save to", ext)
  random <- random_string()
  filename <- paste0(random, ext)
  tab <- datasummary_balance(~am, data = tmp, output = filename)
  unlink(filename)
}

for (ext in c('.html', '.tex', '.rtf', '.docx', '.pptx', '.jpg', '.png')) {
save_to_file(ext)
}


###################################
# "output: datasummary_skim"
###################################

dat <- mtcars
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)

# write to file
datasummary_skim(dat, output = "test.jpg")
datasummary_skim(dat, type = "categorical", output = "test.jpg")
datasummary_skim(dat, output = "test.png")
datasummary_skim(dat, type = "categorical", output = "test.png")
datasummary_skim(dat, output = "test.html")
datasummary_skim(dat, type = "categorical", output = "test.html")
expect_warning(datasummary_skim(dat, output = "test.tex"))
unlink("test.png")
unlink("test.jpg")
unlink("test.html")
unlink("test.tex")


# unsupported formats
expect_warning(datasummary_skim(dat, output="flextable"))
expect_warning(datasummary_skim(dat, output="gt"))
expect_warning(datasummary_skim(dat, output="huxtable"))
expect_warning(datasummary_skim(dat, output="latex"))



##########################################
# "output: datasummary_correlation"
##########################################

# output format do not produce errors

# output formats do not produce errors
otp <- c("gt", "kableExtra", "flextable", "huxtable", "html", "latex", "markdown")
for (o in otp) {
  datasummary_correlation(mtcars, output = o) 
}

tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext = ".html") {
  msg <- paste("datasummary_correlation: save to", ext)
  random <- random_string()
  filename <- paste0(random, ext)
  datasummary_correlation(tmp, output = filename)
  unlink(filename)
}

for (ext in c(".html", ".tex", ".rtf", ".docx", ".pptx", ".jpg", ".png")) {
  save_to_file(ext)
}