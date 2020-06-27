library(modelsummary)

context('datasummary')

test_that('numeric content of simple tables', {

    # 2 rows 2 cols
    truth <- data.frame(c('mpg', 'hp'),
                        c("20.09", "146.69"), 
                        c("6.027", "68.563"))
    tab <- datasummary(mpg + hp ~ mean + sd, 
                       output = 'dataframe',
                       data = mtcars) 
    expect_true(all(truth == tab))

    # nested cols: 1 level
    truth <- data.frame(rows = c('mpg', 'hp'),
                        `0 mean` = c("17.15", "160.26"), 
                        `0 sd` = c("3.834", "53.908"), 
                        `1 mean` = c("24.39", "126.85"), 
                        `1 sd` = c("6.167", "84.062"))
    tab <- datasummary(mpg + hp ~ Factor(am) * (mean + sd), 
                       output = 'dataframe',
                       data = mtcars) 
    expect_true(all(truth == tab))

    # nested cols: 2 levels
    tab <- datasummary(mpg + hp ~ Factor(cyl) * Factor(am) * (mean + sd), 
                       output = 'dataframe',
                       data = mtcars) 
    expect_equal(dim(tab), c(2, 13))

    # nested rows: 1 level
    truth <- data.frame(am = c("0", "", "1", ""), 
                        b = c("mpg", "hp", "mpg", "hp"), 
                        mean = c("17.15", "160.26", "24.39", "126.85"), 
                        sd = c("3.834", "53.908", "6.167", "84.062"))
    tab <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd, 
                       output = 'dataframe',
                       data = mtcars) 
    expect_true(all(truth == tab))

    # nested rows: 2 levels
    tab <- datasummary(Factor(cyl) * Factor(am) * mpg + hp ~ mean + sd, 
                       output = 'dataframe',
                       data = mtcars) 
    expect_equal(dim(tab), c(7, 5))

})

test_that('Header carry-forward', {

    coln <- c(" ", "4 mean", "4 sd", "6 mean", "6 sd", "8 mean", "8 sd",
              " median")
    tab <- datasummary(mpg + hp ~ Factor(cyl) * (mean + sd) + median,
                       data = mtcars,
                       output = 'dataframe')
    expect_equal(coln, colnames(tab))

})

test_that('Factor() is equivalent to assign', {

    tab1 <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd, 
                        output = 'dataframe',
                        data = mtcars) 
    tmp <- mtcars
    tmp$am <- factor(tmp$am)
    tab2 <- datasummary(am * (mpg + hp) ~ mean + sd, 
                        output = 'dataframe',
                        data = tmp) 
    expect_identical(tab1, tab2)

})


test_that('logical and characters converted to factors automatically', {

    tmp <- mtcars
    tmp$am <- ifelse(tmp$am == 0, FALSE, TRUE)
    tmp$cyl <- as.character(tmp$cyl)
    tab <- datasummary(cyl * am * (mpg + hp) ~ mean + sd, 
                       output = 'dataframe',
                       data = tmp) 
    expect_equal(dim(tab), c(12, 5))

})

test_that('datasummary_table1: various datasets', {

    data(PlantGrowth)
    tab <- datasummary_table1(~group, PlantGrowth, output = 'dataframe')
    expect_equal(tab[, 3], c('', '5.0'))
    expect_equal(tab[, 4], c('', '(0.6)'))
    expect_equal(tab[, 5], c('', '4.7'))
    expect_equal(tab[, 6], c('', '(0.8)'))
    expect_equal(tab[, 7], c('', '5.5'))
    expect_equal(tab[, 8], c('', '(0.4)'))

})

test_that('datasummary_table1: output format do not produce errors', {

    tmp <- mtcars
    tmp$cyl <- as.character(tmp$cyl)
    tmp$vs <- as.logical(tmp$vs)

    tab <- datasummary_table1(~gear, tmp, output='dataframe')
    expect_equal(dim(tab), c(15, 8))
    
    tab <- datasummary_table1(~am, tmp, output='dataframe')
    expect_equal(dim(tab), c(15, 6))
    
    # output formats do not produce errors
    expect_error(datasummary_table1(~am, tmp, output='huxtable'), NA)
    expect_error(datasummary_table1(~am, tmp, output='flextable'), NA)
    expect_error(datasummary_table1(~am, tmp, output='kableExtra'), NA)
    expect_error(datasummary_table1(~am, tmp, output='huxtable'), NA)
    expect_error(datasummary_table1(~am, tmp, output='dataframe'), NA)
    expect_error(datasummary_table1(~am, tmp, output='markdown'), NA)
    expect_error(datasummary_table1(~am, tmp, output='latex'), NA)
    expect_error(datasummary_table1(~am, tmp, output='html'), NA)


})

test_that('datasummary_correlation: output format do not produce errors', {
    
    # output formats do not produce errors
    expect_error(datasummary_correlation(mtcars, output='huxtable'), NA)
    expect_error(datasummary_correlation(mtcars, output='flextable'), NA)
    expect_error(datasummary_correlation(mtcars, output='huxtable'), NA)
    expect_error(datasummary_correlation(mtcars, output='kableExtra'), NA)
    expect_error(datasummary_correlation(mtcars, output='dataframe'), NA)
    expect_error(datasummary_correlation(mtcars, output='markdown'), NA)
    expect_error(datasummary_correlation(mtcars, output='latex'), NA)
    expect_error(datasummary_correlation(mtcars, output='html'), NA) 
    
})

test_that('datasummary: output format do not produce errors', {

    # output formats do not produce errors
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='huxtable'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='flextable'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='kableExtra'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='huxtable'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='dataframe'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='markdown'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='latex'), NA)
    expect_error(datasummary(All(mtcars) ~ Mean + SD, mtcars, output='html'), NA) 

})


######################
#  datasummary_skim  #
######################

context('datasummary_skim')

test_that('datasummary_skim type', {

    tmp_num <- mtcars[, c('mpg', 'hp')]
    tmp_cat <- mtcars[, c('cyl', 'am')]
    tmp_cat$cyl <- as.factor(tmp_cat$cyl)
    tmp_cat$am <- as.logical(tmp_cat$am)

    expect_error(datasummary_skim(tmp_num, type = 'categorical'))
    expect_error(datasummary_skim(tmp_cat, type = 'categorical'), NA)
    expect_error(datasummary_skim(tmp_num, type = 'numeric'), NA)
    expect_error(datasummary_skim(tmp_cat, type = 'numeric'))

})




#######################################
#  datasummary_table1: write to file  #
#######################################
context('datasummary write to file')

tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext = '.html') {
    msg <- paste('datasummary_table1: save to', ext)
    test_that(msg, {
        random <- stringi::stri_rand_strings(1, 30)
        filename <- paste0(random, ext)
        expect_error(datasummary_table1(~gear, data = tmp, output = filename), NA)
        unlink(filename)
    })
    
}
for (ext in c('.html', '.tex', '.rtf', '.docx', '.pptx', '.jpg', '.png')) {
    save_to_file(ext)
}

############################################
#  datasummary_correlation: write to file  #
############################################
tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext = '.html') {
    msg <- paste('datasummary_correlation: save to', ext)
    test_that(msg, {
        random <- stringi::stri_rand_strings(1, 30)
        filename <- paste0(random, ext)
        expect_error(datasummary_correlation(tmp, output = filename), NA)
        unlink(filename)
    })
    
}
for (ext in c('.html', '.tex', '.rtf', '.docx', '.pptx', '.jpg', '.png')) {
    save_to_file(ext)
}
