library(modelsummary)
context('datasummary_table1')



test_that('datasummary_table1: various datasets', {
    data(PlantGrowth)
    tab <- datasummary_table1(~group, PlantGrowth, output = 'dataframe')
    expect_equal(tab[1, 2], '5.0')
    expect_equal(tab[1, 3], '(0.6)')
    expect_equal(tab[1, 4], '4.7')
    expect_equal(tab[1, 5], '(0.8)')
    expect_equal(tab[1, 6], '5.5')
    expect_equal(tab[1, 7], '(0.4)')
})

test_that('datasummary_table1: output format do not produce errors', {

    tmp <- mtcars
    tmp$cyl <- as.character(tmp$cyl)
    tmp$vs <- as.logical(tmp$vs)

    tab <- datasummary_table1(~gear, tmp, output='dataframe')
    expect_equal(dim(tab), c(14, 8))
    
    tab <- datasummary_table1(~am, tmp, output='dataframe')
    expect_equal(dim(tab), c(14, 6))
    
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


tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$vs <- as.logical(tmp$vs)
tmp$am <- as.character(tmp$am)
save_to_file <- function(ext) {
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

