# Test


```r

library(gt)
library(MASS)
library(gtsummary)
url = 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat = read.csv(url)
models = list()
models[['OLS 1']] = lm(Literacy ~ Crime_prop + Infants, dat)
models[['NBin 1']] = glm.nb(Literacy ~ Crime_prop + Donations, dat)
models[['OLS 2']] = lm(Desertion ~ Crime_prop + Infants, dat)
models[['NBin 2']] = glm.nb(Desertion ~ Crime_prop + Donations, dat)

gtsummary(models, 
          title = 'This is the title', 
          subtitle = 'This is the subtitle',
          filename = '~/Downloads/test.html')

gtsummary(models, 
          title = 'This is the title', 
          subtitle = 'This is the subtitle') %>%
    tab_source_note('A first note at the bottom') %>%
    tab_source_note('A second note at the bottom')  %>%
    tab_spanner('Literacy', columns = c('Model 1', 'Model 2')) %>%
    tab_spanner('Desertion', columns = c('Model 3', 'Model 4')) %>%
    as_raw_html() %>%
    cat(file = '~/Downloads/test.html')


extract(models) %>%
    gt() %>%
    tab_row_group(group = '', rows = 1:8) %>%
    tab_row_group(rows = 9:21) %>%
    as_rtf() %>% 
    cat(file = '~/Downloads/test.rtf')



```

# Customization
#gt::tab_source_note('Source note text 1') %>%
#gt::tab_source_note('Source note text 2') 
