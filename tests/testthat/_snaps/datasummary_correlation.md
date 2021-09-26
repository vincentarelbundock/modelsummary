# different rows and columns

    Code
      dput(tab)
    Output
      structure(list(` ` = c("(1) mpg", "(2) hp", "(3) vs"), `(1)` = c("1.00", 
      "-.78", ".66"), `(2)` = c("-.78", "1.00", "-.72"), `(3)` = c(".66", 
      "-.72", "1.00")), row.names = c(NA, -3L), class = "data.frame", align = c("l", 
      "r", "r", "r"))

# pearson, kendall, spearman, pearspear

    Code
      dput(tab)
    Output
      structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.78"), hp = c(".", 
      "1")), row.names = c(NA, -2L), class = "data.frame", align = c("l", 
      "r", "r"))

---

    Code
      dput(tab)
    Output
      structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.74"), hp = c(".", 
      "1")), row.names = c(NA, -2L), class = "data.frame", align = c("l", 
      "r", "r"))

---

    Code
      dput(tab)
    Output
      structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.89"), hp = c(".", 
      "1")), row.names = c(NA, -2L), class = "data.frame", align = c("l", 
      "r", "r"))

---

    Code
      dput(tab)
    Output
      structure(list(` ` = c("mpg", "hp"), mpg = c("1", "-.89"), hp = c("-.78", 
      "1")), row.names = c(NA, -2L), class = "data.frame", align = c("l", 
      "r", "r"))

