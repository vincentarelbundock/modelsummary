# kable markdown: complex table

    Code
      modelsummary(models, coef_map = cm, stars = TRUE, gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
        title = "Summarizing 5 statistical models using the `modelsummary` package for `R`.",
        notes = c("First custom note to contain text.",
          "Second custom note with different content."), output = "markdown")
    Output
      
      
      Table: Summarizing 5 statistical models using the `modelsummary` package for `R`.
      
      |                |   OLS 1   | Poisson 1 |   OLS 2   | Logit 1  | Logit 2 |
      |:---------------|:---------:|:---------:|:---------:|:--------:|:-------:|
      |Horsepower      |           |           | -0.005*** | -0.089*  | 0.122+  |
      |                |           |           |  (0.001)  | (0.039)  | (0.068) |
      |Miles/Gallon    | -9.417**  | -0.078*** |           |          |         |
      |                |  (2.676)  |  (0.004)  |           |          |         |
      |Weight          |  -4.168   |           |  -0.072   |          |         |
      |                | (16.485)  |           |  (0.087)  |          |         |
      |Rear axle ratio |           | 0.139***  |           |  -1.717  |         |
      |                |           |  (0.038)  |           | (1.909)  |         |
      |Displacement    |           |           |           |          | -0.095* |
      |                |           |           |           |          | (0.048) |
      |Constant        | 349.287** | 5.972***  | 1.349***  |  17.100  |  1.403  |
      |                | (103.509) |  (0.105)  |  (0.219)  | (10.843) | (1.368) |
      |Num.Obs.        |    32     |    32     |    32     |    32    |   32    |
      |R2              |   0.603   |           |   0.534   |          |         |
      |R2 Adj.         |   0.576   |           |   0.502   |          |         |
      |AIC             |   338.8   |   511.7   |   29.5    |   21.9   |  22.7   |
      |BIC             |   344.6   |   516.1   |   35.4    |   26.3   |  27.1   |
      |F               |  22.053   |  301.667  |  16.601   |  3.013   |  2.617  |
      |RMSE            |   42.50   |   39.22   |   0.34    |   0.29   |  0.30   |
      
      __Note:__
      ^^ + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001
      
      __Note:__
      ^^ First custom note to contain text.
      
      __Note:__
      ^^ Second custom note with different content.

# kable markdown: rouding + custom stars

    Code
      modelsummary(models, stars = c(`+` = 0.1, `*` = 0.01), fmt = "%.8f", output = "markdown")
    Output
      
      
      |            |     OLS 1      |  Poisson 1   |    OLS 2     |    Logit 1    |   Logit 2    |
      |:-----------|:--------------:|:------------:|:------------:|:-------------:|:------------:|
      |(Intercept) | 349.28734855*  | 5.97181727*  | 1.34867726*  |  17.09975551  |  1.40342203  |
      |            | (103.50934696) | (0.10521726) | (0.21935248) | (10.84261865) | (1.36756660) |
      |mpg         |  -9.41684544*  | -0.07770591* |              |               |              |
      |            |  (2.67621950)  | (0.00368319) |              |               |              |
      |wt          |  -4.16801248   |              | -0.07150616  |               |              |
      |            | (16.48455540)  |              | (0.08681057) |               |              |
      |drat        |                | 0.13881387*  |              |  -1.71683105  |              |
      |            |                | (0.03805311) |              | (1.90934366)  |              |
      |hp          |                |              | -0.00464337* | -0.08901954+  | 0.12170173+  |
      |            |                |              | (0.00123887) | (0.03933000)  | (0.06777320) |
      |disp        |                |              |              |               | -0.09517972+ |
      |            |                |              |              |               | (0.04800283) |
      |Num.Obs.    |       32       |      32      |      32      |      32       |      32      |
      |R2          |     0.603      |              |    0.534     |               |              |
      |R2 Adj.     |     0.576      |              |    0.502     |               |              |
      |AIC         |     338.8      |    511.7     |     29.5     |     21.9      |     22.7     |
      |BIC         |     344.6      |    516.1     |     35.4     |     26.3      |     27.1     |
      |Log.Lik.    |    -165.392    |   -252.848   |   -10.764    |    -7.928     |    -8.356    |
      |F           |     22.053     |   301.667    |    16.601    |     3.013     |    2.617     |
      |RMSE        |     42.50      |    39.22     |     0.34     |     0.29      |     0.30     |
      
      __Note:__
      ^^ + p < 0.1, * p < 0.01

