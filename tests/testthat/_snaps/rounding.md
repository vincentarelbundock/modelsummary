# rounding cleans up NaN inside \num

    Code
      datasummary(cyl + mpg ~ SD + N, data = dat, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrr}
      \toprule
      cyl & SD & N\\
      \midrule
      4 &  & 11\\
      6 &  & 7\\
      8 &  & 14\\
      mpg & \num{6.03} & 32\\
      \bottomrule
      \end{tabular}
      \end{table}

# siunitx is disabled with glue

    Code
      modelsummary(mod, output = "latex", estimate = "{estimate} [{conf.low}, {conf.high}]")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lc}
      \toprule
        & Model 1\\
      \midrule
      (Intercept) & \num{24.708} [\num{18.292}, \num{31.124}]\\
       & (\num{3.132})\\
      hp & \num{-0.030} [\num{-0.060}, \num{-0.001}]\\
       & (\num{0.015})\\
      SD (Intercept) & \num{4.023}\\
      SD (Observations) & \num{3.149}\\
      \midrule
      Num.Obs. & \num{32}\\
      R2 Marg. & \num{0.143}\\
      R2 Cond. & \num{0.674}\\
      AIC & \num{181.9}\\
      BIC & \num{187.8}\\
      ICC & \num{0.6}\\
      RMSE & \num{2.96}\\
      \bottomrule
      \end{tabular}
      \end{table}

