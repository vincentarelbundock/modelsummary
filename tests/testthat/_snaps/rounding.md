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
      4 & \num{} & 11\\
      6 & \num{} & 7\\
      8 & \num{} & 14\\
      mpg & \num{6.03} & 32\\
      \bottomrule
      \end{tabular}
      \end{table}

