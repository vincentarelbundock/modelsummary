# escape group names

    Code
      datasummary_balance(~vs, data = dat, output = "latex", escape = TRUE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrrrrrr}
      \toprule
      \multicolumn{1}{c}{ } & \multicolumn{2}{c}{no\_no (N=18)} & \multicolumn{2}{c}{yes\_yes (N=14)} & \multicolumn{2}{c}{ } \\
      \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5}
        & Mean & Std. Dev. & Mean  & Std. Dev.  & Diff. in Means & Std. Error\\
      \midrule
      mpg & 16.6 & 3.9 & 24.6 & 5.4 & 7.9 & 1.7\\
      cyl & 7.4 & 1.1 & 4.6 & 0.9 & -2.9 & 0.4\\
      disp & 307.1 & 106.8 & 132.5 & 56.9 & -174.7 & 29.4\\
      hp & 189.7 & 60.3 & 91.4 & 24.4 & -98.4 & 15.6\\
      drat & 3.4 & 0.5 & 3.9 & 0.5 & 0.5 & 0.2\\
      wt & 3.7 & 0.9 & 2.6 & 0.7 & -1.1 & 0.3\\
      qsec & 16.7 & 1.1 & 19.3 & 1.4 & 2.6 & 0.4\\
      am & 0.3 & 0.5 & 0.5 & 0.5 & 0.2 & 0.2\\
      gear & 3.6 & 0.9 & 3.9 & 0.5 & 0.3 & 0.2\\
      carb & 3.6 & 1.5 & 1.8 & 1.1 & -1.8 & 0.5\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      datasummary_balance(~vs, data = dat, output = "latex", escape = FALSE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrrrrrr}
      \toprule
      \multicolumn{1}{c}{ } & \multicolumn{2}{c}{no_no (N=18)} & \multicolumn{2}{c}{yes_yes (N=14)} & \multicolumn{2}{c}{ } \\
      \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5}
        & Mean & Std. Dev. & Mean  & Std. Dev.  & Diff. in Means & Std. Error\\
      \midrule
      mpg & 16.6 & 3.9 & 24.6 & 5.4 & 7.9 & 1.7\\
      cyl & 7.4 & 1.1 & 4.6 & 0.9 & -2.9 & 0.4\\
      disp & 307.1 & 106.8 & 132.5 & 56.9 & -174.7 & 29.4\\
      hp & 189.7 & 60.3 & 91.4 & 24.4 & -98.4 & 15.6\\
      drat & 3.4 & 0.5 & 3.9 & 0.5 & 0.5 & 0.2\\
      wt & 3.7 & 0.9 & 2.6 & 0.7 & -1.1 & 0.3\\
      qsec & 16.7 & 1.1 & 19.3 & 1.4 & 2.6 & 0.4\\
      am & 0.3 & 0.5 & 0.5 & 0.5 & 0.2 & 0.2\\
      gear & 3.6 & 0.9 & 3.9 & 0.5 & 0.3 & 0.2\\
      carb & 3.6 & 1.5 & 1.8 & 1.1 & -1.8 & 0.5\\
      \bottomrule
      \end{tabular}
      \end{table}

