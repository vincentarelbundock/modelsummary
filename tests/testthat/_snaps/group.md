# Michael E Flynn ultra-niche bug check

    Code
      cat(tab)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lcccc}
      \toprule
      \multicolumn{1}{c}{ } & \multicolumn{2}{c}{a} & \multicolumn{2}{c}{b} \\
      \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5}
        & 6 & 8 & 6 & 8\\
      \midrule
      (Intercept) & \num{47.252} & \num{72.440} & \num{89.573} & \num{117.971}\\
       & (\num{34.975}) & (\num{37.175}) & (\num{86.884}) & (\num{87.998})\\
      under\_score & \num{-2.205} & \num{-3.580} & \num{-3.627} & \num{-4.838}\\
       & (\num{1.638}) & (\num{1.775}) & (\num{3.869}) & (\num{3.915})\\
      drat &  &  & \num{-3.210} & \num{-5.028}\\
       &  &  & (\num{3.810}) & (\num{4.199})\\
      \midrule
      Num.Obs. & \num{32} &  & \num{32} & \\
      R2 & \num{0.763} &  & \num{0.815} & \\
      R2 Adj. & \num{0.733} &  & \num{0.786} & \\
      AIC & \num{24.1} &  & \num{24.5} & \\
      BIC & \num{30.0} &  & \num{33.3} & \\
      RMSE & \num{0.24} &  & \num{0.20} & \\
      \bottomrule
      \end{tabular}
      \end{table}

