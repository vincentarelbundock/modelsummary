# d-column: known output

    Code
      modelsummary(mod, align = "ldd", output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{ldd}
      \toprule
        & {Model 1} & {Model 2}\\
      \midrule
      (Intercept) & 30.099 & 10.790\\
       & (1.634) & (5.078)\\
      hp & -0.068 & -0.052\\
       & (0.010) & (0.009)\\
      drat & {} & 4.698\\
       & {} & (1.192)\\
      \midrule
      Num.Obs. & 32 & 32\\
      R2 & 0.602 & 0.741\\
      R2 Adj. & 0.589 & 0.723\\
      AIC & 181.2 & 169.5\\
      BIC & 185.6 & 175.4\\
      Log.Lik. & -87.619 & -80.752\\
      F & 45.460 & 41.522\\
      \bottomrule
      \end{tabular}
      \end{table}

# HTML global options

    Code
      modelsummary(mod, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> Model 1 </th>
         <th style="text-align:center;"> Model 2 </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> 30.099 </td>
         <td style="text-align:center;"> 10.790 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (1.634) </td>
         <td style="text-align:center;"> (5.078) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:center;"> −0.068 </td>
         <td style="text-align:center;"> −0.052 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (0.010) </td>
         <td style="text-align:center;"> (0.009) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;"> 4.698 </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (1.192) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> 32 </td>
         <td style="text-align:center;"> 32 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> 0.602 </td>
         <td style="text-align:center;"> 0.741 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> 0.589 </td>
         <td style="text-align:center;"> 0.723 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> 181.2 </td>
         <td style="text-align:center;"> 169.5 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> 185.6 </td>
         <td style="text-align:center;"> 175.4 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> −87.619 </td>
         <td style="text-align:center;"> −80.752 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> 45.460 </td>
         <td style="text-align:center;"> 41.522 </td>
        </tr>
      </tbody>
      </table>

---

    Code
      modelsummary(mod, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> Model 1 </th>
         <th style="text-align:center;"> Model 2 </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> $30.099$ </td>
         <td style="text-align:center;"> $10.790$ </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> ($1.634$) </td>
         <td style="text-align:center;"> ($5.078$) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:center;"> $-0.068$ </td>
         <td style="text-align:center;"> $-0.052$ </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> ($0.010$) </td>
         <td style="text-align:center;"> ($0.009$) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;"> $4.698$ </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> ($1.192$) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> $32$ </td>
         <td style="text-align:center;"> $32$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> $0.602$ </td>
         <td style="text-align:center;"> $0.741$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> $0.589$ </td>
         <td style="text-align:center;"> $0.723$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> $181.2$ </td>
         <td style="text-align:center;"> $169.5$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> $185.6$ </td>
         <td style="text-align:center;"> $175.4$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> $-87.619$ </td>
         <td style="text-align:center;"> $-80.752$ </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> $45.460$ </td>
         <td style="text-align:center;"> $41.522$ </td>
        </tr>
      </tbody>
      </table>

---

    Code
      modelsummary(mod, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> Model 1 </th>
         <th style="text-align:center;"> Model 2 </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> 30.099 </td>
         <td style="text-align:center;"> 10.790 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (1.634) </td>
         <td style="text-align:center;"> (5.078) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:center;"> -0.068 </td>
         <td style="text-align:center;"> -0.052 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (0.010) </td>
         <td style="text-align:center;"> (0.009) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;"> 4.698 </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (1.192) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> 32 </td>
         <td style="text-align:center;"> 32 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> 0.602 </td>
         <td style="text-align:center;"> 0.741 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> 0.589 </td>
         <td style="text-align:center;"> 0.723 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> 181.2 </td>
         <td style="text-align:center;"> 169.5 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> 185.6 </td>
         <td style="text-align:center;"> 175.4 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> -87.619 </td>
         <td style="text-align:center;"> -80.752 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> 45.460 </td>
         <td style="text-align:center;"> 41.522 </td>
        </tr>
      </tbody>
      </table>

# LaTeX global options

    Code
      modelsummary(mod, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lcc}
      \toprule
        & Model 1 & Model 2\\
      \midrule
      (Intercept) & \num{30.099} & \num{10.790}\\
       & (\num{1.634}) & (\num{5.078})\\
      hp & \num{-0.068} & \num{-0.052}\\
       & (\num{0.010}) & (\num{0.009})\\
      drat &  & \num{4.698}\\
       &  & (\num{1.192})\\
      \midrule
      Num.Obs. & \num{32} & \num{32}\\
      R2 & \num{0.602} & \num{0.741}\\
      R2 Adj. & \num{0.589} & \num{0.723}\\
      AIC & \num{181.2} & \num{169.5}\\
      BIC & \num{185.6} & \num{175.4}\\
      Log.Lik. & \num{-87.619} & \num{-80.752}\\
      F & \num{45.460} & \num{41.522}\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      modelsummary(mod, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lcc}
      \toprule
        & Model 1 & Model 2\\
      \midrule
      (Intercept) & $30.099$ & $10.790$\\
       & ($1.634$) & ($5.078$)\\
      hp & $-0.068$ & $-0.052$\\
       & ($0.010$) & ($0.009$)\\
      drat &  & $4.698$\\
       &  & ($1.192$)\\
      \midrule
      Num.Obs. & $32$ & $32$\\
      R2 & $0.602$ & $0.741$\\
      R2 Adj. & $0.589$ & $0.723$\\
      AIC & $181.2$ & $169.5$\\
      BIC & $185.6$ & $175.4$\\
      Log.Lik. & $-87.619$ & $-80.752$\\
      F & $45.460$ & $41.522$\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      modelsummary(mod, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lcc}
      \toprule
        & Model 1 & Model 2\\
      \midrule
      (Intercept) & 30.099 & 10.790\\
       & (1.634) & (5.078)\\
      hp & -0.068 & -0.052\\
       & (0.010) & (0.009)\\
      drat &  & 4.698\\
       &  & (1.192)\\
      \midrule
      Num.Obs. & 32 & 32\\
      R2 & 0.602 & 0.741\\
      R2 Adj. & 0.589 & 0.723\\
      AIC & 181.2 & 169.5\\
      BIC & 185.6 & 175.4\\
      Log.Lik. & -87.619 & -80.752\\
      F & 45.460 & 41.522\\
      \bottomrule
      \end{tabular}
      \end{table}

