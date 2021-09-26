# escaped tables

    Code
      modelsummary(mod, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lcc}
      \toprule
        & First\&Second & Third\_Fourth\\
      \midrule
      (Intercept) & \num{-103.772} & \num{-103.772}\\
       & (\num{103.551}) & (\num{103.551})\\
      under\_score & \num{-3.899} & \num{-3.899}\\
       & (\num{2.072}) & (\num{2.072})\\
      `oh\&yeah<sup>2</sup>` & \num{29.329} & \num{29.329}\\
       & (\num{7.169}) & (\num{7.169})\\
      drat & \num{40.961} & \num{40.961}\\
       & (\num{17.115}) & (\num{17.115})\\
      \midrule
      Num.Obs. & \num{32} & \num{32}\\
      R2 & \num{0.759} & \num{0.759}\\
      R2 Adj. & \num{0.733} & \num{0.733}\\
      AIC & \num{324.9} & \num{324.9}\\
      BIC & \num{332.2} & \num{332.2}\\
      Log.Lik. & \num{-157.443} & \num{-157.443}\\
      F & \num{29.335} & \num{29.335}\\
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
        & First\&Second & Third\_Fourth\\
      \midrule
      (Intercept) & \num{-103.772} & \num{-103.772}\\
       & (\num{103.551}) & (\num{103.551})\\
      under\_score & \num{-3.899} & \num{-3.899}\\
       & (\num{2.072}) & (\num{2.072})\\
      `oh\&yeah<sup>2</sup>` & \num{29.329} & \num{29.329}\\
       & (\num{7.169}) & (\num{7.169})\\
      drat & \num{40.961} & \num{40.961}\\
       & (\num{17.115}) & (\num{17.115})\\
      \midrule
      Num.Obs. & \num{32} & \num{32}\\
      R2 & \num{0.759} & \num{0.759}\\
      R2 Adj. & \num{0.733} & \num{0.733}\\
      AIC & \num{324.9} & \num{324.9}\\
      BIC & \num{332.2} & \num{332.2}\\
      Log.Lik. & \num{-157.443} & \num{-157.443}\\
      F & \num{29.335} & \num{29.335}\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      modelsummary(mod, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> First&amp;Second </th>
         <th style="text-align:center;"> Third_Fourth </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> −103.772 </td>
         <td style="text-align:center;"> −103.772 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (103.551) </td>
         <td style="text-align:center;"> (103.551) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> under_score </td>
         <td style="text-align:center;"> −3.899 </td>
         <td style="text-align:center;"> −3.899 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (2.072) </td>
         <td style="text-align:center;"> (2.072) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> `oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt;` </td>
         <td style="text-align:center;"> 29.329 </td>
         <td style="text-align:center;"> 29.329 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (7.169) </td>
         <td style="text-align:center;"> (7.169) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;"> 40.961 </td>
         <td style="text-align:center;"> 40.961 </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (17.115) </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (17.115) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> 32 </td>
         <td style="text-align:center;"> 32 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> 0.759 </td>
         <td style="text-align:center;"> 0.759 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> 0.733 </td>
         <td style="text-align:center;"> 0.733 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> 324.9 </td>
         <td style="text-align:center;"> 324.9 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> 332.2 </td>
         <td style="text-align:center;"> 332.2 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> −157.443 </td>
         <td style="text-align:center;"> −157.443 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> 29.335 </td>
         <td style="text-align:center;"> 29.335 </td>
        </tr>
      </tbody>
      </table>

---

    Code
      modelsummary(mod, output = "html", escape = FALSE)
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> First&amp;Second </th>
         <th style="text-align:center;"> Third_Fourth </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> −103.772 </td>
         <td style="text-align:center;"> −103.772 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (103.551) </td>
         <td style="text-align:center;"> (103.551) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> under_score </td>
         <td style="text-align:center;"> −3.899 </td>
         <td style="text-align:center;"> −3.899 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (2.072) </td>
         <td style="text-align:center;"> (2.072) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> `oh&amp;yeah<sup>2</sup>` </td>
         <td style="text-align:center;"> 29.329 </td>
         <td style="text-align:center;"> 29.329 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (7.169) </td>
         <td style="text-align:center;"> (7.169) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;"> 40.961 </td>
         <td style="text-align:center;"> 40.961 </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (17.115) </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (17.115) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> 32 </td>
         <td style="text-align:center;"> 32 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> 0.759 </td>
         <td style="text-align:center;"> 0.759 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> 0.733 </td>
         <td style="text-align:center;"> 0.733 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> 324.9 </td>
         <td style="text-align:center;"> 324.9 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> 332.2 </td>
         <td style="text-align:center;"> 332.2 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> −157.443 </td>
         <td style="text-align:center;"> −157.443 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> 29.335 </td>
         <td style="text-align:center;"> 29.335 </td>
        </tr>
      </tbody>
      </table>

# datasummary escape colnames and stub

    Code
      datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
      data = dat, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrr}
      \toprule
        & \% & Money \$\$\\
      \midrule
      under\_score & \num{20.09} & \num{6.03}\\
      hp & \num{146.69} & \num{68.56}\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      datasummary(under_score + hp ~ Heading("%") * mean + Heading("Money $$") * sd,
      data = dat, output = "latex", escape = FALSE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrr}
      \toprule
        & % & Money $$\\
      \midrule
      under_score & \num{20.09} & \num{6.03}\\
      hp & \num{146.69} & \num{68.56}\\
      \bottomrule
      \end{tabular}
      \end{table}

# datasummary_crosstab escape colnames and stub

    Code
      datasummary_crosstab(under_score1 * under_score2 ~ under_score3, data = tmp,
      output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lllrrr}
      \toprule
      under\_score1 & under\_score2 &   & 0 & 1 & All\\
      \midrule
      0 & 3 & N & 12 & 0 & 12\\
       &  & \% row & \num{100.0} & \num{0.0} & \num{100.0}\\
       & 4 & N & 0 & 2 & 2\\
       &  & \% row & \num{0.0} & \num{100.0} & \num{100.0}\\
       & 5 & N & 0 & 4 & 4\\
       &  & \% row & \num{0.0} & \num{100.0} & \num{100.0}\\
      1 & 3 & N & 3 & 0 & 3\\
       &  & \% row & \num{100.0} & \num{0.0} & \num{100.0}\\
       & 4 & N & 4 & 6 & 10\\
       &  & \% row & \num{40.0} & \num{60.0} & \num{100.0}\\
       & 5 & N & 0 & 1 & 1\\
       &  & \% row & \num{0.0} & \num{100.0} & \num{100.0}\\
       & All & N & 19 & 13 & 32\\
       &  & \% row & \num{59.4} & \num{40.6} & \num{100.0}\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      datasummary_crosstab(under_score1 * under_score2 ~ under_score3, data = tmp,
      output = "latex", escape = FALSE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lllrrr}
      \toprule
      under_score1 & under_score2 &   & 0 & 1 & All\\
      \midrule
      0 & 3 & N & 12 & 0 & 12\\
       &  & % row & \num{100.0} & \num{0.0} & \num{100.0}\\
       & 4 & N & 0 & 2 & 2\\
       &  & % row & \num{0.0} & \num{100.0} & \num{100.0}\\
       & 5 & N & 0 & 4 & 4\\
       &  & % row & \num{0.0} & \num{100.0} & \num{100.0}\\
      1 & 3 & N & 3 & 0 & 3\\
       &  & % row & \num{100.0} & \num{0.0} & \num{100.0}\\
       & 4 & N & 4 & 6 & 10\\
       &  & % row & \num{40.0} & \num{60.0} & \num{100.0}\\
       & 5 & N & 0 & 1 & 1\\
       &  & % row & \num{0.0} & \num{100.0} & \num{100.0}\\
       & All & N & 19 & 13 & 32\\
       &  & % row & \num{59.4} & \num{40.6} & \num{100.0}\\
      \bottomrule
      \end{tabular}
      \end{table}

# datasummary_correlation escape rownames and colnames

    Code
      datasummary_correlation(dat, output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrrrrrrrrrrr}
      \toprule
        & under\_score & oh\&yeah<sup>2</sup> & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
      \midrule
      under\_score & 1 & . & . & . & . & . & . & . & . & . & .\\
      oh\&yeah<sup>2</sup> & \num{-.85} & 1 & . & . & . & . & . & . & . & . & .\\
      disp & \num{-.85} & \num{.90} & 1 & . & . & . & . & . & . & . & .\\
      hp & \num{-.78} & \num{.83} & \num{.79} & 1 & . & . & . & . & . & . & .\\
      drat & \num{.68} & \num{-.70} & \num{-.71} & \num{-.45} & 1 & . & . & . & . & . & .\\
      wt & \num{-.87} & \num{.78} & \num{.89} & \num{.66} & \num{-.71} & 1 & . & . & . & . & .\\
      qsec & \num{.42} & \num{-.59} & \num{-.43} & \num{-.71} & \num{.09} & \num{-.17} & 1 & . & . & . & .\\
      vs & \num{.66} & \num{-.81} & \num{-.71} & \num{-.72} & \num{.44} & \num{-.55} & \num{.74} & 1 & . & . & .\\
      am & \num{.60} & \num{-.52} & \num{-.59} & \num{-.24} & \num{.71} & \num{-.69} & \num{-.23} & \num{.17} & 1 & . & .\\
      gear & \num{.48} & \num{-.49} & \num{-.56} & \num{-.13} & \num{.70} & \num{-.58} & \num{-.21} & \num{.21} & \num{.79} & 1 & .\\
      carb & \num{-.55} & \num{.53} & \num{.39} & \num{.75} & \num{-.09} & \num{.43} & \num{-.66} & \num{-.57} & \num{.06} & \num{.27} & 1\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      datasummary_correlation(dat, output = "latex", escape = FALSE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lrrrrrrrrrrr}
      \toprule
        & under_score & oh&yeah<sup>2</sup> & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
      \midrule
      under_score & 1 & . & . & . & . & . & . & . & . & . & .\\
      oh&yeah<sup>2</sup> & \num{-.85} & 1 & . & . & . & . & . & . & . & . & .\\
      disp & \num{-.85} & \num{.90} & 1 & . & . & . & . & . & . & . & .\\
      hp & \num{-.78} & \num{.83} & \num{.79} & 1 & . & . & . & . & . & . & .\\
      drat & \num{.68} & \num{-.70} & \num{-.71} & \num{-.45} & 1 & . & . & . & . & . & .\\
      wt & \num{-.87} & \num{.78} & \num{.89} & \num{.66} & \num{-.71} & 1 & . & . & . & . & .\\
      qsec & \num{.42} & \num{-.59} & \num{-.43} & \num{-.71} & \num{.09} & \num{-.17} & 1 & . & . & . & .\\
      vs & \num{.66} & \num{-.81} & \num{-.71} & \num{-.72} & \num{.44} & \num{-.55} & \num{.74} & 1 & . & . & .\\
      am & \num{.60} & \num{-.52} & \num{-.59} & \num{-.24} & \num{.71} & \num{-.69} & \num{-.23} & \num{.17} & 1 & . & .\\
      gear & \num{.48} & \num{-.49} & \num{-.56} & \num{-.13} & \num{.70} & \num{-.58} & \num{-.21} & \num{.21} & \num{.79} & 1 & .\\
      carb & \num{-.55} & \num{.53} & \num{.39} & \num{.75} & \num{-.09} & \num{.43} & \num{-.66} & \num{-.57} & \num{.06} & \num{.27} & 1\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      datasummary_correlation(dat, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:right;"> under_score </th>
         <th style="text-align:right;"> oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt; </th>
         <th style="text-align:right;"> disp </th>
         <th style="text-align:right;"> hp </th>
         <th style="text-align:right;"> drat </th>
         <th style="text-align:right;"> wt </th>
         <th style="text-align:right;"> qsec </th>
         <th style="text-align:right;"> vs </th>
         <th style="text-align:right;"> am </th>
         <th style="text-align:right;"> gear </th>
         <th style="text-align:right;"> carb </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> under_score </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> oh&amp;yeah&lt;sup&gt;2&lt;/sup&gt; </td>
         <td style="text-align:right;"> −.85 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> disp </td>
         <td style="text-align:right;"> −.85 </td>
         <td style="text-align:right;"> .90 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:right;"> −.78 </td>
         <td style="text-align:right;"> .83 </td>
         <td style="text-align:right;"> .79 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:right;"> .68 </td>
         <td style="text-align:right;"> −.70 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> −.45 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> wt </td>
         <td style="text-align:right;"> −.87 </td>
         <td style="text-align:right;"> .78 </td>
         <td style="text-align:right;"> .89 </td>
         <td style="text-align:right;"> .66 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> qsec </td>
         <td style="text-align:right;"> .42 </td>
         <td style="text-align:right;"> −.59 </td>
         <td style="text-align:right;"> −.43 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> .09 </td>
         <td style="text-align:right;"> −.17 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> vs </td>
         <td style="text-align:right;"> .66 </td>
         <td style="text-align:right;"> −.81 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> −.72 </td>
         <td style="text-align:right;"> .44 </td>
         <td style="text-align:right;"> −.55 </td>
         <td style="text-align:right;"> .74 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> am </td>
         <td style="text-align:right;"> .60 </td>
         <td style="text-align:right;"> −.52 </td>
         <td style="text-align:right;"> −.59 </td>
         <td style="text-align:right;"> −.24 </td>
         <td style="text-align:right;"> .71 </td>
         <td style="text-align:right;"> −.69 </td>
         <td style="text-align:right;"> −.23 </td>
         <td style="text-align:right;"> .17 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> gear </td>
         <td style="text-align:right;"> .48 </td>
         <td style="text-align:right;"> −.49 </td>
         <td style="text-align:right;"> −.56 </td>
         <td style="text-align:right;"> −.13 </td>
         <td style="text-align:right;"> .70 </td>
         <td style="text-align:right;"> −.58 </td>
         <td style="text-align:right;"> −.21 </td>
         <td style="text-align:right;"> .21 </td>
         <td style="text-align:right;"> .79 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> carb </td>
         <td style="text-align:right;"> −.55 </td>
         <td style="text-align:right;"> .53 </td>
         <td style="text-align:right;"> .39 </td>
         <td style="text-align:right;"> .75 </td>
         <td style="text-align:right;"> −.09 </td>
         <td style="text-align:right;"> .43 </td>
         <td style="text-align:right;"> −.66 </td>
         <td style="text-align:right;"> −.57 </td>
         <td style="text-align:right;"> .06 </td>
         <td style="text-align:right;"> .27 </td>
         <td style="text-align:right;"> 1 </td>
        </tr>
      </tbody>
      </table>

---

    Code
      datasummary_correlation(dat, output = "html", escape = FALSE)
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:right;"> under_score </th>
         <th style="text-align:right;"> oh&amp;yeah<sup>2</sup> </th>
         <th style="text-align:right;"> disp </th>
         <th style="text-align:right;"> hp </th>
         <th style="text-align:right;"> drat </th>
         <th style="text-align:right;"> wt </th>
         <th style="text-align:right;"> qsec </th>
         <th style="text-align:right;"> vs </th>
         <th style="text-align:right;"> am </th>
         <th style="text-align:right;"> gear </th>
         <th style="text-align:right;"> carb </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> under_score </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> oh&amp;yeah<sup>2</sup> </td>
         <td style="text-align:right;"> −.85 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> disp </td>
         <td style="text-align:right;"> −.85 </td>
         <td style="text-align:right;"> .90 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:right;"> −.78 </td>
         <td style="text-align:right;"> .83 </td>
         <td style="text-align:right;"> .79 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:right;"> .68 </td>
         <td style="text-align:right;"> −.70 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> −.45 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> wt </td>
         <td style="text-align:right;"> −.87 </td>
         <td style="text-align:right;"> .78 </td>
         <td style="text-align:right;"> .89 </td>
         <td style="text-align:right;"> .66 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> qsec </td>
         <td style="text-align:right;"> .42 </td>
         <td style="text-align:right;"> −.59 </td>
         <td style="text-align:right;"> −.43 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> .09 </td>
         <td style="text-align:right;"> −.17 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> vs </td>
         <td style="text-align:right;"> .66 </td>
         <td style="text-align:right;"> −.81 </td>
         <td style="text-align:right;"> −.71 </td>
         <td style="text-align:right;"> −.72 </td>
         <td style="text-align:right;"> .44 </td>
         <td style="text-align:right;"> −.55 </td>
         <td style="text-align:right;"> .74 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> am </td>
         <td style="text-align:right;"> .60 </td>
         <td style="text-align:right;"> −.52 </td>
         <td style="text-align:right;"> −.59 </td>
         <td style="text-align:right;"> −.24 </td>
         <td style="text-align:right;"> .71 </td>
         <td style="text-align:right;"> −.69 </td>
         <td style="text-align:right;"> −.23 </td>
         <td style="text-align:right;"> .17 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> gear </td>
         <td style="text-align:right;"> .48 </td>
         <td style="text-align:right;"> −.49 </td>
         <td style="text-align:right;"> −.56 </td>
         <td style="text-align:right;"> −.13 </td>
         <td style="text-align:right;"> .70 </td>
         <td style="text-align:right;"> −.58 </td>
         <td style="text-align:right;"> −.21 </td>
         <td style="text-align:right;"> .21 </td>
         <td style="text-align:right;"> .79 </td>
         <td style="text-align:right;"> 1 </td>
         <td style="text-align:right;"> . </td>
        </tr>
        <tr>
         <td style="text-align:left;"> carb </td>
         <td style="text-align:right;"> −.55 </td>
         <td style="text-align:right;"> .53 </td>
         <td style="text-align:right;"> .39 </td>
         <td style="text-align:right;"> .75 </td>
         <td style="text-align:right;"> −.09 </td>
         <td style="text-align:right;"> .43 </td>
         <td style="text-align:right;"> −.66 </td>
         <td style="text-align:right;"> −.57 </td>
         <td style="text-align:right;"> .06 </td>
         <td style="text-align:right;"> .27 </td>
         <td style="text-align:right;"> 1 </td>
        </tr>
      </tbody>
      </table>

# Bugfix: escape & latex & coef_map

    Code
      msummary(reg_prob1, coef_map = c("disp_x", "wt"), output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lccc}
      \toprule
        & Model 1 & Model 2 & Model 3\\
      \midrule
      disp\_x & \num{-0.041} & \num{-0.018} & \num{-0.018}\\
       & (\num{0.005}) & (\num{0.009}) & (\num{0.009})\\
      wt &  & \num{-3.351} & \num{-3.279}\\
       &  & (\num{1.164}) & (\num{1.328})\\
      \midrule
      Num.Obs. & \num{32} & \num{32} & \num{32}\\
      R2 & \num{0.718} & \num{0.781} & \num{0.781}\\
      R2 Adj. & \num{0.709} & \num{0.766} & \num{0.758}\\
      AIC & \num{170.2} & \num{164.2} & \num{166.2}\\
      BIC & \num{174.6} & \num{170.0} & \num{173.5}\\
      Log.Lik. & \num{-82.105} & \num{-78.084} & \num{-78.076}\\
      F & \num{76.513} & \num{51.689} & \num{33.293}\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      msummary(reg_prob2, coef_map = c("disp_x", "wt_x"), output = "latex")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{lccc}
      \toprule
        & Model 1 & Model 2 & Model 3\\
      \midrule
      disp\_x & \num{-0.041} & \num{-0.018} & \num{-0.018}\\
       & (\num{0.005}) & (\num{0.009}) & (\num{0.009})\\
      wt\_x &  & \num{-3.351} & \num{-3.279}\\
       &  & (\num{1.164}) & (\num{1.328})\\
      \midrule
      Num.Obs. & \num{32} & \num{32} & \num{32}\\
      R2 & \num{0.718} & \num{0.781} & \num{0.781}\\
      R2 Adj. & \num{0.709} & \num{0.766} & \num{0.758}\\
      AIC & \num{170.2} & \num{164.2} & \num{166.2}\\
      BIC & \num{174.6} & \num{170.0} & \num{173.5}\\
      Log.Lik. & \num{-82.105} & \num{-78.084} & \num{-78.076}\\
      F & \num{76.513} & \num{51.689} & \num{33.293}\\
      \bottomrule
      \end{tabular}
      \end{table}

# bugs stay dead: escape=FALSE w/ coef_map

    Code
      modelsummary(models, coef_map = cm, output = "latex_tabular", escape = FALSE)
    Output
      
      \begin{tabular}[t]{lc}
      \toprule
        & OLS 1\\
      \midrule
      Literacy (\%) & \num{22.472}\\
       & (\num{43.212})\\
      Patents per capita & \num{78.055}\\
       & (\num{29.979})\\
      Constant & \num{2852.466}\\
       & (\num{2724.293})\\
      \midrule
      Num.Obs. & \num{86}\\
      R2 & \num{0.091}\\
      R2 Adj. & \num{0.069}\\
      AIC & \num{1734.4}\\
      BIC & \num{1744.2}\\
      Log.Lik. & \num{-863.198}\\
      F & \num{4.133}\\
      \bottomrule
      \end{tabular}

# column headers are not escaped with `escape=FALSE`

    Code
      modelsummary(mod, vcov = c("classical", "HC1"), escape = FALSE, output = "html")
    Output
      <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
       <thead>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:center;"> <code>lm()</code> </th>
         <th style="text-align:center;"> <code>lm_robust()</code> </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> (Intercept) </td>
         <td style="text-align:center;"> 10.790 </td>
         <td style="text-align:center;"> 10.790 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (5.078) </td>
         <td style="text-align:center;"> (3.626) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> hp </td>
         <td style="text-align:center;"> −0.052 </td>
         <td style="text-align:center;"> −0.052 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:center;"> (0.009) </td>
         <td style="text-align:center;"> (0.010) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> drat </td>
         <td style="text-align:center;"> 4.698 </td>
         <td style="text-align:center;"> 4.698 </td>
        </tr>
        <tr>
         <td style="text-align:left;box-shadow: 0px 1px">  </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (1.192) </td>
         <td style="text-align:center;box-shadow: 0px 1px"> (0.850) </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Num.Obs. </td>
         <td style="text-align:center;"> 32 </td>
         <td style="text-align:center;"> 32 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 </td>
         <td style="text-align:center;"> 0.741 </td>
         <td style="text-align:center;"> 0.741 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> R2 Adj. </td>
         <td style="text-align:center;"> 0.723 </td>
         <td style="text-align:center;"> 0.723 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> AIC </td>
         <td style="text-align:center;"> 169.5 </td>
         <td style="text-align:center;"> 169.5 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> BIC </td>
         <td style="text-align:center;"> 175.4 </td>
         <td style="text-align:center;"> 175.4 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Log.Lik. </td>
         <td style="text-align:center;"> −80.752 </td>
         <td style="text-align:center;"> −80.752 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> F </td>
         <td style="text-align:center;"> 41.522 </td>
         <td style="text-align:center;"> 41.522 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Std.Errors </td>
         <td style="text-align:center;"> Classical </td>
         <td style="text-align:center;"> HC1 </td>
        </tr>
      </tbody>
      </table>

