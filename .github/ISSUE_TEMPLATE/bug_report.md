---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

Before reporting a bug, please make sure the bug is not already fixed in the development version. To do so, install all the dependencies:

```r
library(remotes)
install_github("easystats/parameters")
install_github("easystats/performance")
install_github("easystats/insight")
install_github("vincentarelbundock/modelsummary")
```

Restart `R` completely. Then try again.

If your problem persists, please:

1. Give a *concise* description of the bug
2. A *minimal* working code example, loading only the bare minimum libraries needed to reproduce.
3. Using *publicly* available data like the `mtcars` dataset which comes bundled with `R`.

Finally, please include the output of this command:

```r
sessionInfo()
```
