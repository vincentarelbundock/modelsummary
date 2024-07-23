---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''
---

Before reporting a bug, please update to the latest development version of `modelsummary` and make sure the bug has not been fixed yet. You can update `modelsummary` and all its dependencies with:

```r
modelsummary::update_modelsummary()
```

Then, restart your R session completely and try again.


Bug reports must include:

1. *Concise* description of the bug
2. *Minimal* reproducible example using publicly available data and the bare minimum code and libraries needed to reproduce the bug.
    - Consider using the `mtcars` dataset which is distributed by default with `R`, or one of the [CSV files from the RDatasets archive.](https://vincentarelbundock.github.io/Rdatasets/articles/data.html)
3. `sessionInfo()` output

Make sure you are running the *latest development version* of `modelsummary` and its dependencies: https://modelsummary.com/#installation
