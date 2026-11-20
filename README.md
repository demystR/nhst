# nhst

Demystifying Null Hypothesis Testing, part of the [demystR](https://demystr.github.io) series.

Draft. Chapters are outlines only so far.

- Part 1, Sampling: sampling distributions; standard error and t
- Part 2, Testing: null distributions; p-values; error rates; power
- Part 3, Power in Practice: a priori power; sensitivity analysis; post hoc power

## Render locally

```r
install.packages(c("bookdown", "tidyverse", "downlit", "xml2", "pwr"))
bookdown::render_book("index.Rmd", "bookdown::bs4_book")
```
