# highr

<!-- badges: start -->
[![R-CMD-check](https://github.com/yihui/highr/workflows/R-CMD-check/badge.svg)](https://github.com/yihui/highr/actions)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/highr)](https://cran.r-project.org/package=highr)
<!-- badges: end -->


This is an infrastructure R package for syntax highlighting. It supports
LaTeX and HTML output. Not surprisingly, it works best with R code. It
attaches markups onto source code, e.g., it turns

```r
a <- 1 # something
```

into LaTeX code

```latex
\hlstd{a} \hlkwb{<-} \hlnum{1} \hlcom{\# something}
```

or HTML code

```html
<span class="hl std">a</span> <span class="hl kwb">&lt;-</span> <span class="hl num">1</span> <span class="hl com"># something</span>
```

via

```r
library(highr)
hi_latex("a <- 1 # something")
hi_html("a <- 1 # something")
# or hilight(code, format = "latex"/"html")
```

This package also has a wrapper function, `hi_andre()`, for Andre Simon's
[Highlight](http://www.andre-simon.de) package.

There are a few package Markdown vignettes in this package:

```r
browseVignettes(package = "highr")
```

To install the development version here, use

```r
install.packages('highr', repos = 'http://rforge.net', type = 'source')
```

This package is licensed under GPL, and is supposed to serve other packages
such as [**knitr**](http://yihui.name/knitr).
