# highr

[![Build Status](https://travis-ci.org/yihui/highr.png)](https://travis-ci.org/yihui/highr)

This is an infrastructure R package for syntax highlighting. It supports
LaTeX and HTML output. Not surprisingly, it works best with R code. It
attaches markups onto source code, e.g., it turns

```s
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

```s
library(highr)
hilight("a <- 1 # something")
hilight("a <- 1 # something", format = "html")
```

This package also has a wrapper function, `hi_andre()`, for Andre Simon's
[Highlight](http://www.andre-simon.de) package.

This package is supposed to serve other packages such as
[**knitr**](http://yihui.name/knitr).
