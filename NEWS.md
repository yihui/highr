# CHANGES IN highr VERSION 0.10

- The minimal R version required is 3.3.0 now.

# CHANGES IN highr VERSION 0.9

- Added `markdown` to `Suggests` of the package `DESCRIPTION` for https://github.com/yihui/knitr/issues/1864.

# CHANGES IN highr VERSION 0.8

- The minimal required R version is 3.2.3 now.

# CHANGES IN highr VERSION 0.7

- Added an internal data frame highr:::cmd_pandoc_latex (mainly for https://stackoverflow.com/q/40252885/559676).

# CHANGES IN highr VERSION 0.6

- `hilight()` can deal with multibyte characters that cannot represented in the system native encoding now (on Windows)

- blank lines before/after code are preserved in the hilight() output now (thanks, Terry Therneau)

# CHANGES IN highr VERSION 0.5

- the minimal required R version is 3.0.2 now

# CHANGES IN highr VERSION 0.4

- hi_andre() may fail to find highlight under OS X (thanks, Christopher Gandrud, #2)

- :: and ::: are recognized as operators, and they will be syntax highlighted in the same way as other operators like +, -, *, and /, etc (thanks, Qiang Li, #3)

# CHANGES IN highr VERSION 0.3

- blank lines were not preserved in previous versions; now they can be correctly preserved

- hilight() works when the code is only a comment; fixed the bug reported at http://stackoverflow.com/q/18548020/559676 which was actually due to the bug of utils::getParseData() in R 3.0.1

- Linux Mint has a built-in version of highlight under /usr/local/bin/ which is not Andre Simon's highlight, and this can hang highr (https://groups.google.com/forum/#!topic/knitr/nicYgzqhwX8)

- the package vignettes were built with the knitr::docco_classic engine; see vignette(package = 'highr')

# CHANGES IN highr VERSION 0.2.1

- fixed a test that failed under R 2.15.x due to the keep.source argument in parse(), which was not available in R until R 3.0.0

# CHANGES IN highr VERSION 0.2

- added a new argument 'fallback' in hilight(); for R < 3.0.0, we can use hilight(..., fallback = TRUE), which is a rough syntax highlighting method based on regular expressions (hence less accurate and comphrehensive than the getParseData() approach); as a result, highr no longer has to depend on R 3.0.0, although it is recommended to use R 3.0.0

# CHANGES IN highr VERSION 0.1

- the first version of highr: a hilight() function based on utils::getParseData() to do syntax highlighting for R code; hi_andrew() as a wrapper for Andre Simon's Highlight package

