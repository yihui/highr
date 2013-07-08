\name{hilight}
\alias{hi_html}
\alias{hi_latex}
\alias{hilight}
\title{Syntax highlight an R code fragment}
\usage{
hilight(code, format = c("latex", "html"), markup, prompt = FALSE)

hi_latex(code, ...)

hi_html(code, ...)
}
\arguments{
  \item{code}{a character string (the R source code)}

  \item{format}{the output format}

  \item{markup}{a data frame of two columns containing the
  markup commands}

  \item{prompt}{whether to add prompts to the code}

  \item{...}{arguments to be passed to \code{hilight()}}
}
\value{
  A character vector for the syntax highlighted code.
}
\description{
  This function \code{\link{parse}}s the R code, fetches
  the tokens in it (\code{\link{getParseData}}), and attach
  syntax highlighting commands onto them. With proper style
  definitions for these commands (such as colors or font
  styles), the R code will be syntax highlighted in the
  LaTeX/HTML output. The two functions \code{hi_latex} and
  \code{hi_html} are wrappers of \code{hilight} for LaTeX
  and HTML output, respectively.
}
\details{
  For the \code{markup} data frame, the first column is put
  before the R tokens, and the second column is behind; the
  row names of the data frame must be the R token names; a
  special row is named \code{STANDARD}, which contains the
  markup for the standard tokens (i.e. those that do not
  need to be highlighted); if missing, the built-in data
  frames \code{highr:::cmd_latex} and
  \code{highr:::cmd_html} will be used.

  This function only binds markups onto R tokens, and the
  real syntax highlighting must be done with style
  definitions, which is out of the scope of this package.
  It was designed to be used as the syntax highlighting
  infrastructure of other packages such as \pkg{knitr},
  where the colors and font styles are properly defined in
  the LaTeX preamble and HTML header.
}
\examples{
library(highr)
hilight("x=1 # assignment")

txt = c("a <- 1 # something", "c(y=\"world\", z=\"hello\")", "b=function(x=5) {", 
    "for(i in 1:10) {\n  if (i < x) print(i) else break}}", "z@child # S4 slot", 
    "'special chars <>#$\%&_{}'")
cat(hi_latex(txt), sep = "\\n")
cat(hi_html(txt), sep = "\\n")

# the markup data frames
highr:::cmd_latex
highr:::cmd_html
}
\author{
  Yihui Xie <\url{http://yihui.name}> and Yixuan Qiu
  <\url{http://yixuan.cos.name}>
}
\seealso{
  The \pkg{highlight} package is a more comphrehensive
  package, which ships syntax highlighting themes as well.
  The \pkg{knitr} package uses \pkg{highr} when it is
  available.

  See the package vignettes \code{browseVignettes('highr')}
  for how this function works internally.
}
