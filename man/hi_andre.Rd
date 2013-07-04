\name{hi_andre}
\alias{hi_andre}
\title{A wrapper to Andre Simon's Highlight}
\usage{
hi_andre(code, language, format = "html")
}
\arguments{
  \item{code}{a character string of the source code}

  \item{language}{the input language (c, cpp, python, r,
  ...); see \code{system('highlight -p')}}

  \item{format}{the output format (html, latex, ...)}
}
\value{
  A character string for the syntax highlighted code.
}
\description{
  This function calls Highlight to syntax highlight a code
  fragment.
}
\references{
  Andre Simon's Highlight package
  \url{http://www.andre-simon.de}.
}
