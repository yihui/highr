# some special symbols (keywords a)
.keywords = c('FUNCTION', 'IF', 'ELSE', 'WHILE', 'FOR', 'IN', 'BREAK', 'REPEAT', 'NEXT', 'NULL_CONST')

.operators = c(
  sprintf("'%s'", c('+', '-', '*', '/', '^', '$', '@', ':', '?', '~', '!')),
  'SPECIAL', 'GT', 'GE', 'LT', 'LE', 'EQ', 'NE', 'AND', 'AND2', 'OR', 'OR2',
  'NS_GET', 'NS_GET_INT'
)

.cmd.list = sort(c(
  NUM_CONST            = 'num', # numbers
  SYMBOL_FUNCTION_CALL = 'kwd', # function calls
  STR_CONST            = 'str', # character strings
  COMMENT              = 'com', # comment
  SYMBOL_FORMALS       = 'kwc', # function(formals)
  SYMBOL_SUB           = 'kwc', # FUN(args)
  SLOT                 = 'kwc', # S4 slot
  LEFT_ASSIGN          = 'kwb', # assignment
  EQ_ASSIGN            = 'kwb',
  RIGHT_ASSIGN         = 'kwb',
  setNames(rep('opt', length(.operators)), .operators),
  setNames(rep('kwa', length(.keywords)), .keywords),
  STANDARD             = 'std' # everything else
))

cmd_latex = data.frame(
  cmd1 = paste('\\hl', .cmd.list, '{', sep = ''),
  cmd2 = '}',
  stringsAsFactors = FALSE,
  row.names = names(.cmd.list)
)

cmd_html = data.frame(
  cmd1 = paste('<span class="hl ', .cmd.list, '">', sep = ''),
  cmd2 = '</span>',
  stringsAsFactors = FALSE,
  row.names = names(.cmd.list)
)

# merge code and markups; use standard markups on unknown tokens
merge_cmd = function(pdata, cmd) {
  res = cmd[pdata$token, ]
  idx = is.na(res[, 1])
  res[idx, 1] = cmd['STANDARD', 1]
  res[idx, 2] = cmd['STANDARD', 2]
  res[is.na(res)] = '' # if STANDARD is undefined in the markup data frame
  res
}

#' Syntax highlight an R code fragment
#'
#' This function \code{\link{parse}}s the R code, fetches the tokens in it
#' (\code{\link{getParseData}}), and attach syntax highlighting commands onto
#' them. With proper style definitions for these commands (such as colors or
#' font styles), the R code will be syntax highlighted in the LaTeX/HTML output.
#' The two functions \code{hi_latex} and \code{hi_html} are wrappers of
#' \code{hilight} for LaTeX and HTML output, respectively.
#'
#' For the \code{markup} data frame, the first column is put before the R
#' tokens, and the second column is behind; the row names of the data frame must
#' be the R token names; a special row is named \code{STANDARD}, which contains
#' the markup for the standard tokens (i.e. those that do not need to be
#' highlighted); if missing, the built-in data frames \code{highr:::cmd_latex}
#' and \code{highr:::cmd_html} will be used.
#'
#' This function only binds markups onto R tokens, and the real syntax
#' highlighting must be done with style definitions, which is out of the scope
#' of this package. It was designed to be used as the syntax highlighting
#' infrastructure of other packages such as \pkg{knitr}, where the colors and
#' font styles are properly defined in the LaTeX preamble and HTML header.
#' @param code a character string (the R source code)
#' @param format the output format
#' @param markup a data frame of two columns containing the markup commands
#' @param prompt whether to add prompts to the code
#' @param fallback whether to use the fallback method, i.e. the regular
#'   expression based method; this method is not precise and only highlights a
#'   few types of symbols such as comments, strings and functions;
#'   \code{fallback} will be set to \code{TRUE} when the input \code{code} fails
#'   to be \code{\link{parse}d}
#' @param ... arguments to be passed to \code{hilight()}
#' @author Yihui Xie <\url{http://yihui.name}> and Yixuan Qiu
#'   <\url{http://yixuan.cos.name}>
#' @seealso See the package vignettes \code{browseVignettes('highr')} for how
#' this function works internally.
#' @return A character vector for the syntax highlighted code.
#' @examples library(highr)
#' hilight("x=1 # assignment")
#'
#' txt = c("a <- 1 # something", 'c(y="world", z="hello")', 'b=function(x=5) {',
#' 'for(i in 1:10) {
#'   if (i < x) print(i) else break}}',
#' "z@@child # S4 slot", "'special chars <>#$%&_{}'")
#' cat(hi_latex(txt), sep = '\n')
#' cat(hi_html(txt), sep = '\n')
#'
#' # the markup data frames
#' highr:::cmd_latex; highr:::cmd_html
#' @import utils
#' @export
hilight = function(code, format = c('latex', 'html'), markup, prompt = FALSE, fallback = FALSE) {
  if (length(code) == 0) return(code)
  format = match.arg(format)
  if (missing(markup) || is.null(markup))
    markup = if (format == 'latex') cmd_latex else cmd_html
  escape_fun = if (format == 'latex') escape_latex else escape_html
  if (!fallback && !try_parse(code, silent = FALSE)) {
    # the code is not valid, so you must use the fallback mode
    warning('the syntax of the source code is invalid; the fallback mode is used')
    fallback = TRUE
  }
  if (!prompt) return(
    (if (fallback) hi_naive else hilight_one)(code, format, markup, escape_fun)
  )
  p1 = escape_fun(getOption('prompt')); p2 = escape_fun(getOption('continue'))
  std = unlist(markup['STANDARD', ])
  if (!any(is.na(std))) {
    p1 = paste0(std[1], p1, std[2]); p2 = paste0(std[1], p2, std[2])
  }
  code = group_src(code)
  sapply(mapply(hilight_one, code, MoreArgs = list(format, markup, escape_fun),
                SIMPLIFY = FALSE, USE.NAMES = FALSE),
         function(x) paste0(rep(c(p1, p2), c(1L, length(x) - 1L)), x, collapse = '\n'))
}
# highlight one expression
hilight_one = function(code, format, markup, escape_fun) {

  # the data frames do not need factors in this function; need to keep source
  op = options(stringsAsFactors = FALSE, keep.source = TRUE); on.exit(options(op))

  p = parse_source(code)
  z = utils::getParseData(p)
  if (NROW(z) == 0L || !any(z$terminal)) return(code)
  z = z[z$terminal, ]

  # record how empty lines before/after the code
  one = paste(code, collapse = '\n')
  r1 = '^(\\s*)\n.*'; r2 = '^.*?\n(\\s*)$'
  s1 = if (grepl(r1, one)) gsub(r1, '\\1', one)
  s2 = if (grepl(r2, one)) gsub(r2, '\\1', one)

  res = cbind(z[, c('line1', 'col1', 'line2', 'col2', 'text')], merge_cmd(z, markup))

  # escape special LaTeX/HTML chars
  res$text = escape_fun(res$text)

  # record how many blank lines after each token
  blanks = c(pmax(res$line1[-1] - res$line2[-nrow(res)] - 1, 0), 0)
  # add line breaks to the 8th column
  res = cbind(res, mapply(spaces, blanks, '\n'))

  # e.g. a string spans across multiple lines; now need to replace line1 with
  # line2 so that we know the starting and ending positions of spaces; e.g. turn
  # line/col numbers 1 5 2 6 into 2 5 2 6
  for (i in which(res$line1 != res$line2)) {
    res$line1[res$line1 == res$line1[i]] = res$line2[i]
  }

  out = lapply(split(res, res$line1), function(d) {
    # merge adjacent tokens of the same type so that the output is cleaner
    empty = matrix(FALSE, nrow = nrow(d), ncol = 2)
    for (i in seq_len(nrow(d) - 1)) {
      if (all(d[i, 6:7] == d[i + 1, 6:7])) empty[i + 1, 1] = empty[i, 2] = TRUE
    }
    d[, 6:7][empty] = ''
    col = as.matrix(d[, c('col1', 'col2')])
    # add 0 and remove col[n, 2] to get start/end positions of spaces
    col = matrix(head(c(0, t(col)), -1), ncol = 2, byrow = TRUE)
    paste(mapply(spaces, col[, 2] - col[, 1] - 1), d[, 6], d[, 'text'], d[, 7],
          d[, 8], sep = '', collapse = '')
  })
  c(s1, unlist(out, use.names = FALSE), s2)
}
#' @export
#' @rdname hilight
hi_latex = function(code, ...) hilight(code, 'latex', ...)
#' @export
#' @rdname hilight
hi_html = function(code, ...) hilight(code, 'html', ...)

#' A wrapper to Andre Simon's Highlight
#'
#' This function calls Highlight to syntax highlight a code fragment.
#' @param code a character string of the source code
#' @param language the input language (c, cpp, python, r, ...); see
#'   \code{system('highlight -p')}
#' @param format the output format (html, latex, ...)
#' @references Andre Simon's Highlight package \url{http://www.andre-simon.de}.
#' @return A character string for the syntax highlighted code.
#' @export
#' @examples \dontrun{hi_andre('1+1', language='R')
#' hi_andre('void main() {\nreturn(0)\n}', language='c', format='latex')}
hi_andre = function(code, language, format = 'html') {
  h = Sys.which('highlight')
  os = Sys.info()[['sysname']]
  # highlight on Linux Mint can be something else
  # on OS10 with highlight installed using Homebrew it's often in /usr/local/bin
  if (!nzchar(h) || (h == '/usr/local/bin/highlight' && os != 'Darwin' &&
                       !file.exists(h <- '/usr/bin/highlight')))
    stop('please first install highlight from http://www.andre-simon.de')
  f = basename(tempfile('code', '.'))
  writeLines(code, f); on.exit(unlink(f))
  cmd = sprintf('%s -f -S %s -O %s %s', shQuote(h), correct_lang(language), format, f)
  system(cmd, intern = TRUE)
}

# to help knitr engines decide the highlight language
correct_lang = function(x) {
  switch(x, Rcpp = 'cpp', tikz = 'latex', Rscript = 'R', fortran = 'f', stan = 'R', x)
}
