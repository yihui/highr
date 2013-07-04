# some special symbols (keywords a)
.keywords = c('FUNCTION', 'IF', 'ELSE', 'WHILE', 'FOR', 'IN', 'BREAK', 'REPEAT', 'NEXT', 'NULL_CONST')

.operators = c(
  sprintf("'%s'", c('+', '-', '*', '/', '^', '$', '@', ':', '?', '~', '!')),
  'SPECIAL', 'GT', 'GE', 'LT', 'LE', 'EQ', 'NE', 'AND', 'AND2', 'OR', 'OR2'
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
#' @author Yihui Xie <\url{http://yihui.name}> and Yixuan Qiu
#'   <\url{http://yixuan.cos.name}>
#' @seealso The \pkg{highlight} package is a more comphrehensive package, which
#'   ships syntax highlighting themes as well. The \pkg{knitr} package uses
#'   \pkg{highr} when it is available.
#'
#'   See the package vignette \code{browseVignettes('highr')} for how this
#'   function works internally.
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
#' @export
hilight = function(code, format = c('latex', 'html'), markup) {

  # the data frames do not need factors in this function
  op = options(stringsAsFactors = FALSE); on.exit(options(op))

  format = match.arg(format)
  p = parse(text = code, keep.source = TRUE)
  if (length(p) == 0L) return(code)
  z = getParseData(p)
  if (NROW(z) == 0L || !any(z$terminal)) return(x)
  z = z[z$terminal, ]

  if (missing(markup))
    markup = getFromNamespace(paste('cmd', format, sep = '_'), 'highr')
  res = cbind(z[, c('line1', 'col1', 'line2', 'col2', 'text')], merge_cmd(z, markup))

  # escape special LaTeX/HTML chars
  res$text = do.call(paste('escape', format, sep = '_'), list(res$text))

  # e.g. a string spans across multiple lines; now need to replace line1 with
  # line2 so that we know the starting and ending positions of spaces; e.g. turn
  # line/col numbers 1 5 2 6 into 2 5 2 6
  for (i in which(res$line1 != res$line2)) {
    res$line1[res$line1 == res$line1[i]] = res$line2[i]
  }

  unlist(lapply(split(res, res$line1), function(d) {
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
          sep = '', collapse = '')
  }), use.names = FALSE)
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
hi_andre = function(code, language, format = 'html') {
  if (!nzchar(Sys.which('highlight')))
    stop('please first install highlight from http://www.andre-simon.de')
  f = basename(tempfile('code', '.'))
  writeLines(code, f); on.exit(unlink(f))
  cmd = sprintf('highlight -f -S %s -O %s %s', language, format, f)
  system(cmd, intern = TRUE)
}
