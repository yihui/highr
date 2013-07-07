# generate spaces of width n
spaces = function(n = 1) {
  if (n <= 0) return('')
  if (n == 1) return(' ')
  paste(rep(' ', n), collapse = '')
}

# borrowed from knitr

# escape backslashes and {} for the alltt package
escape_latex = function(x, newlines = FALSE, spaces = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([{}])', '\\\\\\1', x)
  gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
}

# escape special HTML chars
escape_html = function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}
