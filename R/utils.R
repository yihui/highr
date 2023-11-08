# parse source and keep source
parse_source = function(lines) parse(text = lines, keep.source = TRUE)

# borrowed from knitr

# escape backslashes and {} for the alltt package
escape_latex = function(x) {
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
