hi.keywords =  paste('(\\W)(', paste(c(
  'if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next', 'break', 'repeat',
  'LETTERS', 'letters', 'month.abb', 'month.name', 'pi',
  'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_'
), collapse = '|'), ')(\\W)', sep = '')

#  only highlight function names, strings and comments
hi_naive_latex = function(x, str, com, kwa, kwd) {
  i = grepl('^\\s*#', x)  # whole lines of comments
  x[i] = sprintf(paste(com[1], '%s', com[2], sep = ''), x[i])
  str = escape_bs(str); com = escape_bs(com); kwd = escape_bs(kwd)
  # comments: what if # inside quotes?
  if (any(idx <- grepl('#', x) & !grepl('"', x) & !i))
    x[idx] = gsub('(#.*)', sprintf('%s\\1%s', com[1], com[2]), x[idx])
  i = which(!i)  # not comments
  # function names
  x[i] = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', sprintf('%s\\1%s\\2(', kwd[1], kwd[2]), x[i])
  # character strings
  x[i] = gsub('"([^"]*)"', sprintf('%s"\\1"%s', str[1], str[2]), x[i])
  x[i] = gsub("'([^']*)'", sprintf("%s'\\1'%s", str[1], str[2]), x[i])
  x
}
hi_naive_html = function(x, str, com, kwa, kwd) {
  # character strings
  x = gsub('&quot;(.*?)&quot;', sprintf('%s&quot;\\1&quot;%s', str[1], str[2]), x)
  x = gsub("'([^']*)'", sprintf("%s'\\1'%s", str[1], str[2]), x)
  # function names
  x = gsub('([[:alnum:]_\\.]+)(\\s*)\\(', sprintf('%s\\1%s\\2(', kwd[1], kwd[2]), x)
  if (any(idx <- grepl('#', x) & !grepl('"', x)))
    x[idx] = gsub('(#.*)', sprintf('%s\\1%s', com[1], com[2]), x[idx])
  gsub(hi.keywords, sprintf('\\1%s\\2%s\\3', kwa[1], kwa[2]), x)
}

hi_naive = function(code, format = c('latex', 'html'), markup, escape_fun = identity,
                    prompt = NULL) {
  format = match.arg(format)
  code = escape_fun(code)
  if (length(prompt) == 2) {
    # borrowed from knitr:::line_prompt
    code = paste(
      prompt[1],
      gsub('(?<=\n)(?=.|\n)', prompt[2], code, perl = TRUE), sep = ''
    )
  }
  hi_fun = if (format == 'latex') hi_naive_latex else hi_naive_html
  hi_fun(split_lines(code), str = markup['STR_CONST', ], com = markup['COMMENT', ],
         kwa = markup['IF', ], kwd = markup['SYMBOL_FUNCTION_CALL', ])
}

# split a character vector by \n
split_lines = function(x) {
  if (!any(grepl('\n', x))) return(x)
  x[x == ''] = '\n'
  unlist(strsplit(x, '\n'))
}

# escape backslashes
escape_bs = function(x) gsub('\\\\', '\\\\\\\\', x)
