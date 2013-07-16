library(testit)

assert(
  'hi_latex() works without prompts',
  hi_latex('1+1') == '\\hlnum{1}\\hlopt{+}\\hlnum{1}',
  hi_latex('  1 +    1') == '  \\hlnum{1} \\hlopt{+}    \\hlnum{1}',
  identical(hi_latex(c('  if (TRUE ){', 'foo && bar}')), c(
    '  \\hlkwa{if} \\hlstd{(}\\hlnum{TRUE} \\hlstd{)\\{}',
    '\\hlstd{foo} \\hlopt{&&} \\hlstd{bar\\}}'
  ))
)

assert(
  'hi_latex() works with prompts',
  hi_latex('1+1', prompt=TRUE) == '\\hlstd{> }\\hlnum{1}\\hlopt{+}\\hlnum{1}',
  identical(hi_latex(c('  if (TRUE ){', 'foo && bar}'), prompt = TRUE), paste(
    '\\hlstd{> }  \\hlkwa{if} \\hlstd{(}\\hlnum{TRUE} \\hlstd{)\\{}',
    '\\hlstd{+ }\\hlstd{foo} \\hlopt{&&} \\hlstd{bar\\}}', sep = '\n'
  ))
)

assert(
  'the fallback method recognizes comments, functions and strings',
  identical(hi_latex('1+1 # a comment', fallback = TRUE), '1+1 \\hlcom{# a comment}'),
  identical(hi_latex('paste("STRING", \'string\')', fallback = TRUE),
            '\\hlkwd{paste}(\\hlstr{"STRING"}, \\hlstr{\'string\'})')
)

assert(
  'the fallback mode is used when the code does not parse',
  has_warning(res <- hi_latex('1+1+ # comment')),
  identical(res, '1+1+ \\hlcom{# comment}')
)

# define one's own markup data frame
my_cmd = cmd_html
my_cmd['NUM_CONST', 1] = '<span class="my num">'

assert(
  'custom markup also works',
  hi_html('1+ 1') ==
    '<span class="hl num">1</span><span class="hl opt">+</span> <span class="hl num">1</span>',
  hi_html('1+ 1', markup = my_cmd) ==
    '<span class="my num">1</span><span class="hl opt">+</span> <span class="my num">1</span>'
)

rm(my_cmd)
