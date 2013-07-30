library(testit)

assert(
  'R 2.15.x recognizes comments, functions and strings in fallback mode',
  identical(hi_latex('1+1 # a comment'), '1+1 \\hlcom{# a comment}'),
  identical(hi_latex('paste("STRING", \'string\')'),
            '\\hlkwd{paste}(\\hlstr{"STRING"}, \\hlstr{\'string\'})')
)
