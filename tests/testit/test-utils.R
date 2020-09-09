library(testit)

assert(
  'spaces(n) gives n spaces',
  spaces(-1) == '', spaces(0) == '', spaces(1) == ' ', spaces(5) == '     '
)

assert(
  'escape_latex() sanitizes backslashes and {}',
  escape_latex('\\') == '\\textbackslash{}',
  escape_latex('\\{}') == '\\textbackslash{}\\{\\}',
  escape_latex('{\\}') == '\\{\\textbackslash{}\\}',
  escape_latex('~!@#$%^&*()') == '~!@#$%^&*()'
)

assert(
  'escape_html() escapes HTML chars',
  escape_html('&"<>') == '&amp;&quot;&lt;&gt;',
  escape_html('~!@#$%^&*()') == '~!@#$%^&amp;*()'
)
