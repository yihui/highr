library(testit)

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
