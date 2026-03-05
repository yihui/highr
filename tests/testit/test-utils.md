# escape_latex() sanitizes backslashes and {}

```{r}
escape_latex('\\')
escape_latex('\\{}')
escape_latex('{\\}')
escape_latex('~!@#$%^&*()')
```
```
[1] "\\textbackslash{}"
[1] "\\textbackslash{}\\{\\}"
[1] "\\{\\textbackslash{}\\}"
[1] "~!@#$%^&*()"
```

# escape_html() escapes HTML chars

```{r}
escape_html('&"<>')
escape_html('~!@#$%^&*()')
```
```
[1] "&amp;&quot;&lt;&gt;"
[1] "~!@#$%^&amp;*()"
```
