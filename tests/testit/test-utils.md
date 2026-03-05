# escape_latex() sanitizes backslashes and {}

```{r}
escape_latex('\\')
escape_latex('\\{}')
escape_latex('{\\}')
escape_latex('~!@#$%^&*()')
```
```
\textbackslash{}
\textbackslash{}\{\}
\{\textbackslash{}\}
~!@#$%^&*()
```

# escape_html() escapes HTML chars

```{r}
escape_html('&"<>')
escape_html('~!@#$%^&*()')
```
```
&amp;&quot;&lt;&gt;
~!@#$%^&amp;*()
```
