# hi_latex() works without prompts

```{r}
hi_latex('1+1')
hi_latex('  1 +    1')
hi_latex(c('  if (TRUE ){', 'foo && bar}'))
```
```
[1] "\\hlnum{1}\\hlopt{+}\\hlnum{1}"
[1] "  \\hlnum{1} \\hlopt{+}    \\hlnum{1}"
[1] "  \\hlkwa{if} \\hldef{(}\\hlnum{TRUE} \\hldef{)\\{}"
[2] "\\hldef{foo} \\hlopt{&&} \\hldef{bar\\}}"           
```

# hi_latex() works with prompts

```{r}
hi_latex('1+1', prompt = TRUE)
hi_latex(c('  if (TRUE ){', 'foo && bar}'), prompt = TRUE)
```
```
\hldef{> }\hlnum{1}\hlopt{+}\hlnum{1}
\hldef{> }  \hlkwa{if} \hldef{(}\hlnum{TRUE} \hldef{)\{}
\hldef{+ }\hldef{foo} \hlopt{&&} \hldef{bar\}}
```

# hi_latex() preserves blank lines

```{r}
hi_latex(c('1+1', '', 'foo(x=3) # comm'))
```
```
[1] "\\hlnum{1}\\hlopt{+}\\hlnum{1}\n"                                              
[2] "\\hlkwd{foo}\\hldef{(}\\hlkwc{x}\\hldef{=}\\hlnum{3}\\hldef{)} \\hlcom{# comm}"
```

# the fallback method recognizes comments, functions and strings

```{r}
hi_latex('1+1 # a comment', fallback = TRUE)
hi_latex('paste("STRING", \'string\')', fallback = TRUE)
```
```
1+1 \hlcom{# a comment}
\hlkwd{paste}(\hlsng{"STRING"}, \hlsng{'string'})
```

# the fallback mode is used when the code does not parse

```{r}
has_warning(res <- hi_latex('1+1+ # comment'))
res
```
```
[1] TRUE
1+1+ \hlcom{# comment}
```

# hilight() works even if code only contains comments

```{r}
hi_latex('# only comments')
```
```
[1] "\\hlcom{# only comments}"
```

# the right arrow -> is preserved

```{r}
hi_latex('1 ->x # foo')
```
```
[1] "\\hlnum{1} \\hlkwb{->}\\hldef{x} \\hlcom{# foo}"
```

# blank lines before/after code are preserved

```{r}
hi_latex(c('', '', '1'))
hi_latex(c('', '', '1', ''))
```
```
[1] "\n"         "\\hlnum{1}"
[1] "\n"         "\\hlnum{1}" ""          
```

# custom markup also works

```{r}
hi_html('1+ 1')
my_cmd = cmd_html
my_cmd['NUM_CONST', 1] = '<span class="my num">'
hi_html('1+ 1', markup = my_cmd)
rm(my_cmd)
```
```
[1] "<span class=\"hl num\">1</span><span class=\"hl opt\">+</span> <span class=\"hl num\">1</span>"
[1] "<span class=\"my num\">1</span><span class=\"hl opt\">+</span> <span class=\"my num\">1</span>"
```
