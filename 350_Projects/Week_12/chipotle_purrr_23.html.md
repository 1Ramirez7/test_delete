---
title: "Task/project name here"

editor: visual
execute:
  keep-md: true
#  freeze: true alternative way to only render this doc and not all 
# maybe the gh-pages setting cause all to render and docs dont? more testing

date: "November 04, 2024"
warnings: false
format:
  html:
    df-print: paged
    code-fold: true
    code-line-numbers: true
---



## Dow-Jones Industrial Average Returns


::: {.cell}

```{.r .cell-code}
# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reader, ggplot2)
```
:::
