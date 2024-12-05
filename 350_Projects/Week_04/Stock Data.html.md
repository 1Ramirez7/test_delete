---
title: "Task/project name here"

editor: visual
execute:
  keep-md: true
#  freeze: true alternative way to only render this doc and not all 
# maybe the gh-pages setting cause all to render and docs dont? more testing

date: "`r format(Sys.time(), '%B %d, %Y')`"
warnings: false
format:
  html:
    df-print: paged
    code-fold: true
    code-line-numbers: true
---
