---
title: "Combining Height Datesets from the 1700s to 1900s"
author: "Your Name here"

editor: visual
execute:
  keep-md: true

date: "November 04, 2024"
warnings: false
format:
  html:
    df-print: paged
    code-fold: true
    code-line-numbers: true
---


::: {.cell}

```{.r .cell-code}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, foreign, ggpubr)

# leaving this here because I know it will cause problems
# the zip file, you just need to run the unzip line of code once the comment out
# when referring to the datasets, either use the current df name or edit accordingly
germancon <- read_dta("data/germanconscr.dta")
prison <- read_dta("data/germanprison.dta")
# unzip("data/Heights_south-east.zip", exdir = "data/Heights_south-east")
g18 <- read.dbf("data/Heights_south-east/B6090.DBF")
main <- read_sav("data/main05022005.sav")
BLS <- read_csv("https://raw.githubusercontent.com/hadley/r4ds/main/data/heights.csv")
```
:::
