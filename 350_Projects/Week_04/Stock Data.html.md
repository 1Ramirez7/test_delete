---
title: "Stock Data"

editor: visual
execute:
  keep-md: true

date: "December 04, 2024"
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
pacman::p_load(reader, ggplot2, tidyverse, dplyr, kableExtra, knitr
)

stock_data <- read_rds('https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS')
```
:::



::: {.cell}

```{.r .cell-code}
stock <- stock_data |>
  separate(contest_period, into = c("month_start", "month_end_year"), sep = "-") |> #resutls1
  separate(month_end_year, into = c("month_end", "year"), sep = "(?<=\\D)(?=\\d)") |> # results2
  select(-month_start) |> # results3
  filter(variable == "DJIA", !is.na(month_end)) |> # results4
  group_by(year, month_end) |> # results5
  summarise(return = value) |> # result6
  mutate(
    month_end = case_when(
      month_end == "Jan." ~ 'January',
      month_end == "Feb." ~ 'February',
      month_end == "Mar." ~ 'March',
      month_end == "Apr." ~ 'April',
      month_end == "May" ~ 'May',
      month_end == "Jun." ~ 'June',
      month_end == "Jul." ~ 'July',
      month_end == "Aug." ~ 'August',
      month_end == "Sep." ~ 'September',
      month_end == "Oct." ~ 'October',
      month_end == "Nov." ~ 'November',
      month_end == "Dec." ~ 'December',
      month_end == "Febuary" ~ 'February',
      is.na(month_end) ~ '-',
      TRUE ~ as.character(month_end)
    )
  ) |> # results7
  pivot_wider(names_from = year, values_from = return) |> # results8
  ungroup() |> # results9
  mutate(month_end = factor(month_end, levels = month.name)) |>
  arrange(month_end) |>
  rename(Month = month_end)

kable(stock, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

::: {.cell-output-display}

`````{=html}
<table style="width:100%; width: auto !important; margin-left: auto; margin-right: auto;" class="table table-striped table-hover">
 <thead>
  <tr>
   <th style="text-align:left;"> Month </th>
   <th style="text-align:right;"> 1990 </th>
   <th style="text-align:right;"> 1991 </th>
   <th style="text-align:right;"> 1992 </th>
   <th style="text-align:right;"> 1993 </th>
   <th style="text-align:right;"> 1994 </th>
   <th style="text-align:right;"> 1995 </th>
   <th style="text-align:right;"> 1996 </th>
   <th style="text-align:right;"> 1997 </th>
   <th style="text-align:right;"> 1998 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> January </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> -0.8 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> -0.8 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> -0.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> February </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 11.0 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 15.6 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 10.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> March </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 7.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> April </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 12.8 </td>
   <td style="text-align:right;"> 14.8 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 22.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> May </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 17.3 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 19.5 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 13.3 </td>
   <td style="text-align:right;"> 10.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> June </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 17.7 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> -6.2 </td>
   <td style="text-align:right;"> 16.0 </td>
   <td style="text-align:right;"> 10.2 </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 15.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> July </td>
   <td style="text-align:right;"> 11.5 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> -5.3 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 7.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> August </td>
   <td style="text-align:right;"> -2.3 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> -0.3 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> -13.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> September </td>
   <td style="text-align:right;"> -9.2 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> -0.1 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> -11.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> October </td>
   <td style="text-align:right;"> -8.5 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> -5.0 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> November </td>
   <td style="text-align:right;"> -12.8 </td>
   <td style="text-align:right;"> -3.3 </td>
   <td style="text-align:right;"> -2.8 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> -0.3 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> December </td>
   <td style="text-align:right;"> -9.3 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 9.3 </td>
   <td style="text-align:right;"> 15.5 </td>
   <td style="text-align:right;"> -0.7 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>

`````

:::
:::

::: {.cell}

```{.r .cell-code}
saveRDS(stock, file = "tidy_stock.rds")
```
:::
