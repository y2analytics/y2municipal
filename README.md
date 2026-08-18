
<!-- README.md is generated from README.Rmd. Please edit that file -->

# y2municipal

<!-- badges: start -->

<!-- badges: end -->

## Overview

`y2municipal` is the flagship package from Y2 Analytics to automate
municipal reports. It relies heavily on other y2 packages, `y2clerk` and
`orderlabel`, so be sure to have those installed first.

The goal of `y2municipal` is to quickly and easily go through the
standard processes used in every municipal project of:

1)  Clean the voter file:
2)  Weight the data:
3)  Create a topline report: `topline()` - uses freqs(unweighted_ns =
    TRUE under the hood)
4)  Load in data, y2 fonts, and create a names df:
    `read_data_names_fonts()`
5)  Create visualizations:

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("y2analytics/y2municipal")
```

## Examples

Below you will find a few basic examples which show you how to quickly
get a frequencies table with `freqs()`:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(y2clerk)
#> 
#> Attaching package: 'y2clerk'
#> The following object is masked from 'package:graphics':
#> 
#>     stem
library(orderlabel)
library(y2municipal)

# Run a weighted frequencies with unweighted ns
frequencies <- municipal_data |>
  freqs(s_sex, wt = weights, unweighted_ns = TRUE)

# Run a topline
DATA_PATH <- '~/Desktop/'
municipal_data |> topline_freqs(weight_var = weights)
#> In addition to standard Qualtrics variables, the following variables from your dataset were not included in the topline:
#> d_yearborn
#> Variable stem "m_race" successfully freq'd
#> # m_race_1: Are you: - Selected Choice American Indian / Native American
#> # m_race_2: Are you: - Selected Choice Asian
#> # m_race_3: Are you: - Selected Choice Black / African American
#> # ℹ 4 more questions with labels
#> # 
#> # A frequency tibble: 89 × 8
#>    variable    prompt                     value label     n stat  result base_ns
#>    <chr>       <chr>                      <chr> <chr> <dbl> <chr>  <dbl>   <dbl>
#>  1 s_qualify   Do you currently live in … "1"   Yes      23 perc…   0.24     100
#>  2 s_qualify   Do you currently live in … "2"   No       34 perc…   0.34     100
#>  3 s_qualify   Do you currently live in … "3"   Don'…    28 perc…   0.27     100
#>  4 s_qualify   Do you currently live in … "4"   Refu…    15 perc…   0.15     100
#>  5 s_direction Overall, would you say [I… "1"   Righ…    49 perc…   0.5      100
#>  6 s_direction Overall, would you say [I… "2"   Wron…    51 perc…   0.5      100
#>  7 n_overall_1 All things considered, on… ""    1       100 mean   54.3      100
#>  8 s_5year     How would you rate [INSER… "1"   Much…    16 perc…   0.15     100
#>  9 s_5year     How would you rate [INSER… "2"   Some…     9 perc…   0.11     100
#> 10 s_5year     How would you rate [INSER… "3"   Abou…    23 perc…   0.23     100
#> # ℹ 79 more rows
```

## Help

If you have issues using y2municipal, please post your issue on
[GitHub](https://github.com/y2analytics/y2municipal/issues) along with a
minimal reproducible example. We will do our best to address your issues
and get them fixed for the next version of y2municipal.
