
<!-- README.md is generated from README.Rmd. Please edit that file -->
y2clerk
=======

<!-- badges: start -->
<!-- badges: end -->
Overview
--------

`y2municipal` is the flagship package from Y2 Analytics to automate municipal reports. It relies heavily on other y2 packages, `y2clerk` and `orderlabel`, so be sure to have those installed first.

The goal of `y2municipal` is to quickly and easily go through the standard processes used in every municipal project of:

1.  Clean the voter file:
2.  Weight the data:
3.  Create a topline report: `topline()` - uses freqs(unweighted\_ns = TRUE under the hood)
4.  Load in data, y2 fonts, and create a names df: `read_data_names_fonts()`
5.  Create visualizations:

Installation
------------

<!-- You can install the released version of y2municipal from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("y2municipal") -->
<!-- ``` -->
And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nick-moffitt/y2municipal")
```

Examples
--------

Below you will find a few basic examples which show you how to quickly get a frequencies table with `freqs()`:

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
library(orderlabel)
library(y2municipal)

# Run a weighted frequencies with unweighted ns
frequencies <- municipal_data %>%
  freqs_wuw(s_sex)

# Run a topline
DATA_PATH <- '~/Desktop/'
municipal_data %>% topline(weight_var = weights)
#> Warning in names_checker(dataset, {: The following variables from your dataset were not included in the topline:
#> d_yearborn
```

Help
----

If you have issues using y2municipal, please post your issue on [GitHub](https://github.com/nick-moffitt/y2municipal/issues) along with a minimal reproducible example. We will do our best to address your issues and get them fixed for the next version of y2municipal.
