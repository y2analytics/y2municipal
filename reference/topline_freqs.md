# Create all data for a topline

Use topline_freqs() to automate all the quantitative frequencies for a
topline report. This function works best if your questions have the
proper prefixes:

1.  "s\_" for single select,

2.  "m\_" for multiple select,

3.  "oe\_" for open ends,

4.  "n\_" for numeric,

5.  "r\_" for ranked,

6.  "md\_" for max diff.

## Usage

``` r
topline_freqs(
  dataset,
  weight_var,
  assign_s = NULL,
  assign_m = NULL,
  assign_n = NULL,
  unweighted_ns = TRUE,
  silently = FALSE
)

jarvis_top_us_all_off(
  dataset,
  weight_var,
  assign_s = NULL,
  assign_m = NULL,
  assign_n = NULL,
  unweighted_ns = TRUE,
  silently = FALSE
)
```

## Arguments

- dataset:

  A dataframe for which you want to create a topline

- weight_var:

  Variable containing weights

- assign_s:

  DEFAULT = NULL, A vector of unquoted variables to be treated as single
  select variables, put within c()

- assign_m:

  DEFAULT = NULL, A vector of unquoted variables to be treated as
  multiple select variables, put within c()

- assign_n:

  DEFAULT = NULL, A vector of unquoted variables to be treated as
  numeric variables, put within c()

- unweighted_ns:

  DEFAULT = TRUE, Display weighted or unweighted n-sizes in topline
  report

- silently:

  DEFAULT = FALSE, Hide message output (e.g., progress of completing
  freqs on variables or printing of variables not included in the
  topline)

## Value

A tibble of frequencies

## Examples

``` r
municipal_data %>%
topline_freqs()
#> In addition to standard Qualtrics variables, the following variables from your dataset were not included in the topline:
#> d_yearborn
#> Variable stem "m_race" successfully freq'd
#> # A tibble: 89 × 8
#>    variable    prompt                     value label     n stat  result base_ns
#>    <chr>       <chr>                      <chr> <chr> <dbl> <chr>  <dbl>   <dbl>
#>  1 s_qualify   Do you currently live in … "1"   Yes      23 perc…   0.23     100
#>  2 s_qualify   Do you currently live in … "2"   No       34 perc…   0.34     100
#>  3 s_qualify   Do you currently live in … "3"   Don'…    28 perc…   0.28     100
#>  4 s_qualify   Do you currently live in … "4"   Refu…    15 perc…   0.15     100
#>  5 s_direction Overall, would you say [I… "1"   Righ…    49 perc…   0.49     100
#>  6 s_direction Overall, would you say [I… "2"   Wron…    51 perc…   0.51     100
#>  7 n_overall_1 All things considered, on… ""    1       100 mean   54.2      100
#>  8 s_5year     How would you rate [INSER… "1"   Much…    16 perc…   0.16     100
#>  9 s_5year     How would you rate [INSER… "2"   Some…     9 perc…   0.09     100
#> 10 s_5year     How would you rate [INSER… "3"   Abou…    23 perc…   0.23     100
#> # ℹ 79 more rows

municipal_data %>%
  topline_freqs(
    assign_n = c(d_yearborn, Duration__in_seconds_),
    weight_var = weights
)
#> Variable stem "m_race" successfully freq'd
#> # A tibble: 91 × 8
#>    variable              prompt           value label     n stat  result base_ns
#>    <chr>                 <chr>            <chr> <chr> <dbl> <chr>  <dbl>   <dbl>
#>  1 Duration__in_seconds_ Duration (in se… ""    Dura…   100 mean    0.01     100
#>  2 s_qualify             Do you currentl… "1"   Yes      23 perc…   0.24     100
#>  3 s_qualify             Do you currentl… "2"   No       34 perc…   0.34     100
#>  4 s_qualify             Do you currentl… "3"   Don'…    28 perc…   0.27     100
#>  5 s_qualify             Do you currentl… "4"   Refu…    15 perc…   0.15     100
#>  6 s_direction           Overall, would … "1"   Righ…    49 perc…   0.5      100
#>  7 s_direction           Overall, would … "2"   Wron…    51 perc…   0.5      100
#>  8 n_overall_1           All things cons… ""    1       100 mean   54.3      100
#>  9 s_5year               How would you r… "1"   Much…    16 perc…   0.15     100
#> 10 s_5year               How would you r… "2"   Some…     9 perc…   0.11     100
#> # ℹ 81 more rows
```
