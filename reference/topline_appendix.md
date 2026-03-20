# Create open ended data for a topline

Use topline_appendix() to automate all the qualitative responses for a
topline report. This function works best if your questions have the
proper prefixes and suffixes:

1.  prefix "oe\_" for open ends

2.  suffix "\_TEXT" for open ends

## Usage

``` r
topline_appendix(dataset, assign_oe = NULL)

jarvis_listen_to_guests(dataset, assign_oe = NULL)
```

## Arguments

- dataset:

  A dataframe for which you want to create a topline

- assign_oe:

  DEFAULT = NULL, A vector of unquoted variables to be treated as
  open-ended variables, put within c()

## Value

A tibble of open-ended responses

## Examples

``` r
municipal_data %>%
topline_appendix()
#> # A tibble: 274 × 4
#>    variable    prompt                                              label base_ns
#>    <chr>       <chr>                                               <chr>   <int>
#>  1 oe_likemost What do you like most about living in [INSERT CITY… Feli…     100
#>  2 oe_likemost What do you like most about living in [INSERT CITY… Ulla…     100
#>  3 oe_likemost What do you like most about living in [INSERT CITY… Plac…     100
#>  4 oe_likemost What do you like most about living in [INSERT CITY… Inte…     100
#>  5 oe_likemost What do you like most about living in [INSERT CITY… Magn…     100
#>  6 oe_likemost What do you like most about living in [INSERT CITY… Bibe…     100
#>  7 oe_likemost What do you like most about living in [INSERT CITY… Plac…     100
#>  8 oe_likemost What do you like most about living in [INSERT CITY… At s…     100
#>  9 oe_likemost What do you like most about living in [INSERT CITY… Temp…     100
#> 10 oe_likemost What do you like most about living in [INSERT CITY… Est …     100
#> # ℹ 264 more rows

municipal_data %>%
  topline_appendix(
    assign_oe = c(RecipientEmail, UserLanguage)
  )
#> # A tibble: 274 × 4
#>    variable    prompt                                              label base_ns
#>    <chr>       <chr>                                               <chr>   <int>
#>  1 oe_likemost What do you like most about living in [INSERT CITY… Feli…     100
#>  2 oe_likemost What do you like most about living in [INSERT CITY… Ulla…     100
#>  3 oe_likemost What do you like most about living in [INSERT CITY… Plac…     100
#>  4 oe_likemost What do you like most about living in [INSERT CITY… Inte…     100
#>  5 oe_likemost What do you like most about living in [INSERT CITY… Magn…     100
#>  6 oe_likemost What do you like most about living in [INSERT CITY… Bibe…     100
#>  7 oe_likemost What do you like most about living in [INSERT CITY… Plac…     100
#>  8 oe_likemost What do you like most about living in [INSERT CITY… At s…     100
#>  9 oe_likemost What do you like most about living in [INSERT CITY… Temp…     100
#> 10 oe_likemost What do you like most about living in [INSERT CITY… Est …     100
#> # ℹ 264 more rows
```
