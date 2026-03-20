# Get info on survey methodology

Get the following survey methodology information: Length of interview
(LOI) Fielding dates Survey margin of error (MOE)

## Usage

``` r
methodology(dataset, population)

jarvis_sommelier_the_survey(dataset, population)
```

## Arguments

- dataset:

  A dataframe for which you want the methodology information. Your
  dataset needs the following standard variables from Qualtrics to work:
  StartDate, EndDate, Duration\_\_in_seconds\_

- population:

  The population size your survey is based on (e.g. number of voters in
  the city)

## Value

Methodology information printed to your console

## Examples

``` r
methodology(municipal_data, 30000)
#> Mean LOI: 0 minutes
#> Mean LOI trimmed: 0 minutes
#> Median LOI trimmed: 0 minutes
#> Fielded: Mar 24 - Mar 24
#> MOE: +/- 9.80
```
