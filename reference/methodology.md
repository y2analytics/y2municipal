# Get info on survey methodology

Get the following survey methodology information: Length of interview
(LOI) Fielding dates Survey margin of error (MOE)

## Usage

``` r
methodology(
  dataset,
  population,
  start_date = StartDate,
  end_date = EndDate,
  duration = Duration__in_seconds_
)

jarvis_sommelier_the_survey(
  dataset,
  population,
  start_date = StartDate,
  end_date = EndDate,
  duration = Duration__in_seconds_
)
```

## Arguments

- dataset:

  A dataframe for which you want the methodology information.

- population:

  The population size your survey is based on (e.g. number of voters in
  the city)

- start_date:

  Unquoted name of the start date column (default: StartDate)

- end_date:

  Unquoted name of the end date column (default: EndDate)

- duration:

  Unquoted name of the interview duration column in seconds (default:
  Duration\_\_in_seconds\_)

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
