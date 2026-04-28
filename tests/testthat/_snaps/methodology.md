# methodology full output

    Code
      methodology(dataset, 1e+05)
    Output
      Mean LOI: 10 minutes
      Mean LOI trimmed: 10 minutes
      Median LOI trimmed: 10 minutes
      Fielded: Jan 1 - Feb 1
      MOE: +/- 9.80

# methodology warns and continues with missing date columns

    Code
      methodology(dataset, 1e+05)
    Condition
      Warning in `get_field_dates()`:
      Could not find StartDate and/or EndDate columns - skipping fielding dates
    Output
      Mean LOI: 10 minutes
      Mean LOI trimmed: 10 minutes
      Median LOI trimmed: 10 minutes
      MOE: +/- 9.80

# methodology warns and continues with missing duration column

    Code
      methodology(dataset, 1e+05)
    Condition
      Warning in `get_loi()`:
      Could not find Duration__in_seconds_ column - skipping LOI
    Output
      Fielded: Jan 1 - Feb 1
      MOE: +/- 69.30

# methodology respects custom column names

    Code
      methodology(dataset, 1e+05, start_date = survey_start, end_date = survey_end,
        duration = dur_secs)
    Output
      Mean LOI: 10 minutes
      Mean LOI trimmed: 10 minutes
      Median LOI trimmed: 10 minutes
      Fielded: Jan 1 - Feb 1
      MOE: +/- 9.80

