
test_that("No errors", {
  dataset <- tibble::tibble(
    StartDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:00'), lubridate::mdy_hms( 'Feb 1 2022 10:00:00')),
    EndDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:10'), lubridate::mdy_hms( 'Feb 1 2022 10:00:30')),
    Duration__in_seconds_ = c(600, 1800)
  )

  expect_error(
    methodology(dataset, 100000),
    regexp = NA
  )
})


test_that("MOEs", {
  dataset <- tibble::tibble(
    StartDate = c(rep(lubridate::mdy_hms('Jan 1 2022 10:00:00'), 100)),
    EndDate = c(rep(lubridate::mdy_hms('Jan 1 2022 10:00:10'), 100)),
    Duration__in_seconds_ = c(rep(600, 100))
  )

  expect_equal(
    get_moe(dataset, population = 100000),
    'MOE: +/- 9.80'
  )
})


test_that("Field Dates", {
  dataset <- tibble::tibble(
    StartDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:00'), lubridate::mdy_hms( 'Feb 1 2022 10:00:00')),
    EndDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:10'), lubridate::mdy_hms( 'Feb 1 2022 10:00:30')),
    Duration__in_seconds_ = c(600, 1800)
  )

  expect_equal(
    get_field_dates(dataset),
    'Fielded: Jan 1 - Feb 1'
  )
})


test_that("LOIs", {
  dataset <- tibble::tibble(
    StartDate = c(rep(lubridate::mdy_hms('Jan 1 2022 10:00:00'), 98), lubridate::mdy_hms( 'Feb 1 2022 10:00:00')),
    EndDate = c(rep(lubridate::mdy_hms('Jan 1 2022 10:00:10'), 98), lubridate::mdy_hms( 'Feb 1 2022 10:00:30')),
    Duration__in_seconds_ = c(rep(600, 98), 1800)
  )

  loi_minutes_untrimmed <- (mean(dataset$Duration__in_seconds_) / 60) %>% round(1)
  loi_minutes_trimmed <- (mean(dataset$Duration__in_seconds_, trim = 0.025) / 60)  %>% round(1)
  loi_minutes_untrimmed_char <- stringr::str_c('Mean LOI: ', loi_minutes_untrimmed, ' minutes')
  loi_minutes_trimmed_char <- stringr::str_c('Mean LOI trimmed: ', loi_minutes_trimmed, ' minutes')

  expect_equal(
    loi_minutes_untrimmed_char,
    'Mean LOI: 10.2 minutes'
  )
  expect_equal(
    loi_minutes_trimmed_char,
    'Mean LOI trimmed: 10 minutes'
  )
})

