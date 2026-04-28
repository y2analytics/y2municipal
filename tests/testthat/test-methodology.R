
# Snapshot tests for methodology() ----------------------------------------

test_that("methodology full output", {
  dataset <- tibble::tibble(
    StartDate             = rep(lubridate::mdy_hms('Jan 1 2022 10:00:00'), 100),
    EndDate               = rep(lubridate::mdy_hms('Feb 1 2022 10:00:30'), 100),
    Duration__in_seconds_ = rep(600, 100)
  )
  expect_snapshot(methodology(dataset, 100000))
})

test_that("methodology warns and continues with missing date columns", {
  dataset <- tibble::tibble(Duration__in_seconds_ = rep(600, 100))
  expect_snapshot(methodology(dataset, 100000))
})

test_that("methodology warns and continues with missing duration column", {
  dataset <- tibble::tibble(
    StartDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:00'), lubridate::mdy_hms('Feb 1 2022 10:00:00')),
    EndDate   = c(lubridate::mdy_hms('Jan 1 2022 10:00:10'), lubridate::mdy_hms('Feb 1 2022 10:00:30'))
  )
  expect_snapshot(methodology(dataset, 100000))
})

test_that("methodology respects custom column names", {
  dataset <- tibble::tibble(
    survey_start = rep(lubridate::mdy_hms('Jan 1 2022 10:00:00'), 100),
    survey_end   = rep(lubridate::mdy_hms('Feb 1 2022 10:00:30'), 100),
    dur_secs     = rep(600, 100)
  )
  expect_snapshot(
    methodology(dataset, 100000, start_date = survey_start, end_date = survey_end, duration = dur_secs)
  )
})


# Unit tests for private functions ----------------------------------------

test_that("get_moe calculation", {
  dataset <- tibble::tibble(
    StartDate             = rep(lubridate::mdy_hms('Jan 1 2022 10:00:00'), 100),
    EndDate               = rep(lubridate::mdy_hms('Jan 1 2022 10:00:10'), 100),
    Duration__in_seconds_ = rep(600, 100)
  )
  expect_equal(get_moe(dataset, population = 100000), 'MOE: +/- 9.80')
})

test_that("get_field_dates formats correctly", {
  dataset <- tibble::tibble(
    StartDate = c(lubridate::mdy_hms('Jan 1 2022 10:00:00'), lubridate::mdy_hms('Feb 1 2022 10:00:00')),
    EndDate   = c(lubridate::mdy_hms('Jan 1 2022 10:00:10'), lubridate::mdy_hms('Feb 1 2022 10:00:30'))
  )
  expect_equal(get_field_dates(dataset, StartDate, EndDate), 'Fielded: Jan 1 - Feb 1')
})

test_that("get_field_dates returns NULL with warning when columns missing", {
  dataset <- tibble::tibble(x = 1)
  expect_warning(result <- get_field_dates(dataset, StartDate, EndDate), "StartDate")
  expect_null(result)
})

test_that("get_loi returns NULL with warning when column missing", {
  dataset <- tibble::tibble(x = 1)
  expect_warning(result <- get_loi(dataset, Duration__in_seconds_), "Duration")
  expect_null(result)
})

test_that("get_loi LOI values", {
  dataset <- tibble::tibble(
    Duration__in_seconds_ = c(rep(600, 98), 1800)
  )
  loi <- get_loi(dataset, Duration__in_seconds_)
  expect_equal(loi[[1]], 'Mean LOI: 10.2 minutes')
  expect_equal(loi[[2]], 'Mean LOI trimmed: 10 minutes')
})
