# Public function ---------------------------------------------------------
### methodology

#' Get info on survey methodology
#'
#' Get the following survey methodology information:
#'  Length of interview (LOI)
#'  Fielding dates
#'  Survey margin of error (MOE)
#'
#'
#' @keywords methodology
#' @param dataset A dataframe for which you want the methodology information.
#' @param population The population size your survey is based on (e.g. number of voters in the city)
#' @param start_date Unquoted name of the start date column (default: StartDate)
#' @param end_date Unquoted name of the end date column (default: EndDate)
#' @param duration Unquoted name of the interview duration column in seconds (default: Duration__in_seconds_)
#' @export
#' @return Methodology information printed to your console
#' @examples
#' methodology(municipal_data, 30000)
#'

methodology <- function(
  dataset,
  population,
  start_date = StartDate,
  end_date = EndDate,
  duration = Duration__in_seconds_
) {
  loi_char <- get_loi(dataset, {{ duration }})
  field_dates_char <- get_field_dates(dataset, {{ start_date }}, {{ end_date }})
  moe_char <- get_moe(dataset, population)

  cat(c(loi_char, field_dates_char, moe_char), sep = '\n')
}

#' @rdname methodology
#' @export
jarvis_sommelier_the_survey <- methodology


# Private functions -------------------------------------------------------
get_loi <- function(dataset, duration) {
  duration_q <- rlang::enquo(duration)
  col_name <- rlang::as_label(duration_q)

  if (!col_name %in% names(dataset)) {
    warning("Could not find ", col_name, " column - skipping LOI")
    return(NULL)
  }

  dur_values <- dplyr::pull(dataset, !!duration_q)
  loi_minutes_untrimmed <- (mean(dur_values) / 60) %>% round(1)
  loi_minutes_trimmed <- (mean(dur_values, trim = 0.025) / 60) %>% round(1)
  loi_minutes_median <- (stats::median(dur_values) / 60) %>% round(1)

  c(
    stringr::str_c('Mean LOI: ', loi_minutes_untrimmed, ' minutes'),
    stringr::str_c('Mean LOI trimmed: ', loi_minutes_trimmed, ' minutes'),
    stringr::str_c('Median LOI trimmed: ', loi_minutes_median, ' minutes')
  )
}


get_moe <- function(dataset, population) {
  sample_size <- dplyr::count(dataset)
  nadj <- (population - 1) * sample_size / (population - sample_size)
  moe <- ((1.96 * .5) / sqrt(nadj)) %>% round(3)

  stringr::str_c('MOE: +/- ', sprintf("%0.2f", moe * 100))
}


get_field_dates <- function(dataset, start_date, end_date) {
  start_q <- rlang::enquo(start_date)
  end_q <- rlang::enquo(end_date)
  start_name <- rlang::as_label(start_q)
  end_name <- rlang::as_label(end_q)

  if (!all(c(start_name, end_name) %in% names(dataset))) {
    warning(
      "Could not find ",
      start_name,
      " and/or ",
      end_name,
      " columns - skipping fielding dates"
    )
    return(NULL)
  }

  start_col <- dplyr::pull(dataset, !!start_q)
  end_col <- dplyr::pull(dataset, !!end_q)
  start_month <- min(start_col) %>% lubridate::month(label = TRUE)
  start_day <- min(start_col) %>% lubridate::day()
  end_month <- max(end_col) %>% lubridate::month(label = TRUE)
  end_day <- max(end_col) %>% lubridate::day()

  stringr::str_c(
    "Fielded:",
    start_month,
    start_day,
    "-",
    end_month,
    end_day,
    sep = ' '
  )
}
