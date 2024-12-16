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
#' @param dataset A dataframe for which you want the methodology information. Your dataset needs the following standard variables from Qualtrics to work: StartDate, EndDate, Duration__in_seconds_
#' @param population The population size your survey is based on (e.g. number of voters in the city)
#' @export
#' @return Methodology information printed to your console
#' @examples
#' methodology(municipal_data, 30000)
#'


methodology <- function(dataset, population) {

  # LOI
  loi_minutes_untrimmed <- (mean(dataset$Duration__in_seconds_) / 60) %>% round(1)
  loi_minutes_trimmed <- (mean(dataset$Duration__in_seconds_, trim = 0.025) / 60)  %>% round(1)
  loi_minutes_median <- (stats::median(dataset$Duration__in_seconds_) / 60)  %>% round(1)
  loi_minutes_untrimmed_char <- stringr::str_c('Mean LOI: ', loi_minutes_untrimmed, ' minutes')
  loi_minutes_trimmed_char <- stringr::str_c('Mean LOI trimmed: ', loi_minutes_trimmed, ' minutes')
  loi_minutes_median_char <- stringr::str_c('Median LOI trimmed: ', loi_minutes_median, ' minutes')

  # Field dates
  field_dates_char <- get_field_dates(dataset)

  # MOE
  moe_char <- get_moe(dataset, population)

  # Print it all
  cat(
    loi_minutes_untrimmed_char,
    loi_minutes_trimmed_char,
    loi_minutes_median_char,
    field_dates_char,
    moe_char,
    sep = '\n'
    )

}

#' @rdname methodology
#' @export
jarvis_sommelier_the_survey <- methodology


# Private functions -------------------------------------------------------
get_moe <- function(dataset, population) {
  sample_size <- dplyr::count(dataset)
  nadj <- (population - 1) * sample_size / (population - sample_size)

  moe <- ((1.96 *.5) / sqrt(nadj)) %>% round(3)
  moe_char <- stringr::str_c('MOE: +/- ', sprintf("%0.2f", moe * 100))
}


get_field_dates <- function(dataset) {
  start_month <- min(dataset$StartDate) %>% lubridate::month(label = TRUE)
  start_day <- min(dataset$StartDate) %>% lubridate::day()
  end_month <- max(dataset$EndDate) %>% lubridate::month(label = TRUE)
  end_day <- max(dataset$EndDate) %>% lubridate::day()
  field_dates_char <- stringr::str_c(
    "Fielded:",
    start_month,
    start_day,
    "-",
    end_month,
    end_day,
    sep = ' '
  )
}
