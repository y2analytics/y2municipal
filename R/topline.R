# Public function ---------------------------------------------------------
### topline

#' Create all data for a topline
#'
#' Use topline() to automate all the frequencies for a topline report. This function can only be used if your questions have the proper prefixes:
#'  "s_" for single select,
#'  "m_" for multiple select,
#'  "oe_" for open ends,
#'  "n_" for numeric,
#'  "r_" for ranked,
#'  "md_" for max diff.
#'
#' @keywords freqs topline
#' @param dataset A dataframe for which you want to create a topline
#' @param file_name_topline The filename you want to give your topline freqs table csv. Will be saved into your DATA_PATH
#' @param file_name_appendix The filename you want to give your open ends appendix csv. Will be saved into your DATA_PATH
#' @param weight_var DEFAULT = weights. Can be set to NULL if the data has no weights
#' @export
#' @return A frequencies table including all questions with the prefixes mentioned above.
#' @examples
#'

topline <- function(
  dataset,
  file_name_topline,
  file_name_appendix,
  weight_var = weights
) {

  weights <- NULL

  # Closed ended questions
  frequencies <- run_combine_freqs(dataset, {{weight_var}})
  readr::write_csv(
    frequencies,
    stringr::str_c(
      DATA_PATH,
      file_name_topline,
      '.csv'
    )
  )

  # Open ended questions
  run_freqs_oe(dataset, file_name_appendix)
}



# Private functions -------------------------------------------------------

# single select: run_freq_s
run_freq_s <- function(dataset, weight_var) {
freqs_s <- dataset %>%
  dplyr::select(
    tidyselect::starts_with('s_'),
    tidyselect::starts_with('md_'),
    -tidyselect::ends_with('_TEXT'),
    {{ weight_var }}
  ) %>%
  freqs_wuw(
    prompt = TRUE,
    wt = {{ weight_var }},
    nas = FALSE
  )
}


# multi select: run_freq_m
run_freq_m <- function(dataset, weight_var) {
freqs_m <- dataset %>%
  dplyr::select(
    tidyselect::starts_with('m_'),
    -tidyselect::ends_with('_TEXT'),
    {{ weight_var }}
  ) %>%
  freqs_wuw(
    prompt = TRUE,
    wt = {{ weight_var }}
  ) %>%
  stats::na.omit()
}


# numeric questions: run_freq_n
run_freq_n <- function(dataset, weight_var) {
  freqs_m <- dataset %>%
    dplyr::select(
      tidyselect::starts_with('cs_'),
      tidyselect::starts_with('sl_'),
      tidyselect::starts_with('n_'),
      tidyselect::starts_with('r_'),
      -tidyselect::ends_with('_TEXT'),
      {{ weight_var }}
    ) %>%
    dplyr::mutate_all(
      ~forcats::as_factor(.) %>%
        as.character() %>%
        as.numeric()
      ) %>%
    freqs_wuw(
      prompt = TRUE,
      stat = 'mean',
      wt = {{ weight_var }},
      nas = FALSE
    )
}


# combine all freqs
run_combine_freqs <- function(
  dataset,
  weight_var) {
    freqs_s <- run_freq_s(dataset, {{weight_var}})
    freqs_m <- run_freq_m(dataset, {{weight_var}})
    freqs_n <- run_freq_n(dataset, {{weight_var}})

    frequencies <- dplyr::bind_rows(
      freqs_s,
      freqs_m,
      freqs_n
    )
}


# open ends: run_freqs_oe
run_freqs_oe <- function(dataset, file_name_appendix) {
  freqs_oe <- dataset %>%
    dplyr::select(
      tidyselect::starts_with('oe_'),
      tidyselect::ends_with('_TEXT')
    ) %>%
    y2clerk::verbatims_y2()

  readr::write_csv(
    freqs_oe,
    stringr::str_c(
      DATA_PATH,
      file_name_appendix,
      '.csv'
    )
  )

}

