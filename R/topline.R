# Public function ---------------------------------------------------------
### topline

#' Create all data for a topline
#'
#' Use topline() to automate all the frequencies for a topline report. This function can only be used if your questions have the proper prefixes:
#'  1. "s_" for single select,
#'  2. "m_" for multiple select,
#'  3. "oe_" for open ends,
#'  4. "n_" for numeric,
#'  5. "r_" for ranked,
#'  6. "md_" for max diff.
#'  You need to be using y2 coding conventions and have a DATA_PATH in your R environment because topline() will save your new files to that folder
#'
#' @keywords freqs topline
#' @param dataset A dataframe for which you want to create a topline
#' @param weight_var Variable containing weights
#' @export
#' @return 2 tables saved out into your DATA_PATH folder
#' 1) A csv of frequencies for the topline called "data for topline, *project name*"
#' 2) A csv of open ends for the appendix called "data for appendix, *project name*"
#' @examples
#' \dontrun{
#' DATA_PATH <- '~/Desktop/'
#' topline(municipal_data)
#'}
#'

topline <- function(
  dataset,
  weight_var
) {

  # Pre- work

  PROJECT_NAME <- stringr::str_to_lower(DATA_PATH) %>%
    stringr::str_remove('/data/') %>%
    stringr::str_remove('/data') %>%
    stringr::str_remove('.*/')


  # Make fake weights if no weights supplied
  if ( dataset %>%
       dplyr::select(
         {{ weight_var }}
       ) %>%
       names() %>%
       length == 0 ) {

    dataset <- dataset %>%
      dplyr::mutate(weights = 1)

    weight_var <- as.symbol('weights')

  }

  # Make sure all vars available
  dataset <- create_dummy_vars(dataset)

  # Closed ended questions
  frequencies <- run_combine_freqs(dataset, {{weight_var}}, PROJECT_NAME)

  # Open ended questions
  run_freqs_oe(dataset, PROJECT_NAME)

  # Check missing vars
  names_checker(dataset, {{weight_var}})
}

#' @rdname topline
#' @export
jarvis_top_us_all_off <- topline


# Private functions -------------------------------------------------------

# create dummies
create_dummy_vars <- function(dataset) {
  dataset <- dataset %>%
    dplyr::mutate(
      s_xyz123 = NA_character_,
      m_xyz123 = NA_character_,
      n_xyz123 = NA_real_,
      oe_xyz123 = NA_character_
    )
}

# single select: run_freq_s
run_freq_s <- function(dataset, weight_var) {
freqs_s <- dataset %>%
  dplyr::select(
    tidyselect::starts_with('s_'),
    -tidyselect::ends_with('_TEXT'),
    {{ weight_var }}
  ) %>%
  y2clerk::freqs(
    prompt = TRUE,
    wt = {{ weight_var }},
    nas = FALSE,
    unweighted_ns = TRUE
  )
}


# multi select: run_freq_m
run_freq_m <- function(dataset, weight_var) {
freqs_m <- dataset %>%
  dplyr::select(
    tidyselect::starts_with('m_'),
    tidyselect::starts_with('md_'),
    -tidyselect::ends_with('_TEXT'),
    {{ weight_var }}
  ) %>%
  y2clerk::multi_freqs(
    prompt = TRUE,
    wt = {{ weight_var }},
    unweighted_ns = TRUE
  )
}


# numeric questions: run_freq_n
run_freq_n <- function(dataset, weight_var) {

  labels_list <- dataset %>%
    dplyr::select(
      tidyselect::starts_with('cs_'),
      tidyselect::starts_with('sl_'),
      tidyselect::starts_with('n_'),
      tidyselect::starts_with('r_'),
      -tidyselect::ends_with('_TEXT'),
    )
  labels <- tibble::tibble(
    prompt = labelled::var_label(labels_list) %>% as.character(),
    label = .data$prompt,
    variable = names(labels_list)
  )

  freqs_n <- dataset %>%
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
    y2clerk::freqs(
      stat = 'mean',
      wt = {{ weight_var }},
      nas = FALSE,
      unweighted_ns = TRUE
    ) %>%
    dplyr::select(-.data$label) %>%
    dplyr::left_join(labels, by = 'variable') %>%
    dplyr::mutate(
      label = stringr::str_remove(.data$label, '.*\n'),
      label = stringr::str_remove(.data$label, '.*- '),
      label = stringr::str_trim(.data$label)
    ) %>%
    dplyr::relocate(
      .data$label,
      .after = .data$value
    ) %>%
    dplyr::filter(.data$variable != 'n_xyz123')
}


# combine all freqs
run_combine_freqs <- function(
  dataset,
  weight_var,
  PROJECT_NAME) {
    freqs_s <- run_freq_s(dataset, {{weight_var}})
    freqs_m <- run_freq_m(dataset, {{weight_var}})
    freqs_n <- run_freq_n(dataset, {{weight_var}})

    frequencies <- dplyr::bind_rows(
      freqs_s,
      freqs_m,
      freqs_n
    )

    readr::write_csv(
      frequencies,
      stringr::str_c(
        DATA_PATH,
        'data for topline, ',
        PROJECT_NAME,
        '.csv'
      )
    )
}


# open ends: run_freqs_oe
run_freqs_oe <- function(dataset, PROJECT_NAME) {
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
      'data for appendix, ',
      PROJECT_NAME,
      '.csv'
    )
  )

}


# Check var names
names_checker <- function(dataset, weight_var) {
  ACTUAL_NAMES <- orderlabel::taking_names(dataset)

  QUALTRICS_STANDARD_VARS <- c(
    'StartDate|EndDate|Status|IPAddress|Progress|Duration__in_seconds_|Finished|RecordedDate|ResponseId|RecipientLastName|RecipientFirstName|RecipientEmail|ExternalReference|LocationLatitude|LocationLongitude|DistributionChannel|UserLanguage|ExternalReference|term|gc|year_born_numeric|age_numeric|census_age_groups|lon|lat'
  )
  weight_quoed <- rlang::enquo(weight_var)
  weight_char <- rlang::quo_name(weight_quoed)

  leftover_vars <- ACTUAL_NAMES %>%
    dplyr::filter(
      !stringr::str_detect(.data$name, QUALTRICS_STANDARD_VARS),
      !stringr::str_detect(.data$name, '^s_'),
      !stringr::str_detect(.data$name, '^md_'),
      !stringr::str_detect(.data$name, '^m_'),
      !stringr::str_detect(.data$name, '^cs_'),
      !stringr::str_detect(.data$name, '^sl_'),
      !stringr::str_detect(.data$name, '^n_'),
      !stringr::str_detect(.data$name, '^r_'),
      !stringr::str_detect(.data$name, '^oe_'),
      !stringr::str_detect(.data$name, 'TEXT$'),
      !stringr::str_detect(.data$name, weight_char)
    ) %>%
    dplyr::select(.data$name) %>%
    purrr::as_vector()
  leftovers_1string <- paste(leftover_vars, collapse = ', ')

  if (length(leftover_vars) >= 1) {
    warning(
      stringr::str_c(
      "The following variables from your dataset were not included in the topline:\n",
      leftovers_1string
      )
    )
  }
}
