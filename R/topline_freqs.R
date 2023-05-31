# Public function ---------------------------------------------------------
### topline_freqs

#' Create all data for a topline
#'
#' Use topline_freqs() to automate all the quantitative frequencies for a topline report. This function works best if your questions have the proper prefixes:
#'  1. "s_" for single select,
#'  2. "m_" for multiple select,
#'  3. "oe_" for open ends,
#'  4. "n_" for numeric,
#'  5. "r_" for ranked,
#'  6. "md_" for max diff.
#'
#' @keywords freqs topline
#' @param dataset A dataframe for which you want to create a topline
#' @param weight_var Variable containing weights
#' @param assign_s DEFAULT = NULL, A vector of unquoted variables to be treated as single select variables, put within c()
#' @param assign_m DEFAULT = NULL, A vector of unquoted variables to be treated as multiple select variables, put within c()
#' @param assign_n DEFAULT = NULL, A vector of unquoted variables to be treated as numeric variables, put within c()
#' @param unweighted_ns DEFAULT = TRUE, Display weighted or unweighted n-sizes in topline report
#' @param silently DEFAULT = FALSE, Hide message output (e.g., progress of completing freqs on variables or printing of variables not included in the topline)
#' @return A tibble of frequencies
#' @importFrom rlang .data
#' @examples
#' municipal_data %>%
#' topline_freqs()
#'
#' municipal_data %>%
#'   topline_freqs(
#'     assign_n = c(d_yearborn, Duration__in_seconds_),
#'     weight_var = weights
#' )
#' @export


topline_freqs <- function(
    dataset,
    weight_var,
    assign_s = NULL,
    assign_m = NULL,
    assign_n = NULL,
    unweighted_ns = TRUE,
    silently = FALSE
) {


  # Check for grouping ------------------------------------------------------

  if (
    dplyr::is_grouped_df(
      dataset
    )
  ) {
    group_variables <-
      dataset %>%
      dplyr::group_vars()


    if (group_variables %>% length > 1) {
      error_message <-
        stringr::str_c(
          'Multiple grouping vars detected, only one grouping variable permitted. Detected grouping vars: ',
          stringr::str_flatten(group_variables, collapse = ', ')
        )

      stop(
        error_message
      )
    }

    dataset <-
      dataset %>%
      dplyr::ungroup()

    group_variable_labels <-
      dataset %>%
      dplyr::select(
        dplyr::all_of(group_variables)
      ) %>%
      y2clerk::freqs() %>%
      dplyr::pull(
        .data$label
      )

    grouped_data = TRUE
  } else {
    grouped_data = FALSE
  }




  # Check for a weight variable ---------------------------------------------

  if (
    dataset %>%
    dplyr::select(
      {{ weight_var }}
    ) %>%
    names() %>%
    length == 0
  ) {
    dataset <-
      dataset %>%
      dplyr::mutate(
        weights = 1
      )

    weight_var <-
      as.symbol('weights')
  }


  # Assign Variable types ---------------------------------------------------

  single_vars <-
    dataset %>%
    dplyr::select(
      tidyselect::starts_with('s_'),
      {{ assign_s }},
      -tidyselect::ends_with('_TEXT'),
      -{{ assign_m }},
      -{{ assign_n }}
    ) %>%
    names

  multi_vars <-
    dataset %>%
    dplyr::select(
      tidyselect::starts_with('m_'),
      tidyselect::starts_with('md_'),
      {{ assign_m }},
      -tidyselect::ends_with('_TEXT'),
      -{{ assign_s }},
      -{{ assign_n }}
    ) %>%
    names

  num_vars <-
    dataset %>%
    dplyr::select(
      tidyselect::starts_with('cs_'),
      tidyselect::starts_with('sl_'),
      tidyselect::starts_with('n_'),
      tidyselect::starts_with('r_'),
      {{ assign_n }},
      -tidyselect::ends_with('_TEXT'),
      -{{ assign_m }},
      -{{ assign_s }}
    ) %>%
    names

  # Get lists of run and unrun variables ------------------------------------

  survey_order <-
    dataset %>%
    dplyr::select(
      -c(
        !tidyselect::all_of(single_vars) &
          !tidyselect::all_of(multi_vars) &
          !tidyselect::all_of(num_vars)
      )
    ) %>%
    names

  unrun_vars <-
    dataset %>%
    dplyr::select(
      -{{ weight_var }},
      -tidyselect::starts_with('oe_'),
      -tidyselect::ends_with('_TEXT')
    ) %>%
    names %>%
    setdiff(
      c(
        'StartDate',
        'EndDate',
        'Status',
        'IPAddress',
        'Progress',
        'Duration__in_seconds_',
        'Finished',
        'RecordedDate',
        'ResponseId',
        'RecipientLastName',
        'RecipientFirstName',
        'RecipientEmail',
        'ExternalReference',
        'LocationLatitude',
        'LocationLongitude',
        'DistributionChannel',
        'UserLanguage',
        'ExternalReference',
        'term',
        'gc'
      )
    ) %>%
    setdiff(
      survey_order
    )


  if (length(unrun_vars) >= 1 & silently == FALSE) {
    message(
      stringr::str_c(
        "In addition to standard Qualtrics variables, the following variables from your dataset were not included in the topline:\n",
        unrun_vars %>% stringr::str_flatten(', ')
      )
    )
  }



  # Get Topline -------------------------------------------------------------

  if (
    grouped_data == TRUE
  ) {
    topline_results <-
      purrr::map(
        group_variable_labels,
        ~combine_grouped_toplines(
          dataset_g = dataset,
          single_vars_g = single_vars,
          multi_vars_g = multi_vars,
          num_vars_g = num_vars,
          weight_var_g = {{ weight_var }},
          unweighted_ns_g = unweighted_ns,
          survey_order_g = survey_order,
          group_variables_g = group_variables,
          group_variable_labels_g = .x,
          silently
        )
      ) %>%
      purrr::reduce(
        dplyr::left_join,
        by = c(
          'variable',
          'prompt',
          'value',
          'label',
          'stat'
        )
      )

    if (silently == TRUE) {
      suppressMessages(
        multi_ns <- base_ns_multi_grouped(dataset, multi_vars, group_variables)
      )
      suppressMessages(
        ns_single <- base_ns_single_grouped(dataset, multi_vars, group_variables)
      )
    } else {
      multi_ns <- base_ns_multi_grouped(dataset, multi_vars, group_variables)
      ns_single <- base_ns_single_grouped(dataset, multi_vars, group_variables)
      }

    base_ns <- dplyr::bind_rows(ns_single, multi_ns)

  } else {
    topline_results <-
      make_topline(
        dataset_top = dataset,
        single_vars_top = single_vars,
        multi_vars_top = multi_vars,
        num_vars_top = num_vars,
        weight_var_top = {{ weight_var }},
        unweighted_ns_top = unweighted_ns,
        survey_order_top = survey_order,
        silently
      )

    multi_ns <- base_ns_multi(dataset, multi_vars)
    ns_single <- base_ns_single(dataset, multi_vars)
    base_ns <- dplyr::bind_rows(ns_single, multi_ns) %>%
      dplyr::rename(base_ns = 'n')
  }

  topline_results <- topline_results %>%
    dplyr::left_join(base_ns, by = 'variable')
  topline_results
}


#' @rdname topline_freqs
#' @export
jarvis_top_us_all_off <- topline_freqs

# Private functions -------------------------------------------------------
# Single Freqs ------------------------------------------------------------

get_singles <-
  function(
    df,
    weight.var,
    unweighted.ns,
    single.vars
  ) {
    if (
      length(single.vars) > 0
    ) {
      single_select_freqs <-
        df %>%
        dplyr::select(
          tidyselect::all_of(single.vars),
          {{ weight.var }}
        ) %>%
        y2clerk::freqs(
          unweighted_ns = unweighted.ns,
          wt = {{ weight.var }},
          prompt = TRUE,
          nas = FALSE
        )
    } else {
      single_select_freqs <-
        tibble::tibble()
    }

    single_select_freqs
  }



# Multi Freqs -------------------------------------------------------------


get_multis <-
  function(
    df,
    weight.var,
    unweighted.ns,
    multi.vars,
    silently
  ) {
    if (
      length(multi.vars) > 0
    ) {
      if (silently == FALSE) {
      multi_select_freqs <-
        df %>%
        dplyr::select(
          tidyselect::all_of(multi.vars),
          {{ weight.var }}
        ) %>%
        y2clerk::multi_freqs(
          unweighted_ns = unweighted.ns,
          wt = {{ weight.var }},
          prompt = TRUE
        )
      } else {
        suppressMessages(
        multi_select_freqs <-
          df %>%
          dplyr::select(
            tidyselect::all_of(multi.vars),
            {{ weight.var }}
          ) %>%
          y2clerk::multi_freqs(
            unweighted_ns = unweighted.ns,
            wt = {{ weight.var }},
            prompt = TRUE
          )
        )
      }
    } else {
      multi_select_freqs <-
        tibble::tibble()
    }
  }



# Numeric Freqs -----------------------------------------------------------


get_nums <-
  function(
    df,
    weight.var,
    unweighted.ns,
    num.vars
  ){
    if (
      length(num.vars) > 0
    ) {
      labels_list <-
        df %>%
        dplyr::select(
          tidyselect::all_of(num.vars)
        )

      labels <-
        tibble::tibble(
          prompt = labelled::var_label(labels_list) %>%
            as.character(),
          label = .data$prompt,
          variable = labels_list %>% names
        )  %>%
        dplyr::mutate(
          prompt = stringr::str_remove(.data$label, ' - .+') %>%
            stringr::str_trim(),
          label = stringr::str_remove(.data$label, '.*\n') %>%
            stringr::str_remove('.*- ') %>%
            stringr::str_trim()
        )

      numeric_freqs <-
        df %>%
        dplyr::select(
          tidyselect::all_of(num.vars),
          {{ weight.var }}
        ) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::everything(),
            .fns = ~forcats::as_factor(.x) %>%
              as.character() %>%
              as.numeric()
          )
        ) %>%
        y2clerk::freqs(
          stat = 'mean',
          wt = {{ weight.var }},
          nas = FALSE,
          unweighted_ns = unweighted.ns
        ) %>%
        dplyr::select(
          -'label'
        ) %>%
        dplyr::left_join(
          labels,
          by = 'variable'
        ) %>%
        dplyr::relocate(
          'label',
          .after = 'value'
        )
    } else {
      numeric_freqs <-
        tibble::tibble()
    }
  }


# Topline -----------------------------------------------------------------

make_topline <- function(
    dataset_top,
    single_vars_top,
    multi_vars_top,
    num_vars_top,
    weight_var_top,
    unweighted_ns_top,
    survey_order_top,
    silently
) {
  single_select_freqs <-
    get_singles(
      df = dataset_top,
      single.vars = single_vars_top,
      weight.var = {{ weight_var_top }},
      unweighted.ns = unweighted_ns_top
    )

  multi_select_freqs <-
    get_multis(
      df = dataset_top,
      multi.vars = multi_vars_top,
      weight.var = {{ weight_var_top }},
      unweighted.ns = unweighted_ns_top,
      silently
    )

  numeric_freqs <-
    get_nums(
      df = dataset_top,
      num.vars = num_vars_top,
      weight.var = {{ weight_var_top }},
      unweighted.ns = unweighted_ns_top
    )

  dplyr::bind_rows(
    multi_select_freqs,
    single_select_freqs,
    numeric_freqs
  ) %>%
    dplyr::mutate(
      variable = factor(
        .data$variable,
        survey_order_top
      )
    ) %>%
    dplyr::arrange(
      .data$variable,
      .data$value
    )
}




# Group_freqs -------------------------------------------------------------

combine_grouped_toplines <- function(
    dataset_g,
    single_vars_g,
    multi_vars_g,
    num_vars_g,
    weight_var_g,
    unweighted_ns_g,
    survey_order_g,
    group_variables_g,
    group_variable_labels_g,
    silently
) {
  dataset_g %>%
    dplyr::mutate(
      group_var_chr = .data[[group_variables_g]] %>% haven::as_factor() %>% as.character()
    ) %>%
    dplyr::filter(
      .data$group_var_chr == group_variable_labels_g
    ) %>%
    make_topline(
      single_vars_top = single_vars_g,
      multi_vars_top = multi_vars_g,
      num_vars_top = num_vars_g,
      weight_var_top = {{ weight_var_g }},
      unweighted_ns_top = unweighted_ns_g,
      survey_order_top = survey_order_g,
      silently
    ) %>%
    dplyr::rename_with(
      .cols = c('n', 'result'),
      ~stringr::str_c(
        .,
        group_variable_labels_g,
        sep = ' '
      )
    ) %>%
    dplyr::select(
      'variable',
      'prompt',
      'value',
      'label',
      'stat',
      tidyselect::everything()
    )
}


# base_ns (ungrouped) ----------------------------------------------------------

base_ns_multi <- function(
    dataset,
    multi_vars
) {
  datalist <- list()

  pattern_full <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(multi_vars)) %>%
    names() %>%
    stringr::str_remove(
      '_[0-9]+$'
    ) %>%
    stringr::str_remove(
      '_[0-9]+_TEXT$'
    )
  pattern <- pattern_full %>%
    unique()

  # Creating a filtered frequencies dataframe for each stem
  for (i in pattern) {
    data <- dataset %>%
      dplyr::select(
        dplyr::starts_with(stringr::str_c(i, '_')),
      ) %>%
      # Following lines filter out rows where none of the questions have been answered
      dplyr::mutate(ns = rowSums(
        dplyr::across(
          .cols = dplyr::starts_with(i),
          .fns = ~ifelse(
            is.na(.x),
            FALSE,
            TRUE
          )
        )
      )) %>%
      dplyr::filter(
        ns > 0
      ) %>%
      dplyr::count() %>%
      dplyr::mutate(value = i)

    # Adds stem freqs to datalist
    datalist[[i]] <- data
  }

  ns <- dplyr::bind_rows(datalist)
  if (nrow(ns) == 0) {
    ns <- multi_vars %>%
      tibble::as_tibble() %>%
      dplyr::mutate(n = .data$value)
  }
  var_names_multi <- multi_vars %>%
    tibble::as_tibble() %>%
    dplyr::rename(variable = 'value') %>%
    dplyr::bind_cols(tibble::as_tibble(pattern_full))
  ns_multi <- dplyr::full_join(
    var_names_multi,
    ns,
    by = 'value'
    ) %>%
    dplyr::select(-'value') %>%
    dplyr::mutate(n = as.numeric(.data$n))
}


base_ns_single <- function(
    dataset,
    multi_vars
    ) {
  var_names_singles <- dataset %>%
    dplyr::select(-tidyselect::all_of(multi_vars)) %>%
    names
  datalist <- list()
  for(i in var_names_singles) {
    data <- dataset %>%
      dplyr::filter(!is.na(i)) %>%
      dplyr::count() %>%
      dplyr::mutate(variable = i)
    datalist[[i]] <- data
  }
  ns_single <- dplyr::bind_rows(datalist)
}



# base_ns (grouped) ----------------------------------------------------------

base_ns_multi_grouped <- function(
    dataset,
    multi_vars,
    group_variables
) {
  datalist <- list()

  pattern_full <- dataset %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(multi_vars)) %>%
    names() %>%
    stringr::str_remove(
      '_[0-9]+$'
    ) %>%
    stringr::str_remove(
      '_[0-9]+_TEXT$'
    )
  pattern <- pattern_full %>%
    unique()
  group_variable_labels <- dataset %>%
    dplyr::pull(.data[[group_variables]]) %>%
    unique()

  # Creating a filtered frequencies dataframe for each stem
  for (i in pattern) {
    data <- dataset %>%
      dplyr::group_by(.data[[group_variables]]) %>%
      dplyr::select(
        dplyr::starts_with(stringr::str_c(i, '_')),
      ) %>%
      # Following lines filter out rows where none of the questions have been answered
      dplyr::mutate(ns = rowSums(
        dplyr::across(
          .cols = dplyr::starts_with(i),
          .fns = ~ifelse(
            is.na(.x),
            FALSE,
            TRUE
          )
        )
      )) %>%
      dplyr::filter(
        ns > 0
      ) %>%
      dplyr::count() %>%
      dplyr::mutate(
        value = i,
        x = stringr::str_c('base_ns ', .data[[group_variables]])
      ) %>%
      tidyr::pivot_wider(
        names_from = 'x',
        values_from = 'n',
        id_cols = -tidyr::all_of(group_variables)
      )
    # Adds stem freqs to datalist
    datalist[[i]] <- data
  }

  ns <- dplyr::bind_rows(datalist)
  if (nrow(ns) == 0) {
    ns <- data.frame(
      matrix(
        nrow = 0,
        ncol = length(group_variable_labels) + 1
      )
    )
    colnames(ns) <- c(
      'value',
      stringr::str_c('base_ns ', group_variable_labels)
      )
    ns$value <- as.character(ns$value)
  }
  var_names_multi <- multi_vars %>%
    tibble::as_tibble() %>%
    dplyr::rename(variable = 'value') %>%
    dplyr::bind_cols(tibble::as_tibble(pattern_full))
  ns_multi <- dplyr::full_join(
    var_names_multi,
    ns,
    by = 'value'
  ) %>%
    dplyr::select(-'value') %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::starts_with('base_ns'),
        .fns = ~as.numeric(.x)
      )
    )
}


base_ns_single_grouped <- function(
    dataset,
    multi_vars,
    group_variables
) {
  var_names_singles <- dataset %>%
    dplyr::select(
      -tidyselect::all_of(multi_vars),
      -tidyselect::all_of(group_variables)
      ) %>%
    names
  datalist <- list()
  for(i in var_names_singles) {
    data <- dataset %>%
      dplyr::group_by(.data[[group_variables]]) %>%
      dplyr::filter(!is.na(i)) %>%
      dplyr::count() %>%
      dplyr::mutate(
        variable = i,
        x = stringr::str_c('base_ns ', .data[[group_variables]])
      ) %>%
      tidyr::pivot_wider(
        names_from = 'x',
        values_from = 'n',
        id_cols = -tidyr::all_of(group_variables)
      )
    datalist[[i]] <- data
  }
  ns_single <- dplyr::bind_rows(datalist)
}

