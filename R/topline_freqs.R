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
#' @param assign_s DEFAULT = NULL, A list of unquoted variables to be treated as single select variables
#' @param assign_m DEFAULT = NULL, A list of unquoted variables to be treated as multiple select variables
#' @param assign_n DEFAULT = NULL, A list of unquoted variables to be treated as numeric variables
#' @param unweighted_ns DEFAULT = TRUE, Display weighted or unweighted n-sizes in topline report
#' @export
#' @return A tibble of frequencies
#' @examples
#' municipal_data %>%
#' topline_freqs()
#'
#' municipal_data %>%
#' topline_freqs(
#' weight_var = weights
#' )


topline_freqs <- function(
    dataset,
    weight_var,
    assign_s = NULL,
    assign_m = NULL,
    assign_n = NULL,
    unweighted_ns = TRUE
) {


  # Check for grouping ------------------------------------------------------

  if(
    dplyr::is_grouped_df(
      dataset
    )
  ){

    group_variables <-
      dataset %>%
      dplyr::group_vars()


    if(group_variables %>% length > 1){

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


  if (length(unrun_vars) >= 1) {
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
  ){

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
          group_variable_labels_g = .x
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

  } else {

    topline_results <-
      make_topline(
        dataset_top = dataset,
        single_vars_top = single_vars,
        multi_vars_top = multi_vars,
        num_vars_top = num_vars,
        weight_var_top = {{ weight_var }},
        unweighted_ns_top = unweighted_ns,
        survey_order_top = survey_order
      )

  }

  topline_results

}




# Private functions -------------------------------------------------------


# Single Freqs ------------------------------------------------------------

get_singles <-
  function(
    df,
    weight.var,
    unweighted.ns,
    single.vars
  ){


    if(
      length(single.vars) > 0
    ){

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
    multi.vars
  ){

    if(
      length(multi.vars) > 0
    ){

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


    if(
      length(num.vars) > 0
    ){

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
          -.data$label
        ) %>%
        dplyr::left_join(
          labels,
          by = 'variable'
        ) %>%
        dplyr::relocate(
          .data$label,
          .after = .data$value
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
    survey_order_top
){

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
      unweighted.ns = unweighted_ns_top
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
    group_variable_labels_g
){

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
      survey_order_top = survey_order_g
    ) %>%
    dplyr::rename_with(
      .cols = c(.data$n, .data$result),
      ~stringr::str_c(
        .,
        group_variable_labels_g,
        sep = ' '
      )
    ) %>%
    dplyr::select(
      .data$variable,
      .data$prompt,
      .data$value,
      .data$label,
      .data$ stat,
      tidyselect::everything()
    )

}
