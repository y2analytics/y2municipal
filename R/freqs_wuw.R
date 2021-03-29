# Public function ---------------------------------------------------------
### freqs_wuw

#' Combine weighted freqs with unweighted ns
#'
#' Use freqs_wuw() when creating a frequencies table with weighted results but unweighted ns. Saves you the time of combining the two freqs tables if run separately
#'
#' @keywords freqs
#' @param dataset A dataframe.
#' @param ... The unquoted names of a set of variables in the dataset. If nothing
#' is specified, the function runs a frequency on every column in given dataset.
#' @param stat Character, stat to run. Currently accepts 'percent,' 'mean,' 'median,' 'min,' 'max,' 'quantile,' and 'summary' (default: 'percent').
#' @param pr Double, for use when stat = 'quantile.' Input should be a real number x such that 0 <= x <= 100. Stands for percentile rank, which is a quantile relative to a 100-point scale. (default:NULL)
#' @param nas Boolean, whether or not to include NAs in the tabulation (default: TRUE).
#' @param wt The unquoted name of a weighting variable in the dataset (default: NULL).
#' @param prompt Boolean, whether or not to include the prompt in the dataset (default: FALSE).
#' @param digits Integer, number of significant digits for rounding (default: 2).
#' @param nas_group Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).
#' @param factor_group Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)
#' @return A dataframe with the variable names, prompts, values, labels, counts,
#' stats, and resulting calculations.
#' @export
#' @examples
#' mtcars %>% y2clerk::freqs(gear)
#' mtcars %>% y2clerk::freqs(gear, wt = mpg)
#' mtcars %>% freqs_wuw(gear, wt = mpg)



freqs_wuw  <- function(
  dataset,
  ...,
  stat = c("percent", "mean", "median", "min", "max", "quantile", "summary"),
  pr = NULL,
  nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE
) {

  # run weighted freqs
  freqs_weighted <-
    dataset %>%
    y2clerk::freqs(
      ...,
      stat = stat,
      pr = pr,
      nas = nas,
      wt = {{ wt }},
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group
    ) %>%
    dplyr::select(-.data$n)

  # run unweighted freqs, but only keep n
  freqs_unweighted <-
    dataset %>%
    dplyr::select(-{{ wt }}) %>%
    y2clerk::freqs(
      ...,
      stat = stat,
      pr = pr,
      nas = nas,
      prompt = prompt,
      digits = digits,
      nas_group = nas_group,
      factor_group = factor_group
    ) %>%
    dplyr::select(.data$n)

  # bind freqs together
  frequencies <- dplyr::bind_cols(
    freqs_weighted,
    freqs_unweighted
  ) %>%
    dplyr::relocate(
      .data$n,
      .after = "label"
    )
  return(frequencies)
}

