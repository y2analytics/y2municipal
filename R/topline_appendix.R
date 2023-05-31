# Public function ---------------------------------------------------------
### topline_appendix

#' Create open ended data for a topline
#'
#' Use topline_appendix() to automate all the qualitative responses for a topline report. This function works best if your questions have the proper prefixes and suffixes:
#'  1. prefix "oe_" for open ends
#'  2. suffix "_TEXT" for open ends
#'
#' @keywords freqs topline
#' @param dataset A dataframe for which you want to create a topline
#' @param assign_oe DEFAULT = NULL, A vector of unquoted variables to be treated as open-ended variables, put within c()
#' @export
#' @return A tibble of open-ended responses
#' @examples
#' municipal_data %>%
#' topline_appendix()
#'
#' municipal_data %>%
#'   topline_appendix(
#'     assign_oe = c(RecipientEmail, UserLanguage)
#'   )

topline_appendix <- function(
    dataset,
    assign_oe = NULL
) {

  dataset %>%
    dplyr::select(
      tidyselect::starts_with('oe_'),
      tidyselect::ends_with('_TEXT'),
      {{ assign_oe }}
    ) %>%
    y2clerk::verbatims_y2()

}


#' @rdname topline_appendix
#' @export
jarvis_listen_to_guests <- topline_appendix
