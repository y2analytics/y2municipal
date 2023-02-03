# Public function ---------------------------------------------------------
### topline_appendix

#' Create open ended data for a topline
#'
#' Use topline_appendix() to automate all the qualitative reponses for a topline report. This function works best if your questions have the proper prefixes:
#'  1. "s_" for single select,
#'  2. "m_" for multiple select,
#'  3. "oe_" for open ends,
#'  4. "n_" for numeric,
#'  5. "r_" for ranked,
#'  6. "md_" for max diff.
#'
#' @keywords freqs topline
#' @param dataset A dataframe for which you want to create a topline
#' @param assign_oe DEFAULT = NULL, A vector of unquoted variables to be treated as open-ended variables, put within c()
#' @param qsf file path to a qsf
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
    assign_oe = NULL,
    qsf
) {

  dataset %>%
    dplyr::select(
      tidyselect::starts_with('oe_'),
      tidyselect::ends_with('_TEXT'),
      {{ assign_oe }}
    ) %>%
    y2clerk::verbatims_y2()

}
