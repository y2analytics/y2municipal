# Public function ---------------------------------------------------------
### read_data_names_fonts

#' prep data, names table, and fonts
#'
#' 3 in 1 function for reading in data, creating a data frame of variable names/labels, and reading in custom fonts
#'
#' Run this function at the start of your code to get your basic data/names/fonts ready
#'
#' @keywords read names font
#' @param file_name The file name of your data. (e.g. 'my weighted data.rds')
#' Using Y2 Analytics coding standards, you should have a DATA_PATH string in your environment specifying the path to your data file
#' @param fonts3 A vector of 2-3 strings:
#' 1) Font name (e.g., 'flama')
#' 2) File name of regular style font (e.g., 'Flama-Medium.otf')
#' 3) File name of bold style font (e.g., 'Flama-Bold.otf')
#' It is also possible to specify only 2 strings. If you give only 2, the bold (what would be 3) will just be the regular (2).
#' DEFAULT is flama: c('flama', 'Flama-Medium.otf', 'Flama-Bold.otf')
#' @return Two dataframes:
#' 1) responses (from your data file),
#' 2) names (with the variable names and labels of your data)
#' Additionally, loads your specified font into your R environment (flama by default)
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' DATA_PATH <- '~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/R&D/Training/orderlabel/data/'
#' read_data_names_fonts('training data.rds')
#'}


read_data_names_fonts <- function(
  file_name,
  fonts3 = c('flama', 'Flama-Medium.otf', 'Flama-Bold.otf')
  ) {
  # prep_data
  responses <- prep_data(file_name)

  # prep_names
  names <- orderlabel::taking_names(responses)

  # prep_fonts
  prep_fonts(fonts3)

  # Write out to environment
  data_names <- list(responses, names)
  names(data_names) <- c('responses', 'names')
  invisible(list2env(data_names, envir = .GlobalEnv))
}


# Private functions -------------------------------------------------------


### prep_data
prep_data <- function(file_name) {
  if (stringr::str_detect(file_name, '.csv')) {
    responses <- readr::read_csv(stringr::str_c(DATA_PATH, file_name))
  } else if (stringr::str_detect(file_name, '.rds')) {
    responses <- readr::read_rds(stringr::str_c(DATA_PATH, file_name))
  } else if (stringr::str_detect(file_name, '.sav')) {
    responses <- haven::read_sav(stringr::str_c(DATA_PATH, file_name))
  } else if (stringr::str_detect(file_name, '.spss')) {
    responses <- haven::read_sav(stringr::str_c(DATA_PATH, file_name))
  } else { # stringr::str_detect(data_file, '.xlsx')
    responses <- readxl::read_xlsx(stringr::str_c(DATA_PATH, file_name))
  }
}


### prep_names
# done in main function


### prep_fonts
prep_fonts <- function(fonts3) {
  if (length(fonts3) !=2 & length(fonts3) != 3) {
    stop("fonts3 must be a length of 2 or 3")
  } else if (length(fonts3 == 2)) {
    sysfonts::font_add(
      family = fonts3[1],
      regular = fonts3[2],
      bold = fonts3[2]
    )
    showtext::showtext_auto()
  } else { # length(fonts3 == 3)
    sysfonts::font_add(
      family = fonts3[1],
      regular = fonts3[2],
      bold = fonts3[3]
    )
    showtext::showtext_auto()
   }
}

