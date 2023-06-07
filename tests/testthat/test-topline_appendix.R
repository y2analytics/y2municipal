
# Error/Warning Tests -----------------------------------------------------

test_that("Error - no variables", {
  dataset <- tibble::tibble(
    m_var_3_TEST = 'Other text',
    q_var = 'a lot of text'
  )

  expect_error(
    topline_appendix(dataset %>% dplyr::select(-dplyr::everything())),
    'You currently have no variables specified OR no variables starting with "oe_" or ending with "_TEXT." Please either list out the variables you wish to include or check if your variables have the correct prefixes/suffixes.'
    )
  expect_error(
    topline_appendix(dataset),
    'You currently have no variables specified OR no variables starting with "oe_" or ending with "_TEXT." Please either list out the variables you wish to include or check if your variables have the correct prefixes/suffixes.'
  )
})

test_that("Warnings", {
  dataset <- tibble::tibble(
    m_var_3_TEXT = 'Other text',
    oe_var = 'a lot of text'
  )

  expect_warning(
    topline_appendix(dataset),
    'You are working with variables that have no labeling. You may want to consider adding a prompt before continuing'
  )
})



# Overall Tests -----------------------------------------------------------

test_that("Pulls right variables", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = NA_real_,
    m_var_3 = 1,
    m_var_3_TEXT = 'Other text',
    oe_var = 'a lot of text',
    n_var = 875,
    r_var_1 = 1,
    r_var_2 = 2,
    r_var_3 = 3,
    md_var_1 = 1,
    md_var_2 = NA_real_,
    md_var_3 = 1,
    M_RACE_1 = 'white',
    M_RACE_2 = 'black',
    q1 = 'a not correctly named question',
    weights = 2.1
  )
  labelled::var_label(dataset$m_var_3_TEXT) <- 'Specify the thing'
  labelled::var_label(dataset$oe_var) <- 'Write some stuff'

  vars_freqd <- topline_appendix(dataset) %>%
    dplyr::select(variable) %>%
    purrr::as_vector() %>%
    as.character()

  expect_equal(
    vars_freqd,
    c('oe_var', 'm_var_3_TEXT')
  )
})


test_that("correct output formatting", {
  dataset <- tibble::tibble(
    m_var_3_TEXT = 'Other text',
    oe_var = 'a lot of text',
    oe_2 = 'a not correctly named question'
  )
  labelled::var_label(dataset$m_var_3_TEXT) <- 'Specify the thing'
  labelled::var_label(dataset$oe_var) <- 'Write some stuff'
  labelled::var_label(dataset$oe_2) <- 'Write some stuff again'

  frequencies <- topline_appendix(dataset)

  expect_equal(
    class(frequencies),
    c('tbl_df', 'tbl', 'data.frame')
  )
  expect_equal(
    names(frequencies),
    c('variable', 'prompt', 'label', 'base_ns')
  )
  expect_equal(
    frequencies %>% dplyr::pull(label),
    c('a lot of text', 'a not correctly named question', 'Other text')
  )
})



# Test Arguments ----------------------------------------------------------

test_that("assign_oe argument", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_3_TEXT = 'Other text',
    oe_var = 'a lot of text',
    q1 = 'a not correctly named question',
    q2 = 'and another',
    weights = 2.1
  )
  labelled::var_label(dataset$m_var_3_TEXT) <- 'Specify the thing'
  labelled::var_label(dataset$oe_var) <- 'Write some stuff'
  labelled::var_label(dataset$q1) <- 'question prompt'
  labelled::var_label(dataset$q2) <- 'question prompt pt 2'

  vars_freqd <- topline_appendix(
    dataset,
    assign_oe = c(q1, q2)
    ) %>%
    dplyr::select(variable) %>%
    purrr::as_vector() %>%
    as.character()

  expect_equal(
    vars_freqd,
    c('oe_var', 'm_var_3_TEXT', 'q1', 'q2')
  )
})

