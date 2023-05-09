
# Overall Tests ----------------------------------------------------------------

test_that("errors", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = 2,
    m_var_3 = 3
  ) %>%
    dplyr::group_by(s_var, m_var_1)

  expect_error(
    topline_freqs(dataset, silently = TRUE),
    'Multiple grouping vars detected, only one grouping variable permitted. Detected grouping vars: s_var, m_var_1'
  )
})


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
    q1 = 1,
    weights = 2.1
  )
  labelled::val_label(dataset$m_var_2, 1) <- 'empty'
  labelled::val_label(dataset$md_var_2, 0) <- 'empty'

  vars_freqd <- topline_freqs(dataset, silently = TRUE) %>%
    dplyr::select(variable) %>%
    purrr::as_vector() %>%
    as.character()

  expect_equal(
    vars_freqd,
    c('s_var', 'm_var_1', 'm_var_2', 'm_var_3', 'n_var', 'r_var_1', 'r_var_2',
      'r_var_3', 'md_var_1', 'md_var_2', 'md_var_3', 'M_RACE_1', 'M_RACE_2')
  )
})


test_that("missing question types", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = NA_real_,
    m_var_3 = 1,
    n_var = 875
  )
  labelled::val_label(dataset$m_var_2, 1) <- 'empty'

  expect_error(
    dataset %>%
      dplyr::select(-s_var) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
  expect_error(
    dataset %>%
      dplyr::select(-m_var_1:-m_var_3) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
  expect_error(
    dataset %>%
      dplyr::select(-n_var) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
})


test_that("output formatting", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = NA_real_,
    m_var_3 = 1,
    n_var = 875
  )
  labelled::val_label(dataset$m_var_2, 1) <- 'empty'

  frequencies <- dataset %>% topline_freqs(silently = TRUE)

  expect_equal(
    class(frequencies),
    c('tbl_df', 'tbl', 'data.frame')
    )
  expect_equal(
    names(frequencies),
    c('variable', 'prompt', 'value', 'label', 'n', 'stat', 'result', 'base_ns')
  )
})


test_that("base_ns calculations", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(1, NA_real_),
    n_var = c(0, 50),
    weights = c(1, 2)
  )
  frequencies <- dataset %>% topline_freqs(silently = TRUE)
  ns_s_var <- frequencies %>%
    dplyr::filter(variable == 's_var') %>%
    dplyr::pull(base_ns)
  ns_m_var <- frequencies %>%
    dplyr::filter(variable == 'm_var_1') %>%
    dplyr::pull(base_ns)
  ns_n_var <- frequencies %>%
    dplyr::filter(variable == 'n_var') %>%
    dplyr::pull(base_ns)

  expect_equal(ns_s_var, c(2, 2))
  expect_equal(ns_m_var, c(1))
  expect_equal(ns_n_var, c(2))
})


test_that("grouped topline", {
  dataset <- tibble::tibble(
    s_var = c(1, 2, 3, 2),
    m_var_1 = c(1, NA_real_, 1, 1),
    n_var = c(0, 50, 100, 100),
    groups = c('group 1', 'group 2', 'group 3', 'group 2')
  )
  labelled::val_label(dataset$m_var_1, 1) <- 'some1'
  dataset <- dataset %>% dplyr::group_by(groups)
  frequencies <- dataset %>% topline_freqs(silently = TRUE)

  expect_equal(
    names(frequencies),
    c('variable', 'prompt', 'value', 'label', 'stat', 'n group 1',
      'result group 1', 'n group 2', 'result group 2', 'n group 3',
      'result group 3', 'base_ns group 1', 'base_ns group 2', 'base_ns group 3'
      )
  )

  base_ns_s_var3 <- frequencies %>%
    dplyr::filter(variable == 's_var') %>%
    dplyr::pull(`base_ns group 2`)
  base_ns_m_var3 <- frequencies %>%
    dplyr::filter(variable == 'm_var_1') %>%
    dplyr::pull(`base_ns group 3`)
  base_ns_m_var2 <- frequencies %>%
    dplyr::filter(variable == 'm_var_1') %>%
    dplyr::pull(`base_ns group 2`)
  ns_m_var2 <- frequencies %>%
    dplyr::filter(variable == 'm_var_1') %>%
    dplyr::pull(`n group 2`)
  result_m_var2 <- frequencies %>%
    dplyr::filter(variable == 'm_var_1') %>%
    dplyr::pull(`n group 2`)

  expect_equal(base_ns_s_var3, 2)
  expect_equal(base_ns_m_var3, 1)
  expect_equal(base_ns_m_var2, 1)
  expect_equal(ns_m_var2, 1)
  expect_equal(result_m_var2, 1)
})


test_that("grouped topline, missing question types", {
  dataset <- tibble::tibble(
    s_var = c(1, 2, 3, 2),
    m_var_1 = c(1, NA_real_, 1, 1),
    n_var = c(0, 50, 100, 100),
    groups = c('group 1', 'group 2', 'group 3', 'group 2')
  )
  labelled::val_label(dataset$m_var_1, 1) <- 'some1'
  dataset <- dataset %>% dplyr::group_by(groups)

  expect_error(
    dataset %>%
      dplyr::select(-s_var) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
  expect_error(
    dataset %>%
      dplyr::select(-m_var_1) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
  expect_error(
    dataset %>%
      dplyr::select(-m_var_1) %>%
      topline_freqs(),
    regexp = NA
  )
  expect_error(
    dataset %>%
      dplyr::select(-n_var) %>%
      topline_freqs(silently = TRUE),
    regexp = NA
  )
})


# Argument Tests (in order of documentation) -----------------------------------

test_that("weight_var argument", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(1, NA_real_),
    n_var = c(0, 50),
    weights = c(1, 2)
  )
  frequencies_unweighted <- topline_freqs(
    dataset,
    silently = TRUE
  )
  frequencies_weighted <- topline_freqs(
    dataset,
    silently = TRUE,
    weight_var = weights
  )

  expect_equal(frequencies_unweighted$result[1], .5)
  expect_equal(frequencies_unweighted$result[5], 25)

  expect_equal(frequencies_weighted$result[1], .33)
  expect_equal(frequencies_weighted$result[5], 33.33)

  expect_equal(
    stringr::str_detect(frequencies_weighted$variable, 'weights') %>% sum,
    0
  )
  expect_equal(
    stringr::str_detect(frequencies_unweighted$variable, 'weights') %>% sum,
    0
  )
})


test_that("assign_s argument", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(NA_real_, NA_real_),
    q1_1 = c(1, 1),
    q1_2 = c(1, 1)
  )
  frequencies <- topline_freqs(
    dataset,
    assign_s = 'q1_1',
    silently = TRUE
  )

  expect_equal(
    stringr::str_detect(frequencies$variable, 'q1_1') %>% sum(),
    1
  )
  expect_equal(
    stringr::str_detect(frequencies$variable, 'q1_2') %>% sum(),
    0
  )
})


test_that("assign_m argument", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(NA_real_, NA_real_),
    q1_1 = c(1, 1),
    q1_2 = c(1, 1)
  )
  frequencies <- topline_freqs(
    dataset,
    assign_m = c('q1_1', 'q1_2'),
    silently = TRUE
  )

  expect_equal(
    stringr::str_detect(frequencies$variable, 'q1_1') %>% sum(),
    1
  )
  expect_equal(
    stringr::str_detect(frequencies$variable, 'q1_2') %>% sum(),
    1
  )
})


test_that("assign_n argument", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(NA_real_, NA_real_),
    q1 = c(0, 50)
  )
  frequencies <- topline_freqs(
    dataset,
    assign_n = 'q1',
    silently = TRUE
  )

  expect_equal(
    frequencies$result[4],
    25.0
  )
  expect_equal(
    frequencies$stat[4],
    'mean'
  )
})


test_that("messages, silently argument", {
  dataset <- tibble::tibble(
    s_var = 1,
    m_var_1 = 1,
    m_var_2 = 2,
    m_var_3 = 3
  )
  dataset2 <- tibble::tibble(
    s_var = 1,
    q1_var = 2,
    m_var_1 = 1,
    m_var_2 = 1
  )

  expect_message(
    topline_freqs(dataset, silently = TRUE),
    NA
  )
  expect_message(
    topline_freqs(dataset),
    "Variable stem \"m_var\" successfully freq'd"
  )
  expect_message(
    topline_freqs(dataset2),
    "In addition to standard Qualtrics variables, the following variables from your dataset were not included in the topline:\nq1_var"
  )
})


test_that("unweighted_ns argument", {
  dataset <- tibble::tibble(
    s_var = c(1, 2),
    m_var_1 = c(1, NA_real_),
    m_var_2 = c(1, NA_real_),
    n_var = c(0, 50),
    weights = c(1, 2)
  )
  frequencies_unweighted <- topline_freqs(
    dataset,
    silently = TRUE
  )
  frequencies_weighted <- topline_freqs(
    dataset,
    silently = TRUE,
    weight_var = weights
  )
  frequencies_weighted_weightedns <- topline_freqs(
    dataset,
    silently = TRUE,
    weight_var = weights,
    unweighted_ns = FALSE
  )
  expect_equal(frequencies_unweighted$n[1], 1)
  expect_equal(frequencies_unweighted$n[5], 2)

  expect_equal(frequencies_weighted$n[2], 1)
  expect_equal(frequencies_weighted$n[5], 2)

  expect_equal(frequencies_weighted_weightedns$n[2], 2)
  expect_equal(frequencies_weighted_weightedns$n[5], 3)
})

