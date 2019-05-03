context("Test checker functions")

test_that("test check_prob", {
  expect_true(check_prob(0.5))
  expect_length(check_prob(0.5), 1)

  expect_true(check_prob(0.25))
  expect_true(check_prob(0.7))

  expect_true(check_prob(0))
  expect_true(check_prob(1))

  expect_error(check_prob(-0.3))
  expect_error(check_prob(2))
})

test_that("test check_trials", {
  expect_true(check_trials(5))
  expect_true(check_trials(5L))
  expect_length(check_trials(5), 1)

  expect_true(check_trials(1))
  expect_true(check_trials(50))
  expect_true(check_trials(83))
  expect_true(check_trials(124))
  expect_true(check_trials(208L))

  expect_error(check_trials(0.01))
  expect_error(check_trials(0.6))
  expect_error(check_trials(-1))
  expect_error(check_trials(-0.43))
})

test_that("test check_success", {
  k_vec <- c(2, 10, 35)

  expect_true(check_success(2, 10))
  expect_true(check_success(2L, 10L))
  expect_length(check_success(2, 10), 1)

  expect_true(check_success(k_vec, 100))
  expect_length(check_success(k_vec, 100), 1)

  expect_error(check_success(-3, 100))
  expect_error(check_success(1.5, 100))

  expect_error(check_success(matrix(1:6, nrow = 2, ncol = 3, 100)))
  expect_error(check_success(5, 2)) # trials too small
  expect_error(check_success(as.integer(k_vec), 2)) # also trials too small

})
