context("Test auxiliary functions")

test_that("test aux_mean", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_length(aux_mean(10, 0.3), 1)

  expect_equal(aux_mean(10, 0.5), 5)

  expect_equal(aux_mean(5, 0.2), 5*0.2)
  expect_equal(aux_mean(5, 0.7), 5*0.7)

  expect_equal(aux_mean(5, 0), 0)
  expect_equal(aux_mean(5, 1), 5)

  expect_equal(aux_mean(0, 0.4), 0)
})

test_that("test aux_variance", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_length(aux_variance(10, 0.3), 1)

  expect_equal(aux_variance(10, 0.5), 10*0.5*0.5)

  expect_equal(aux_variance(5, 0.2), 5*0.2*0.8)
  expect_equal(aux_variance(5, 0.7), 5*0.7*0.3)

  expect_equal(aux_variance(5, 0), 0)
  expect_equal(aux_variance(5, 1), 0)
  expect_equal(aux_variance(0, 0.4), 0)
})

test_that("test aux_mode", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_length(aux_mode(10, 0.3), 1)

  expect_equal(aux_mode(10, 0), 0)
  expect_equal(aux_mode(10, 1), 10)
})

test_that("test aux_skewness", {
  expect_equal(aux_skewness(10, 0.3), (1 - 2 * 0.3) / sqrt(10 * 0.3 * (1 - 0.3)))
  expect_length(aux_skewness(10, 0.3), 1)

  expect_true(is.infinite(aux_skewness(10, 0)))
  expect_true(is.infinite(aux_skewness(10, 1)))
})

test_that("test aux_kurtosis", {
  expect_equal(aux_kurtosis(10, 0.3), (1 - 6 * 0.3 * (1 - 0.3)) / (10 * 0.3 * (1 - 0.3)))
  expect_length(aux_kurtosis(10, 0.3), 1)

  expect_true(is.infinite(aux_kurtosis(10, 0)))
  expect_true(is.infinite(aux_kurtosis(10, 1)))
})
