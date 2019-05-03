context("Test main functions")

test_that("bin_choose", {
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(5, 2), 10)
  expect_length(bin_choose(5, 2), 1)

  expect_equal(bin_choose(5, 0), 1)

  expect_equal(bin_choose(5, 1:3), c(bin_choose(5, 1),
                                     bin_choose(5, 2),
                                     bin_choose(5, 3)))
  expect_length(bin_choose(5, 1:3), 3)

  expect_error(bin_choose(5, 10))
  expect_error(bin_choose(5, 4:6))
})

test_that("bin_probability", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_length(bin_probability(success = 2, trials = 5, prob = 0.5), 1)

  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5),
               c(0.03125, 0.15625, 0.31250))
  expect_length(bin_probability(success = 0:2, trials = 5, prob = 0.5), 3)

  expect_equal(bin_probability(success = 55, trials = 100, prob = 0.45),
               bin_choose(100, 55) * 0.45^55 * (1-0.45)^(100-55))
})

test_that("bin_distribution", {
  expect_equal(class(bin_distribution(trials = 5, prob = 0.5)),
               c("bindis", "data.frame"))
  expect_equal(bin_distribution(trials = 5, prob = 0.5)$success, 0:5)
  expect_equal(bin_distribution(trials = 5, prob = 0.5)$probability,
               c(0.03125,0.15625,0.31250,0.31250,0.15625,0.03125))
})

test_that("bin_cumulative", {
  expect_equal(class(bin_cumulative(trials = 5, prob = 0.5)), c("bincum", "data.frame"))

  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$success, 0:5)

  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$probability,
               c(0.03125,0.15625,0.31250,0.31250,0.15625,0.03125))

  expect_equal(bin_cumulative(trials = 5, prob = 0.5)$cumulative,
               c(0.03125,0.18750,0.50000,0.81250,0.96875,1.00000))
})
