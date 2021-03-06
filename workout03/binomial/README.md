
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

`binomial` is an R package that provides functions for calculating and plotting with binomial random variables.

-   `bin_choose()` calculates the number of ways to get k successes out of n trials.
-   `bin_probability()` calculates the probability of getting a specified number of successes given the number of trials and probability of success.
-   `bin_distribution()` computes the probability distribution given the number of trials and probability of success. Creates a `data.frame` object with additional class `bindis`.
-   `bin_cumulative()` is similar to `bin_distribution()`, but also computes cumulative probabilities. Creates a `data.frame` object with additional class `bincum`.
-   `bin_variable()` creates a binomial random variable object (class `binvar`) given the number of trials and probability of success.
-   `plot()` methods for `bindis` and `bincum` objects
-   `summary()` method for `binvar` objects
-   `bin_mean()`, `bin_variance()`, `bin_mode()`, `bin_skewness()`, `bin_kurtosis()` compute their respective statistics.

Motivation
----------

This package was developed as a means to gain practice with creating R packages, writing public/private functions and methods, testing, and documentation.

Usage
-----

``` r
library(binomial)

# calculate number of ways to have 2 successes out of 5 trials
bin_choose(n = 5, k = 2)
#> [1] 10

# 1, 2, or 3 successes out of 5 trials
bin_choose(5, 1:3)
#> [1]  5 10 10

# probability of getting 2 successes in 5 trials, prob of success is 0.5
bin_probability(success = 2, trials = 5, prob = 0.5)
#> [1] 0.3125

# with 0, 1, and 2 successes
bin_probability(success = 0:2, trials = 5, prob = 0.5)
#> [1] 0.03125 0.15625 0.31250

# compute probability distribution
dis1 <- bin_distribution(trials = 5, prob = 0.5)
dis1
#>   success probability
#> 1       0     0.03125
#> 2       1     0.15625
#> 3       2     0.31250
#> 4       3     0.31250
#> 5       4     0.15625
#> 6       5     0.03125

# can use as a regular dataframe or as an object of class "bindis"
class(dis1)
#> [1] "bindis"     "data.frame"

# plot bindis objects
plot(dis1)

# compute cumulative probabilities
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
dis2
#>   success probability cumulative
#> 1       0     0.03125    0.03125
#> 2       1     0.15625    0.18750
#> 3       2     0.31250    0.50000
#> 4       3     0.31250    0.81250
#> 5       4     0.15625    0.96875
#> 6       5     0.03125    1.00000

class(dis2)
#> [1] "bincum"     "data.frame"

plot(dis2)

# create a binomial random variable
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1
#> [1] "Binomial variable"
#> [1] 
#> [1] Parameters
#> [1] - number of trials: 10
#> [1] - prob of success : 0.3

class(bin1)
#> [1] "binvar"

summary(bin1)
#> [1] "Summary Binomial"
#> [1] 
#> [1] Parameters
#> [1] - number of trials: 10
#> [1] - prob of success : 0.3
#> [1] 
#> [1] Measures
#> [1] - mean    : 3
#> [1] - variance: 2.1
#> [1] - mode    : 3
#> [1] - skewness: 0.276026223736942
#> [1] - kurtosis: -0.123809523809524

# miscellaneous stats
bin_mean(trials = 10, prob = 0.3)
#> [1] 3

bin_variance(trials = 10, prob = 0.3)
#> [1] 2.1

bin_mode(trials = 10, prob = 0.3)
#> [1] 3

bin_skewness(trials = 10, prob = 0.3)
#> [1] 0.2760262

bin_kurtosis(trials = 10, prob = 0.3)
#> [1] -0.1238095
```
