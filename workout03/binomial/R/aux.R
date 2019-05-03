### Private auxiliary functions ###

# Computes expected value (mean) of the binomial distribution
# given number of trials (n) and probability of success (p)
aux_mean <- function(trials, prob) {
  return(trials * prob)  # n*p
}

# Computes variance of the distribution given number of trials
# and probability of success
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob)) # np(1-p)
}

# Computes the most likely number of successes for given
# number of independent trials and probability of success
aux_mode <- function(trials, prob) {
  if (prob == 1) {
    return(trials)
  }
  return(floor(trials * prob + prob))  # m = int(np + p)
}

# Computes skewness of the distribution
aux_skewness <- function(trials, prob) {
  # division by zero (prob == 0 or prob == 1) returns Inf
  return((1 - 2*prob) / sqrt(trials * prob * (1 - prob)))
}

# Computes kurtosis of the distribution
aux_kurtosis <- function(trials, prob) {
  # division by zero (prob == 0 or prob == 1) returns Inf
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}
