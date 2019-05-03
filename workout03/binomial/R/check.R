### Private checker functions ###

# test if PROB is a valid probability value
check_prob <- function(prob) {
  if (prob < 0 || prob > 1) {
    stop("invalid probability value: must be between 0 and 1")
  }
  return(TRUE)
}

# test if TRIALS is a valid value for number of trials
check_trials <- function(trials) {
  if (trials < 0 || as.integer(trials) != trials) {
    stop("invalid number of trials: must be non-negative integer")
  }
  return(TRUE)
}

# test if SUCCESS is a valid value for number of successes
check_success <- function(success, trials) {
  # single & operator accounts for case where success has multiple elements
  # values of first two conditions are recycled
  indicator <- is.vector(success) & success == as.integer(success) &
               success >= 0 & success <= trials
  # check for any false values
  if (!all(indicator)) {
    stop("invalid success value: must be a vector of integer
         values between 0 and number of trials")
  }
  return(TRUE)
}
