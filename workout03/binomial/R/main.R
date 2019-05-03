### Main Functions ###

#' @title Choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return numeric vector with values for n choose k
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n, k) {
  if (any(k > n)) {
    stop("k cannot be greater than n")
  }
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

#' @title Probability
#' @description Computes the probability of getting specified number of
#' successes for given number of trials
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with probabilities
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success))
}

#' @title Distribution
#' @description Computes probability distribution
#' @param trials number of trials (single value)
#' @param prob probability of success
#' @return Data frame with successes from 0 to number of trials, and
#' corresponding probabilities. Has both classes "bindis" and "data.frame"
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  successes <- 0:trials
  probs <- bin_probability(successes, trials, prob)
  result <- data.frame(success = successes, probability = probs)
  class(result) <- c("bindis", "data.frame")
  return(result)
}

#' @export
plot.bindis <- function(x, ...) {
  # graph bar plot with probability distribution for a bindis object
  barplot(x$probability, names.arg = x$success,
          xlab = "successes", ylab = "probability")
}

#' @title Cumulative Distribution
#' @description Computes distribution with probabilities and cumulative probabilities
#' @param trials number of trials (single value)
#' @param prob probability of success
#' @return Data frame with successes from 0 to number of trials,
#' corresponding probabilities, and cumulative probabilities.
#' Has classes "bincum" and "data.frame"
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  result <- bin_distribution(trials, prob)
  result$cumulative <- cumsum(result$probability)
  class(result) <- c("bincum", "data.frame")
  return(result)
}

#' @export
plot.bincum <- function(x, ...) {
  # graph cumulative distribution for a bincum object
  plot(x$success, x$cumulative, type="b", xlab = "successes", ylab = "probability")
}

#' @title Create Variable
#' @description creates a binomial random variable object (class "binvar"),
#' essentially a list with named elements for trials and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @return binvar object containing trials and prob
#' @export
#' @examples
#' bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  result <- list(trials = trials, prob = prob)
  class(result) <- "binvar"
  return(result)
}

#' @export
print.binvar <- function(x, ...) {
  strings <- c('"Binomial variable"',
               "",
               "Parameters",
               paste("- number of trials:", x$trials),
               paste("- prob of success :", x$prob))
  for (s in strings) {
    print(noquote(s))
  }
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...) {
  result <- list(trials = x$trials,
                 prob = x$prob,
                 mean = aux_mean(x$trials, x$prob),
                 variance = aux_variance(x$trials, x$prob),
                 mode = aux_mode(x$trials, x$prob),
                 skewness = aux_skewness(x$trials, x$prob),
                 kurtosis = aux_kurtosis(x$trials, x$prob))
  class(result) <- "summary.binvar"
  return(result)
}

#' @export
print.summary.binvar <- function(x, ...) {
  strings <- c('"Summary Binomial"',
               "",
               "Parameters",
               paste("- number of trials:", x$trials),
               paste("- prob of success :", x$prob),
               "",
               "Measures",
               paste("- mean    :", x$mean),
               paste("- variance:", x$variance),
               paste("- mode    :", x$mode),
               paste("- skewness:", x$skewness),
               paste("- kurtosis:", x$kurtosis))
  for (s in strings) {
    print(noquote(s))
  }
  invisible(x)
}

#' @title Mean
#' @description Computes expected value (mean) of the binomial distribution
#' given number of trials and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with mean value
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Variance
#' @description Computes variance of the distribution given number of trials
#' and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with variance value
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Mode
#' @description Computes the most likely number of successes for given
#' number of independent trials and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with mode value
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Skewness
#' @description Computes skewness measure for the given distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with skewness value
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Kurtosis
#' @description Computes kurtosis measure for the given distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector with kurtosis value
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
