# The book's ready-made functions, under the heading of the chapter that builds them.
# _common.R loads every .R file in R/ before each chapter. Base R where possible, so
# readers can copy a function into their own work.


# PART 1: SAMPLING =========================================================


# Sampling Distributions ---------------------------------------------------

my_mean <- function(x) {
  total <- sum(x)
  n <- length(x)
  output <- total / n
  return(output)
}

my_var <- function(x) {
  n <- length(x)
  mean_x <- my_mean(x)
  squared_distances <- (x - mean_x)^2
  total_squared_distance <- sum(squared_distances)
  output <- total_squared_distance / (n - 1)
  return(output)
}

my_sd <- function(x) {
  output <- sqrt(my_var(x))
  return(output)
}


# Standard Error and t -----------------------------------------------------

my_se <- function(x) {
  n <- length(x)
  sd_x <- my_sd(x)
  output <- sd_x / sqrt(n)
  return(output)
}

my_t_value <- function(x, comparison_value) {
  mean_x <- my_mean(x)
  se_x <- my_se(x)
  output <- (mean_x - comparison_value) / se_x
  return(output)
}


# PART 2: TESTING ==========================================================


# Null Distributions -------------------------------------------------------

my_null_t <- function(n, pop_mean, pop_sd) {
  sample_values <- rnorm(n = n, mean = pop_mean, sd = pop_sd)
  output <- my_t_value(sample_values, comparison_value = pop_mean)
  return(output)
}

my_cutoffs <- function(null_ts, alpha = 0.05) {
  sorted <- sort(null_ts)
  n <- length(sorted)
  k <- floor(n * alpha / 2)
  lower <- sorted[k + 1]
  upper <- sorted[n - k]
  output <- c(lower, upper)
  return(output)
}


# p-values -----------------------------------------------------------------

my_sim_p_value <- function(t, null_ts) {
  as_extreme <- abs(null_ts) >= abs(t)
  output <- my_mean(as_extreme)
  return(output)
}

my_p_value <- function(t, df) {
  lower_tail <- pt(-abs(t), df)
  output <- 2 * lower_tail
  return(output)
}
