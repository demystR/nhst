# My functions
#
# Add each function you build to this file, under the heading of the
# chapter you built it in. Later exercise files load it with
# source("my_functions.R"), so every function here can be reused.
#
# Keep only the functions in this file. Run and check them in your
# exercise files, not here.
#
# Draft: the Part 1 chapters are not written yet, so their functions
# are filled in here for you.


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


# p-values -----------------------------------------------------------------
