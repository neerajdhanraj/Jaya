## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Jaya)

## ----single-optimization, fig.width=7, fig.height=6---------------------------
# Define the sphere function
sphere_function <- function(x) sum(x^2)

# Set optimization parameters
lower_bounds <- c(-5, -5, -5)
upper_bounds <- c(5, 5, 5)

# Run optimization
single_result <- jaya(
  fun = sphere_function,
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 20,
  maxiter = 50,
  n_var = length(lower_bounds),
  opt = "minimize"
)

# Summary of results
summary(single_result)

# Plot the best value over iterations
plot(single_result)

## ----multi-optimization, fig.width=7, fig.height=6----------------------------
# Define multiple objectives
objective1 <- function(x) sum(x^2)
objective2 <- function(x) sum((x - 2)^2)

# Combine objectives
objectives <- list(objective1, objective2)

# Run optimization
multi_result <- jaya_multi(
  objectives = objectives,
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 30,
  maxiter = 100,
  n_var = length(lower_bounds)
)

# Summary of results
summary(multi_result)


# Pairwise plots for multi-objective Pareto front
plot_jaya_multi_pairwise(multi_result)

## ----early-stopping-----------------------------------------------------------
early_stopping_result <- jaya(
  fun = sphere_function,
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 20,
  maxiter = 50,
  n_var = length(lower_bounds),
  early_stopping = TRUE,
  tolerance = 1e-6,
  patience = 5
)

summary(early_stopping_result)

