#' Jaya Algorithm for Multi-Objective Optimization
#'
#' @description Implements the Jaya optimization algorithm for multi-objective optimization.
#' This algorithm supports non-dominated sorting and handles constraints and adaptive population sizes.
#'
#' @param objectives A list of objective functions to optimize.
#' @param lower Numeric vector specifying the lower bounds for variables.
#' @param upper Numeric vector specifying the upper bounds for variables.
#' @param popSize Population size. Default is 50.
#' @param maxiter Maximum number of iterations.
#' @param n_var Number of variables.
#' @param seed Random seed for reproducibility. Default is `NULL`.
#' @param suggestions Data frame of initial suggestions for starting population. Default is an empty data frame.
#' @param constraints A list of constraint functions. Each constraint should return a non-positive value if satisfied.
#' @param adaptive_pop Logical. Whether to adapt population size during optimization. Default is `FALSE`.
#' @param min_popSize Minimum population size if adaptive population is enabled. Default is 20.
#' @param max_popSize Maximum population size if adaptive population is enabled. Default is 100.
#' @param early_stopping Logical. Whether to stop early if no improvement is observed. Default is `FALSE`.
#' @param tolerance Numeric tolerance for early stopping. Default is 1e-6.
#' @param patience Number of iterations to wait for improvement before stopping. Default is 10.
#'
#' @return A list containing:
#' - `Pareto_Front`: A data frame of non-dominated solutions with decision variables and their corresponding objective values.
#' - `Solutions`: The final population including decision variables and their objective values.
#'
#' @examples
#' # Example: Multi-objective optimization
#' sphere_function_1 <- function(x) sum(x^2)
#' sphere_function_2 <- function(x) sum((x - 2)^2)
#' result <- jaya_multi(
#'   objectives = list(sphere_function_1, sphere_function_2),
#'   lower = rep(-5, 3),
#'   upper = rep(5, 3),
#'   popSize = 20,
#'   maxiter = 50,
#'   n_var = 3
#' )
#' print(summary(result))
#'
#' @export
jaya_multi <- function(
    objectives, lower, upper, popSize = 50, maxiter, n_var, seed = NULL,
    suggestions = data.frame(), constraints = list(),
    adaptive_pop = FALSE, min_popSize = 20, max_popSize = 100,
    early_stopping = FALSE, tolerance = 1e-6, patience = 10) {

  set.seed(seed)
  # Initialize population
  tab <- data.frame(matrix(runif((popSize - nrow(suggestions)) * n_var,
                                 min = rep(lower, each = popSize - nrow(suggestions)),
                                 max = rep(upper, each = popSize - nrow(suggestions))),
                           ncol = n_var))
  colnames(tab) <- paste0("x", 1:n_var)
  tab <- rbind(suggestions, tab)

  # Evaluate initial population and store as a list of vectors
  func_values <- lapply(seq_len(nrow(tab)), function(i) sapply(objectives, function(f) f(as.numeric(tab[i, 1:n_var]))))
  func_values_df <- do.call(rbind, func_values)
  colnames(func_values_df) <- paste0("Obj", 1:length(objectives))
  tab <- cbind(tab, func_values_df)

  # Non-dominated sorting function to include decision variables with objective values
  non_dominated_sort <- function(values, decision_vars) {
    pareto_front <- list()
    for (i in 1:nrow(values)) {
      is_dominated <- FALSE
      for (j in 1:nrow(values)) {
        if (all(values[j, ] <= values[i, ]) && any(values[j, ] < values[i, ])) {
          is_dominated <- TRUE
          break
        }
      }
      if (!is_dominated) {
        pareto_front[[length(pareto_front) + 1]] <- c(decision_vars[i, ], values[i, ])
      }
    }
    do.call(rbind, pareto_front)
  }

  # Initial Pareto front
  pareto_front <- non_dominated_sort(func_values_df, tab[, 1:n_var])

  # Early stopping parameters
  no_improvement_count <- 0
  best_pareto_size <- nrow(pareto_front)

  # Main optimization loop
  for (i in 1:maxiter) {
    r1 <- runif(n_var)
    r2 <- runif(n_var)

    # Adaptive population adjustment
    if (adaptive_pop && i %% 10 == 0) {
      current_popSize <- max(min_popSize, min(nrow(tab) + ifelse(i %% 20 == 0, 5, -5), max_popSize))
      tab <- tab[1:current_popSize, ]
    }

    for (can in 1:nrow(tab)) {
      candidate <- as.numeric(tab[can, 1:n_var])

      # Select random Pareto front solutions for updating candidate
      best_index <- sample(1:nrow(pareto_front), 1)
      worst_index <- which.max(rowSums(func_values_df))
      best_solution <- as.numeric(pareto_front[best_index, 1:n_var])
      worst_solution <- as.numeric(tab[worst_index, 1:n_var])

      # Update candidate position based on Pareto best and worst
      updated_candidate <- candidate + r1 * (best_solution - candidate) - r2 * (worst_solution - candidate)

      # Enforce boundaries
      updated_candidate <- pmax(pmin(updated_candidate, upper), lower)

      # Check constraints
      feasible <- all(sapply(constraints, function(con) {
        constraint_val <- con(updated_candidate)
        !is.na(constraint_val) && constraint_val <= 0
      }))

      if (feasible) {
        # Evaluate updated candidate for each objective
        new_func_vals <- sapply(objectives, function(obj) obj(updated_candidate))

        # Only proceed if there are no NA values in new_func_vals
        if (!any(is.na(new_func_vals))) {
          is_better <- all(new_func_vals <= func_values[[can]]) && any(new_func_vals < func_values[[can]])

          if (is_better) {
            tab[can, 1:n_var] <- updated_candidate
            func_values_df[can, ] <- new_func_vals
          }
        }
      }
    }

    # Update Pareto front with decision variables included
    pareto_result <- non_dominated_sort(func_values_df, tab[, 1:n_var])
    pareto_front <- unique(rbind(pareto_front, pareto_result))

    # Early stopping check
    if (early_stopping) {
      current_pareto_size <- nrow(pareto_front)
      if (abs(current_pareto_size - best_pareto_size) < tolerance) {
        no_improvement_count <- no_improvement_count + 1
      } else {
        no_improvement_count <- 0
        best_pareto_size <- current_pareto_size
      }
      if (no_improvement_count >= patience) {
        message("Early stopping triggered due to lack of improvement.")
        break
      }
    }
  }

  # Prepare final output with decision variables and objective values in Pareto front
  pareto_front_df <- data.frame(pareto_front)
  colnames(pareto_front_df) <- c(paste0("x", 1:n_var), paste0("Objective_", 1:length(objectives)))

  # Return result with updated format
  result <- list("Pareto_Front" = pareto_front_df, "Solutions" = tab)


  # Set attributes for summary
  attr(result, "popSize") <- popSize
  attr(result, "maxiter") <- maxiter
  attr(result, "n_var") <- n_var
  attr(result, "lower") <- lower
  attr(result, "upper") <- upper
  attr(result, "objectives") <- objectives  # store objectives for display

  class(result) <- "jaya_multi"
  return(result)
}
