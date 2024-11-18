#' Jaya Algorithm for Single-Objective Optimization
#'
#' @description Implements the Jaya optimization algorithm for single-objective optimization.
#' The algorithm minimizes or maximizes the given objective function over specified bounds.
#'
#' @param objectives (optional) A list of functions for multi-objective optimization.
#' @param constraints (optional) A list of constraints as functions returning <= 0 for feasibility.
#' @param early_stopping Logical. If TRUE, stops optimization early based on tolerance and patience.
#' @param tolerance Numeric. Tolerance for early stopping.
#' @param patience Integer. Number of iterations to wait for improvement before stopping early.
#' @param adaptive_pop Logical. If TRUE, enables adaptive population size adjustment.
#' @param min_popSize Integer. Minimum population size for adaptive adjustment.
#' @param max_popSize Integer. Maximum population size for adaptive adjustment.
#' @param parallel Logical. If TRUE, enables parallel computation for evaluating population.
#' @param cores Integer. Number of cores to use for parallel computation. Defaults to all available cores minus one.
#' @param fun Objective function to be minimized or maximized (single-objective).
#' @param lower Vector of lower bounds for the decision variables.
#' @param upper Vector of upper bounds for the decision variables.
#' @param popSize Size of the population for the optimization process.
#' @param maxiter Maximum number of iterations.
#' @param n_var Number of decision variables.
#' @param seed Optional random seed for reproducibility.
#' @param suggestions Optional data frame of initial population suggestions.
#' @param opt Specify whether to "minimize" or "maximize" the objective function.
#' @importFrom stats runif na.omit
#' @importFrom utils head
#' @importFrom graphics par
#' @return A list containing the following:
#' - `Best`: The best solution found (variable values and objective function value).
#' - `Iterations`: Best objective function values at each iteration.
#'
#' @examples
#' # Example: Single-objective optimization
#' sphere_function <- function(x) sum(x^2)
#' result <- jaya(
#'   fun = sphere_function,
#'   lower = rep(-5, 3),
#'   upper = rep(5, 3),
#'   popSize = 20,
#'   maxiter = 50,
#'   n_var = 3,
#'   opt = "minimize"
#' )
#' print(summary(result))
#' plot(result)
#'
#' @export

jaya <- function(
    fun = NULL, lower, upper, popSize = 50, maxiter, n_var, seed = NULL,
    suggestions = data.frame(), opt = "minimize",
    objectives = NULL, constraints = list(),
    early_stopping = FALSE, tolerance = 1e-6, patience = 10,
    adaptive_pop = FALSE, min_popSize = 20, max_popSize = 100,
    parallel = FALSE, cores = NULL) {

  set.seed(seed)
  is_multi_objective <- !is.null(objectives)
  if (is_multi_objective) {
    fun <- objectives
  } else if (!is.null(fun)) {
    fun <- list(fun)
  } else {
    stop("No objective function provided.")
  }

  # Detect number of cores if not specified and parallel is enabled
  if (parallel) {
    if (is.null(cores)) {
      cores <- max(parallel::detectCores() - 1, 1)
    }
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl, varlist = ls(), envir = environment())
  }

  # Initialize population
  tab <- data.frame(matrix(runif((popSize - nrow(suggestions)) * n_var,
                                 min = rep(lower, each = popSize - nrow(suggestions)),
                                 max = rep(upper, each = popSize - nrow(suggestions))),
                           ncol = n_var))
  colnames(tab) <- paste0("x", 1:n_var)
  tab <- rbind(suggestions, tab)

  # Evaluate initial population
  evaluate_population <- function(i) sapply(fun, function(f) f(as.numeric(tab[i, 1:n_var])))
  tab$func_values <- if (is_multi_objective) {
    if (parallel) {
      parallel::parLapply(cl, seq_len(nrow(tab)), evaluate_population)
    } else {
      lapply(seq_len(nrow(tab)), evaluate_population)
    }
  } else {
    apply(tab[, 1:n_var], 1, fun[[1]])
  }

  # Initialize best/worst for single-objective and Pareto front for multi-objective
  if (!is_multi_objective) {
    if (tolower(opt) == "minimize") {
      best <- tab[which.min(tab$func_values), ]
      worst <- tab[which.max(tab$func_values), ]
    } else {
      best <- tab[which.max(tab$func_values), ]
      worst <- tab[which.min(tab$func_values), ]
    }
    best_each_iter <- numeric(maxiter)
    best_each_iter[1] <- best$func_values
  } else {
    pareto_front <- data.frame(matrix(ncol = n_var + length(fun)))
    colnames(pareto_front) <- c(paste0("x", 1:n_var), paste0("f", 1:length(fun)))
  }

  # Enhanced non-dominated sorting function
  non_dominated_sort <- function(values) {
    values <- as.data.frame(do.call(rbind, values))
    pareto_front <- list()
    for (i in 1:nrow(values)) {
      is_dominated <- FALSE
      for (j in 1:nrow(values)) {
        if (!anyNA(values[j, ]) && !anyNA(values[i, ])) {
          if (all(values[j, ] <= values[i, ]) && any(values[j, ] < values[i, ])) {
            is_dominated <- TRUE
            break
          }
        }
      }
      if (!is_dominated) {
        pareto_front[[length(pareto_front) + 1]] <- values[i, , drop = FALSE]
      }
    }
    do.call(rbind, pareto_front)
  }

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

      if (!is_multi_objective) {
        updated_candidate <- candidate + r1 * (best[1:n_var] - candidate) - r2 * (worst[1:n_var] - candidate)
      } else {
        best_index <- sample(1:nrow(tab), 1)
        best_solution <- tab[best_index, 1:n_var]
        updated_candidate <- candidate + r1 * (best_solution - candidate) - r2 * (runif(n_var) * 2 - 1)
      }

      # Enforce boundaries
      updated_candidate <- pmax(pmin(updated_candidate, upper), lower)

      # Check constraints
      feasible <- all(sapply(constraints, function(con) {
        constraint_val <- con(updated_candidate)
        !is.na(constraint_val) && constraint_val <= 0
      }))

      if (feasible) {
        new_func_val <- if (is_multi_objective) sapply(fun, function(f) f(updated_candidate)) else fun[[1]](updated_candidate)

        if (!is_multi_objective) {
          if ((tolower(opt) == "minimize" && new_func_val < tab$func_values[can]) ||
              (tolower(opt) == "maximize" && new_func_val > tab$func_values[can])) {
            tab[can, 1:n_var] <- updated_candidate
            tab$func_values[can] <- new_func_val
          }
        } else {
          tab[can, 1:n_var] <- updated_candidate
          tab$func_values[[can]] <- new_func_val
        }
      }
    }

    # Update Pareto front for multi-objective
    if (is_multi_objective) {
      func_values_df <- do.call(rbind, tab$func_values)
      pareto_result <- non_dominated_sort(func_values_df)
      if (!is.null(pareto_result) && ncol(pareto_result) == ncol(pareto_front)) {
        colnames(pareto_result) <- colnames(pareto_front)
        pareto_front <- unique(rbind(pareto_front, pareto_result))
      }
    } else {
      if (tolower(opt) == "minimize") {
        best <- tab[which.min(tab$func_values), ]
        worst <- tab[which.max(tab$func_values), ]
      } else {
        best <- tab[which.max(tab$func_values), ]
        worst <- tab[which.min(tab$func_values), ]
      }
      best_each_iter[i] <- best$func_values
    }
  }

  # Clean up final Pareto front for multi-objective
  if (is_multi_objective) {
    pareto_front <- na.omit(unique(pareto_front))
  } else {
    colnames(best) <- c(paste0("x", 1:n_var), "f(x)")
    rownames(best) <- "Best"
  }

  # Stop parallel cluster
  if (parallel) parallel::stopCluster(cl)

  # Prepare result list
  result <- list("Best" = if (!is_multi_objective) best else NULL, "Iterations" = if (!is_multi_objective) best_each_iter else NULL)
  if (is_multi_objective) {
    result$Pareto_Front <- pareto_front
  }


  # Set attributes for summary
  attr(result, "popSize") <- popSize
  attr(result, "maxiter") <- maxiter
  attr(result, "n_var") <- n_var
  attr(result, "opt") <- opt
  attr(result, "lower") <- lower
  attr(result, "upper") <- upper
  attr(result, "fun") <- fun  # store function for display

  class(result) <- "jaya"
  return(result)
}
