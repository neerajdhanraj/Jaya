#' Summary Method for Jaya Algorithm Optimization Results
#'
#' @description Provides a summary of optimization results for both single-objective
#' and multi-objective cases. Displays key parameters, limits, and results such as
#' the best solution for single-objective optimization or the Pareto front for multi-objective optimization.
#'
#' @param object An object of class \code{jaya} or \code{jaya_multi}, containing the results of the Jaya optimization.
#' @param ... Additional arguments (currently unused).
#' @details
#' - For single-objective optimization, the summary includes the best solution and the associated function value.
#' - For multi-objective optimization, the summary displays the objectives, decision variable limits,
#' and the first few entries of the Pareto front.
#' - Automatically handles missing or incomplete attributes gracefully.
#'
#' @examples
#' # Single-objective optimization example
#' sphere_function <- function(x) sum(x^2)
#' single_result <- jaya(
#'   fun = sphere_function,
#'   lower = c(-5, -5, -5),
#'   upper = c(5, 5, 5),
#'   popSize = 20,
#'   maxiter = 50,
#'   n_var = 3,
#'   opt = "minimize"
#' )
#' summary(single_result)
#'
#' # Multi-objective optimization example
#' objective1 <- function(x) sum(x^2)
#' objective2 <- function(x) sum(abs(x))
#' multi_result <- jaya_multi(
#'   objectives = list(objective1, objective2),
#'   lower = c(-5, -5, -5),
#'   upper = c(5, 5, 5),
#'   popSize = 50,
#'   maxiter = 100,
#'   n_var = 3
#' )
#' summary(multi_result)
#'
#' @importFrom utils head
#' @export

summary.jaya <- function(object, ...) {
  cat("Jaya Algorithm\n")

  # Extract attributes, with defaults if missing
  popSize <- if (!is.null(attr(object, "popSize"))) attr(object, "popSize") else "Not specified"
  maxiter <- if (!is.null(attr(object, "maxiter"))) attr(object, "maxiter") else "Not specified"
  n_var <- if (!is.null(attr(object, "n_var"))) attr(object, "n_var") else "Not specified"
  lower <- if (!is.null(attr(object, "lower"))) attr(object, "lower") else rep("Not specified", n_var)
  upper <- if (!is.null(attr(object, "upper"))) attr(object, "upper") else rep("Not specified", n_var)

  # Check if the object is single-objective or multi-objective based on contents
  if ("Best" %in% names(object) && !is.null(object$Best)) {
    # Single-objective summary
    opt <- if (!is.null(attr(object, "opt"))) attr(object, "opt") else "Not specified"
    cat("Population Size      =", popSize, "\n")
    cat("Number of iterations =", maxiter, "\n")
    cat("Number of variables  =", n_var, "\n")
    cat("\nObjective:", opt, "\n")

    # Print objective function if provided
    if (!is.null(attr(object, "fun"))) {
      cat("Objective Function:\n")
      print(attr(object, "fun"))
    } else {
      cat("Objective function not provided.\n")
    }

    # Print variable limits
    cat("\nLimits:\n")
    for (i in 1:length(lower)) {
      cat(paste0("x", i, " = [", lower[i], ", ", upper[i], "]\n"))
    }

    # Display best result
    cat("\nBest Result:\n")
    print(data.frame(Best = object$Best))

  } else if ("Pareto_Front" %in% names(object) && !is.null(object$Pareto_Front)) {
    # Multi-objective summary
    cat("Population Size      =", popSize, "\n")
    cat("Number of iterations =", maxiter, "\n")
    cat("Number of variables  =", n_var, "\n")

    # Print objectives if provided
    objectives <- attr(object, "objectives")
    if (!is.null(objectives)) {
      cat("\nObjectives:\n")
      for (i in seq_along(objectives)) {
        cat(paste0("Objective ", i, ": ", deparse(objectives[[i]])), "\n")
      }
    } else {
      cat("\nObjectives not provided.\n")
    }

    # Print variable limits
    cat("\nLimits:\n")
    for (i in 1:length(lower)) {
      cat(paste0("x", i, " = [", lower[i], ", ", upper[i], "]\n"))
    }

    # Display Pareto front
    cat("\nPareto Front (first few entries):\n")
    print(head(object$Pareto_Front, 5))  # Show first few rows for brevity

  } else {
    stop("Unrecognized jaya object format.")
  }
}
