#' Plot Function for Jaya Algorithm Results
#'
#' @description This function generates plots for single-objective optimization results from the Jaya algorithm.
#' It visualizes the best objective function value against the number of iterations.
#'
#' @param x An object of class \code{jaya} containing the optimization results from the \code{jaya} function.
#' @param ... Additional graphical parameters passed to the \code{plot} function.
#'
#' @details This function supports plotting results for single-objective optimization.
#' It creates a plot of the best objective function value observed across iterations.
#' Ensure that the input object is from the \code{jaya} function.
#'
#' @examples
#' # Example: Single-objective optimization
#' sphere_function <- function(x) sum(x^2)
#'
#' lower_bounds <- rep(-5, 3)
#' upper_bounds <- rep(5, 3)
#' pop_size <- 20
#' max_iterations <- 50
#' num_variables <- length(lower_bounds)
#'
#' # Run optimization
#' single_result <- jaya(
#'   fun = sphere_function,
#'   lower = lower_bounds,
#'   upper = upper_bounds,
#'   popSize = pop_size,
#'   maxiter = max_iterations,
#'   n_var = num_variables,
#'   opt = "minimize"
#' )
#'
#' # Plot the result
#' plot(single_result)
#'
#' @importFrom graphics plot points
#' @export
plot.jaya <- function(x, ...) {
  if ("Iterations" %in% names(x) && !is.null(x$Iterations)) {
    iterations <- as.numeric(unlist(x$Iterations))
    args <- list(...)

    # Extract main title if provided, or use default
    main_title <- if ("main" %in% names(args)) args$main else "Single-Objective Optimization"
    args$main <- NULL  # Remove main to prevent duplication

    # Create the plot
    do.call(plot, c(list(
      x = seq_along(iterations),
      y = iterations,
      type = 'o',
      pch = 19,
      col = "blue",
      xlab = "No. of Iterations",
      ylab = "Best value of f(x)",
      main = main_title
    ), args))
  } else {
    stop("Unrecognized input format for plotting. Ensure the object is from jaya().")
  }
}






#' Pairwise Plot Function for Multi-Objective Optimization Results
#'
#' @description Generates pairwise 2D plots for all combinations of objectives in the Pareto front.
#' This function visualizes trade-offs between different objectives.
#' @param x An object of class \code{jaya_multi} containing the optimization results, including the Pareto front.
#' @param objectives A vector of objective column names to include in the pairwise plots.
#'   If \code{NULL}, all objectives in the Pareto front are used.
#' @param ... Additional graphical parameters passed to the \code{plot} function.
#' @details The function automatically detects objectives in the Pareto front if not specified.
#' It creates pairwise plots for all possible combinations of objectives.
#' @examples
#' # Example usage of plot_jaya_multi_pairwise
#' # Define sample multi-objective optimization problem
#' objective1 <- function(x) sum(x^2)
#' objective2 <- function(x) sum(abs(x))
#' objective3 <- function(x) sum(x^3)
#' objective4 <- function(x) sum(x^4)
#'
#' objectives <- list(objective1, objective2, objective3, objective4)
#' lower_bounds <- c(-5, -5, -5)
#' upper_bounds <- c(5, 5, 5)
#'
#' # Run multi-objective optimization using jaya_multi
#' set.seed(42)
#' multi_result <- jaya_multi(
#'   objectives = objectives,
#'   lower = lower_bounds,
#'   upper = upper_bounds,
#'   popSize = 50,
#'   maxiter = 100,
#'   n_var = length(lower_bounds)
#' )
#'
#' # Pairwise plot of objectives
#' plot_jaya_multi_pairwise(multi_result)
#' @importFrom graphics plot par
#' @export

plot_jaya_multi_pairwise <- function(x, objectives = NULL, ...) {
  # Check that x contains a Pareto_Front component
  if ("Pareto_Front" %in% names(x) && !is.null(x$Pareto_Front)) {
    pareto_front <- x$Pareto_Front

    # If no specific objectives are specified, use all objectives in the Pareto front
    if (is.null(objectives)) {
      objectives <- names(pareto_front)[sapply(names(pareto_front), function(col) grepl("Objective", col))]
    }

    # Ensure we have at least two objectives to plot
    if (length(objectives) < 2) {
      stop("At least two objectives are required for pairwise plotting.")
    }

    # Create pairwise 2D plots for each pair of objectives
    num_obj <- length(objectives)
    plot_index <- 1

    # Set up a plotting area with multiple panels (adjust layout dynamically based on number of objectives)
    par(mfrow = c(num_obj - 1, num_obj - 1))

    for (i in 1:(num_obj - 1)) {
      for (j in (i + 1):num_obj) {
        x_values <- as.numeric(unlist(pareto_front[[objectives[i]]]))
        y_values <- as.numeric(unlist(pareto_front[[objectives[j]]]))

        # Create a 2D plot for the pair of objectives
        plot(x_values, y_values,
             xlab = objectives[i], ylab = objectives[j],
             main = paste(objectives[i], "vs", objectives[j]), col = "blue",
             pch = 19, ...)
        plot_index <- plot_index + 1
      }
    }

    # Reset the plotting layout
    par(mfrow = c(1, 1))
  } else {
    stop("The input object does not contain a Pareto_Front component.")
  }
}



