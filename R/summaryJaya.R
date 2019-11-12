#' Function to summarize the Jaya function
#'
#' @param object as an output object from 'jaya' function
#' @param \dots Additional parameters given to the function
#' @return returns the summary of output object from 'jaya' function
#' @export
#' @examples
#' # Test Function to minimize
#' square <- function(x){return((x[1]^2)+(x[2]^2))}
#' a <- jaya(fun = square, lower = c(-100,-100), upper = c(100,100), maxiter = 10, n_var = 2)
#' summary(a)

summary.jaya <- function(object, ...)
{
  args <- list(...)
  cat("Jaya Algorithm\n")
  cat(paste("Population Size      = ", object$popSize, "\n"))
  cat(paste("Number of iterations = ", object$maxiter), "\n")
  cat(paste("Number of variables  = ", object$n_var, "\n"))
  if(length(object$suggestions) != 0)     # If suggestions are given
  {

    row.names(object$suggestions) <- c()
    colnames(object$suggestions) <- colnames(object@best)[1:object$n_var]
    cat(paste("Suggestions:\n"))
    print(object$suggestions)
  }
  cat(paste0("\n",toupper(object$opt), ",\n"))
  print(object$fun)


  cat("\nLimits: \n")
  for (i in 1:object$n_var) {
    cat(paste0("x", i, " = [", object$lower[i], ",", object$upper[i], "]\n"))
  }
  cat(paste("\nBest Result:  \n"))
  print(object$best)
  cat("Suggestions for the initial population: \n")
  print(object$suggestions)
}
