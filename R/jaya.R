#' Jaya Algorithm, a gradient-free optimization algorithm.
#' Maximization of a function using Jaya Algorithm (JA).
#' A population based method which repeatedly modifies a population of individual solutions.
#' Capable of solving both constrained and unconstrained optimization problems.
#' Does not contain any hyperparameters.
#'
#' @param fun as a function to be optimized
#' @param lower as a vector of lower bounds for the vaiables in the function
#' @param upper as a vector of upper bounds for the vaiables in the function
#' @param popSize as population size
#' @param maxiter as number of iterations to run for finding optimum solution
#' @param n_var as number of variables used in the function to optimize
#' @param seed as an integer vector containing the random number generator state
#' @param suggestions as a data frame of solutions string to be included in the initial population
#' @param opt as a string either "maximize" or "minimize" the function
#' @keywords optimization
#' @importFrom stats runif
#' @export
#' @examples
#' # Test Function to minimize
#' square <- function(x){return((x[1]^2)+(x[2]^2))}
#' jaya(fun = square, lower = c(-100,-100), upper = c(100,100), maxiter = 10, n_var = 2)

jaya <- function(fun, lower, upper, popSize = 50, maxiter, n_var, seed = NULL, suggestions = data.frame(), opt = "minimize")
{
  best <- NULL            # Stores minimum(best) result
  worst <- NULL           # Stores worst result
  best_each_iter <- c()   # Stores best f(x) values for each iteration
  tab <- data.frame(1:(popSize - nrow(suggestions)))   # Table for storing variable and function values

  # Generate random values for variables in range(-100,100)
  for (i in 1:n_var) {
    if(!missing(seed)){
      set.seed(seed)
    }
    x <- runif((popSize - nrow(suggestions)), min = lower[i], max = upper[i])
    tab <- cbind(tab, x)
  }
  tab <- tab[,2:ncol(tab)]
  names(tab) <- names(suggestions)
  tab <- rbind(suggestions, tab)

  # Calculate values for f(x)
  func_values <- c()
  for (i in 1:popSize) {
    func_values <- c(func_values, fun(data.frame(t(tab))[,i]))
  }
  tab <- cbind(tab, func_values)

  # Find the best and worst f(x)
  if(tolower(opt) == "minimize"){
    best <- tab[which.min(tab[ , ncol(tab)]) , ]
    worst <- tab[which.max(tab[ , ncol(tab)]) , ]
  }
  else if(tolower(opt) == "maximize"){
    worst <- tab[which.min(tab[ , ncol(tab)]) , ]
    best <- tab[which.max(tab[ , ncol(tab)]) , ]
  }
  else{
    stop(paste('opt can have only 2 values : "minimize" or "maximize". Passed value is ', opt, '.'))
  }

  best_each_iter <- c(best_each_iter, best[, ncol(best)])

  # iterate till maxiter satisfies
  i <- 0
  while (i < maxiter) {
    # Generate r1 and r2 random variables for each variable
    r1 <- runif(n_var)
    r2 <- runif(n_var)

    # For each row(candidate) in Table
    for (can in 1:popSize) {
      t <- c()

      # For each column(variable)
      for (v in 1:n_var){

        # Update values for variables and store in t
        x <- tab[can,v] + r1[v]*(best[v] - abs(tab[can,v])) - r2[v]*(worst[v] - abs(tab[can,v]))
        if((lower[v] <= x) && (x <= upper[v]))
        {
          t <- c(t,data.frame(x)[,1])
        }
      }

      if(length(t) == n_var){
        # Calculate new function value using new variables
        new_func_val <- fun(t)

        # If function value is better than original value, then, replace in Table
        # Else, keep the original values

        if(tolower(opt) == "minimize")
        {
          if(new_func_val < tab[can,ncol(tab)]) {
            tab[can,] <- c(t, new_func_val)
          }
        }
        else if(tolower(opt) == "maximize")
        {
          if(new_func_val > tab[can,ncol(tab)]) {
            tab[can,] <- c(t, new_func_val)
          }
        }
      }
    }

    # Update best and worst after updating table
    if(tolower(opt) == "minimize")
    {
      best <- tab[which.min(tab[ , ncol(tab)]) , ]
      worst <- tab[which.max(tab[ , ncol(tab)]) , ]
    }
    else if(tolower(opt) == "maximize"){
      worst <- tab[which.min(tab[ , ncol(tab)]) , ]
      best <- tab[which.max(tab[ , ncol(tab)]) , ]
    }

    best_each_iter <- c(best_each_iter, best[, ncol(best)])

    # Increment iterator
    i <- i + 1
  }

  # Give names to columns of best
  names_list <- NULL
  for (i in 1:n_var) {
    names_list <- c(names_list, paste0("x",i))
  }

  names_list <- c(names_list, "f(x)")
  colnames(best) <- names_list
  row.names(best) <- "Best"

  # Return best
  result <- list(best)
  names(result) <- c("Best")

  p <- list("fun" = fun, "best" = best, "lower" = lower, "upper" = upper, "popSize" = popSize,
            "maxiter" = maxiter, "n_var" = n_var, "suggestions" = suggestions, "opt" = opt, "values_each_iter" = best_each_iter)

  #setClass(Class = "jaya", representation(params = "list", best = "data.frame"))

  #return(new(Class = "jaya", params = p, best = best))
  class(p) <- c("jaya", class(p))
  print(p$fun)
  print(p$best)
  return(invisible(p))
  #return(p)
}
