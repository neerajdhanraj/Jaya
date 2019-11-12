#' #' Function to plot the 'best value' VS 'no. of iterations'
#'
#' @param x as an output object from 'jaya' function
#' @param ... as Additional graphical parameters given to plot function
#' @importFrom graphics plot
#' @importFrom graphics points
#' @return Returns plot showing 'best value' VS 'no. of iterations'
#' @export

plot.jaya <- function(x, ...)
{
  args <- list(...)
  f <- x$values_each_iter
  iter <- seq(1, length(f))
  plotdf <- data.frame(iter, f)
  if (length(args) == 0)
  {
    plot(x = plotdf$iter, y=plotdf$f, type = 'l', xlab = "No. of Iterations", ylab = "Best value of f(x)")
    points(x = plotdf$iter, y=plotdf$f)
  }
  else
  {
    plot(x = plotdf$iter, y=plotdf$f, type = 'l', xlab = "No. of Iterations", ylab = "Best value of f(x)", ...)
    points(x = plotdf$iter, y=plotdf$f, ...)
  }
}
