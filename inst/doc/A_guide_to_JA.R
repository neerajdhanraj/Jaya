## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
if (!require("evaluate")) install.packages("evaluate")

## ----setup---------------------------------------------------------------
library(Jaya)

## ----unconstrainted problem----------------------------------------------
# Test Function to minimize
square <- function(x){
  return((x[1]*x[1])+(x[2]*x[2]))
}

## ----fig.width=7, fig.height=5-------------------------------------------
a <- jaya(fun = square, lower = c(-100,-100), upper = c(100,100), maxiter = 50, n_var = 2, seed = 100)

summary(a)

plot(a)

## ----g24-----------------------------------------------------------------
g24 <- function(x)
{
  f <- f(x)
  pen1 <- max(0, c1(x))
  pen2 <- max(0, c2(x))
  return(f + pen1 + pen2)
}

f <- function(x)
{ return(-x[1]-x[2]) }

#Constraints
c1 <- function(x)
{ return( -2*(x[1]**4) + 8*(x[1]**3) - 8*(x[1]**2) + x[2] - 2 ) }

c2 <- function(x)
{ return( -4*(x[1]**4) + 32*(x[1]**3) - 88*(x[1]**2) + 96*x[1] + x[2] -36 ) }

## ----fig.width=7, fig.height=5-------------------------------------------
b <- jaya(fun = g24, lower = c(0,0), upper = c(3,4), popSize = 30, maxiter = 30, n_var = 2, seed = 100)

summary(b)

plot(b)

## ----g11-----------------------------------------------------------------
# Test Function to minimize
g11 <- function(x)
{
  f <- f(x)
  if(round(c1(x),2) != 0){
    return(f + c1(x))
  }
  return(f)
}

f <- function(x)
{ return(x[1]**2 + (x[2] - 1)**2) }

c1 <- function(x)
{ return(x[2] - x[1]**2) }

## ----fig.width=7, fig.height=5-------------------------------------------
c <- jaya(fun = g11, lower = c(-1,-1), upper = c(1,1), maxiter = 100, n_var = 2, seed = 100)

summary(c)

plot(c)

## ------------------------------------------------------------------------
# Function to test for
f <- function(x)
{ return( 100*((x[1]**2 - x[2])**2) + (1 - x[1])**2 ) }

# Constraints
c1 <- function(x)
{ return( (x[1]*x[2]) + x[1] - x[2] + 1.5) }

c2 <- function(x)
{ return(10 - (x[1]*x[2])) }

# Function with penalty
con <- function(x){
  func <- -f(x)
  pen <- sqrt(.Machine$double.xmax)
  pen1 <- max(0, c1(x))*pen
  pen2 <- max(0, c2(x))*pen
  return(func - pen1 - pen2)
}

## ----GA, message=FALSE, warning=FALSE, paged.print=FALSE-----------------
library(GA)
G <- ga("real-valued", fitness = con, lower = c(0,0), upper = c(1,13), 
         maxiter = 1000, run = 200, seed = 123)
# Values of x1 and x2
G@solution
# Value of f(x)
G@fitnessValue

## ----fig.width=7, fig.height=5-------------------------------------------
d <- jaya(fun = con, lower = c(0,0),  upper = c(1,13), maxiter = 100, n_var = 2, seed = 123, opt = "Maximize")

summary(d)

plot(d)

