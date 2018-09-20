#' Geometric Mean of a Population
#'
#' Finds the geometric mean of a population
#' @param x is a vector of intigers to run through the geometric mean formula
#' @return   The mean of the vector
#' @export
pop.geoMean <- function(x){
  #Geometric Mean of a Population
  prod.x <- prod(x)
  N <- length(x)
  gm <- prod.x^(1/N)
  return(gm)
}

#' Standard Deviation of Entire Population (N)
#'
#' Takes in an entire set of values representing the full population and finds the standard deviation without removing sample bias (n-1)
#' @param x A vector of intigers from which to calculate the standard deviation
#' @return  The standard deviation of a population
#' @export
pop.sd <- function(x){
  #Standard Deviation of a Population
  N <- length(x)
  m <- mean(x)
  a <- (x-m)^2
  sd <- sqrt((1/N)*(sum(a)))
  return(sd)
}


#' The average (geomean) rate of anual growth
#'
#' Finds the average rate of return for a set number of periods even with negative intigers
#' @param x a vector of intigers representing percentage growth written in decimal form (eg 1 percent written 0.01)
#' @return  the geometric rate of return accross all observations
#' @export
#' @examples  killpack.return.calc(ExcelImport$column), killpack.return.calc(c(0.1384,0.1125,-0.1234,0.5432))
killpack.return.calc <-function(x){
  #Growth of a fund
  n <- length(x)
  a <- (1+x)
  b <- prod(a)
  c <- ((b)^(1/n))-1
  return(c)
}


#' Variance of a Population
#'
#' Finds the variance of a whole population (N) removing the bias of sample sets (n-1)
#' @param x a vector of intigers from which to find the variance
#' @return  the variance of the Population (x)
#' @export
pop.var <- function(x){
  #Variance of a Population
  N <- length(x)
  m <- mean(x)
  a <- (x-m)^2
  var <- sum(a)/N
  return(var)
}

