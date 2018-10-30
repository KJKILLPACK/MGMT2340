FullCoursePackages <- function(){
  install.packages("jmv")
  install.packages("graphics")
  install.packages("ggplot")
  install.packages("readr")
  install.packages("readxl")
  install.packages("knitr")
  install.packages("rmarkdown")
  install.packages("rmdformats")
  install.packages("stats")
}


#' Chapter 2 Easy Starter
#'
#' @return downloads all files necessary for chapter 2 as well as loads all packages required
#' @export
#'
#' @examples LETSTUDYc1()
LETSTUDYc1 <- function(){
library(jmv)
library(graphics)
 load("~/KillpackStatsPackage/data/Marital_Status.Rdata")
}

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

#' µx to µy lognormal function
#'
#' @return µy of the lognormal µx
#' @export
#'
#' @param m enter the log mean µx
#' @param sd enter the log sd aka øx
#'
#' @examples X2YmLND(6, 3)
X2YmLND <- function(m, sd){
  V <- (m)^2
  a <- (2*sd)
  b <- (a+V)/2
  µy <- exp(b)
  return(µy)
}

#' øx to øy lognormal function
#'
#' @return øy of the lognormal øx
#' @export
#'
#' @param m enter the log mean µx
#' @param sd enter the log sd aka øx
#'
#' @examples X2YmLND(6, 3)
X2YsdLND <- function(m, sd){
  V <- (sd)^2
  a <- (2*m)
  Vy <- (exp(V)-1)*exp(a+V)
  øy <- (Vy)^(1/2)
  return(øy)
}

#' µy to µx lognormal function
#'
#' @return µx
#' @export
#'
#' @param m enter the log mean µy
#' @param sd enter the log sd aka øy
#'
#' @examples X2YmLND(6, 3)
Y2XmLND <- function(m, sd){
  µx  <- (m)^2
  øx  <- (sd)^2
  den <- ((µx+øx)^(1/2))
  div <- µx/den
  µ   <- log(div)
  return(µ)
}

#' øx to øy lognormal function
#'
#' @return øy
#' @export
#'
#' @param my enter the log mean sks µy
#' @param sdy yenter the log sd aka øy
#'
#' @examples X2YmLND(6, 3)
Y2XsdLND <- function(my, sdy){
  µx  <- (my)^2
  øx  <- (sdy)^2
  div <- øx/µx
  V   <- log(1+div)
  ø   <- (V)^(1/2)
  return(ø)
}

#' find Find z for P(Z < z)
#'
#' @param x value in question
#' @param m mean of the normal distribution
#' @param sd sd of the normal distribution
#'
#' @return zscore to look up probability on ztable from
#' @export
#'
#' @examples X2YmLND(6, 3)
zscore.KSP <- function(x, m, sd){
  z <- (x-m)/sd
  return(z)
}


#' Cumulative Uniform Distribution expected value (mean / arithimatic mean)
#'
#' @param Top the Greater number in the range
#' @param Bottom the lesser number in the range
#'
#' @return expected value (or mean) of the range
#' @export
#'
#' @examples A train usually arives between 30 to 200 seconds late what is the expected value CUDmean.KBS(200, 30)
CUDmean.KSP <- function(Top, Bottom){
  µ <- (Top+Bottom)/2
  return(µ)
}

#' Cumulative Uniform Distribution Standard Deviation
#'
#' @param Top the Greater number in the range
#' @param Bottom the lesser number in the range
#'
#' @return standard deviation of the range
#' @export
#'
#' @examples A train usually arives between 30 to 200 seconds late what is the standard deviation CUDsd.KBS(200, 30)
CUDsd.KSP <- function(Top, Bottom){
  V <- ((Top-Bottom)^2)/12
  ø <- V^(1/2)
  return(ø)
}

#' Cumulative Unifrom Distribution Probability P(X < x) calculator
#'
#' @param X value for probability P(X < x)
#' @param Top the Greater number in the range
#' @param Bottom the lesser number in the range
#'
#' @return probabiliy that X < x
#' @export
#'
#' @examples A train usually arives between 30 to 200 seconds late what is the probability the train arives within 160 seconds: CUDprob.KBS(160, 200, 30)
CUDprob.KSP <- function(X, Top, Bottom){
  A <- X-Bottom
  B <- 1/(Top-Bottom)
  C <- A*B
  return(C)
}








