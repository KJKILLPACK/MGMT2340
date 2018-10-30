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


#' Title
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

#' Title
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

#' Title
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





