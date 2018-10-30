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
load("~/KillpackStatsPackage/Marital_Status1.Rdata")
}



