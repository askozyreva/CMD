
if(!require("pacman")) install.packages("pacman"); library(pacman)

p_load("rio", "tidyverse", "expss", "parsedate", "tools","readxl")

library(tidyverse)
library(expss)
library(rio)
library(parsedate)
library(tools)
library(readxl)


revscore <- function (x,mm) {  #this reverse scores a scale for items of reversed polarity
  return ((mm+1)-x)            ## variables passed to the fucntion by POSITIOn  not by name
}
# Give rounded percentage
Per = function( d, i, r){
  if (missing(r) == TRUE){
    r = 0
  }
  round( (sum(d == i, na.rm=T) / length(d) * 100), r )
}
# Calculate the mode
mode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
# Calculate rounded mean & sd - for mkdown report writing.
m = function(x, dp) {
  if (missing(dp)){dp=0}
  k = round(mean(x, na.rm = T),dp)
  return(k)
}
s = function(x, dp) {
  if (missing(dp)){dp=0}
  k = round(sd(x, na.rm=T),dp)
  return(k)
}


fnc_perc <- function(x) {
  # function that rounds numbers nicely:
  # [adding up rounded percentages to equal 100%](http://dochoffiday.com/professional/adding-up-rounded-percentages-to-equal-100)

  # floor-round percentages
  perc_floor <- floor(100 * x)

  # calculate how many percentage points need to be topped up
  top_up <- 100 - sum(perc_floor)

  # order percentages according to their decimal value
  top_up_indices <-
    order(100 * x - perc_floor, decreasing = TRUE)[1:top_up]

  # top up the floor-rounded percentages
  perc <- perc_floor
  perc[top_up_indices] <- perc[top_up_indices] + 1

  # check
  expect_equal(sum(perc), 100)

  return(perc)
}
#fnc_perc(c(.405, .206, .389))

fnc_perc2 <- function(x) {
  # function that rounds numbers nicely:
  # [adding up rounded percentages to equal 100%](http://dochoffiday.com/professional/adding-up-rounded-percentages-to-equal-100)

  # floor-round percentages
  perc_floor <- floor(100 * x)

  # calculate how many percentage points need to be topped up
  top_up <- 100 - sum(perc_floor)

  # order percentages according to their decimal value
  top_up_indices <-
    order(100 * x - perc_floor, decreasing = TRUE)[1:top_up]

  # top up the floor-rounded percentages
  perc <- perc_floor
  perc[top_up_indices] <- perc[top_up_indices] + 1


  return(perc)
}


