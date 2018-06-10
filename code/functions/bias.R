#' @description Calculating the systematic error (average BIAS)
#' @concepts See Eq.17 from Chen 2000.
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jun
#' 
#' @param x the sample
#' @return The SE from the sample
#' 
#' @references https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean

bias <- function(n, theta_0) (1/1000)*summary(1000 - theta_0)
