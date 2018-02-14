#' @description Calculating the Standard Error (SE)
#' @concepts The standard error is just the standard
#'           deviation divided by the square root of
#'           the sample size.
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev
#' 
#' @param x the sample
#' @return The SE from the sample
#' 
#' @references https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean

se <- function(x) sd(x)/sqrt(length(x))
