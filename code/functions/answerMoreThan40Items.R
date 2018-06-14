#' @description Verify if user anser equals or more than 40 items
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jun
#' 
#' @param x The array of responses
#' @return boolean
answerMoreThan40Items <- function(x) {
  return (
    sum(x == '.') <= 5 && sum(x == "NA") <= 5
  )
}
