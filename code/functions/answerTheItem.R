#' @description Verify if exist a valid response
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jun
#' 
#' @param response The user item answer
#' @return boolean
answerTheItem <- function(response) {
  return (
    response == "0" || response == "1"
  )
}