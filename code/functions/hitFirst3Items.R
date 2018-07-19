#' @description Verify if exist a valid response
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jun
#' 
#' @param response The user item answer
#' @return boolean
hitFirst3Items = function(itemsList) {
  return (length(itemsList) >= 3 && itemsList[[1]] == 1 && itemsList[[2]] == 1 && itemsList[[3]] == 1)
}