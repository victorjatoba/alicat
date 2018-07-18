#' @description Calculating the RMSE of the list of thetas hat and true by the item (1 - 35)
#' @concepts See Eq.17 from Chen 2000.
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jun
#' 
#' @param thHatList the list of thetas hat of each examinee
#' @param thTrueList the list of true thetas of each examinee
#' @param item the item (from 1 to 45)
#' 
#' @return The BIAS of the difference by thetas hat and true thetas of the item

rmse <- function(thHatList, thTrueList, item) {
  diffList = matrix(nrow = 0, ncol = 1)
  qtdExaminees = length(thHatList)
  for(k in 1:qtdExaminees) {
    diffList =  rbind(diffList, c((thHatList[[k]][item] - thTrueList[[k]])^2) )
  }
  
  return (sqrt(sum(diffList)/qtdExaminees))
}