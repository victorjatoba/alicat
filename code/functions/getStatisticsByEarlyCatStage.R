#' @description Calculate the  of the values there are on list column
#' 
#' @param list The list of values
#' @param col the column of the list
#' @return the  of the column values
bias <- function(thHatList, thTrueList, item) {
  diffList = matrix(nrow = 0, ncol = 1)
  qtdExaminees = length(thHatList)
  for(k in 1:qtdExaminees) {
    #options(error=recover)
    diffList =  rbind( diffList, c(thHatList[[k]][item] - thTrueList[[k]]) )
  }
  
  return (sum(diffList)/qtdExaminees)
}

rmse <- function(thHatList, thTrueList, item) {
  diffList = matrix(nrow = 0, ncol = 1)
  qtdExaminees = length(thHatList)
  for(k in 1:qtdExaminees) {
    diffList =  rbind(diffList, c((thHatList[[k]][item] - thTrueList[[k]])^2) )
  }
  
  return (sqrt(sum(diffList)/qtdExaminees))
}

###################
#' @description Get the statistics (BIAS, RMSE and SE) of the ISR from the CAT early stage
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018, jul
#' 
#' @param isr the name of the Item Selection Rule to be runned. Go to
#'              uts/isr_compair/ to see the available rules
#'              Example: "MFI", "KLP", "KL", "MPWI", etc.
#' @param initValue the initialization loop value
#' @param stopValue the stop loop criteria
#' @param step the step velocity of the loop
#' @param format should be out or json
#' 
#' @return The intervals of the  of the selected items quantities
###################

## LIBS ##
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)
library(jsonlite)
##########

getStatisticsByEarlyCatStage = function(isr, package, initValue, stopValue, step) {
  
  file = paste(isr , "-statistics.", "json", sep = "")
  isrPath <- paste("outs/",package,"/",file, sep="")
  
  ## ISR results load
  isr_out = fromJSON(isrPath)
  
  rangeByStatistics <- matrix(nrow = 0, ncol = 18)
  colnames(rangeByStatistics) <- c("BiasItem1", "BiasItem2", "BiasItem3", "BiasItem4", "BiasItem5",
                                   "BiasItem10", "BiasItem20", "BiasItem30",
                                   
                                   "RmseItem1", "RmseItem2", "RmseItem3", "RmseItem4", "RmseItem5",
                                   "RmseItem10", "RmseItem20", "RmseItem30",
                                   
                                   "rangeV1", "rangeV2")
  #rownames(rangeByStatistics) <- c("")
  
  # 6 intervals (-2, -1, 0, ..., 4)
  for (initValue in seq(initValue, stopValue, by = step)) {
    # getting subset of users between initValue and initValue+step
    # thFinal is the isr_out attribute
    range <- subset(isr_out, ThTrue >= initValue & ThTrue < (initValue+step))
    
    thHatList = range$EstimatedThetas
    thTrueList = range$ThTrue
    if (length(thHatList) > 0) {
      biasItem1 = bias(thHatList, thTrueList, item = 1)
      rmseItem1 = rmse(thHatList, thTrueList, item = 1)
      
      biasItem2 = bias(thHatList, thTrueList, item = 2)
      rmseItem2 = rmse(thHatList, thTrueList, item = 2)
      
      biasItem3 = bias(thHatList, thTrueList, item = 3)
      rmseItem3 = rmse(thHatList, thTrueList, item = 3)
      
      biasItem4 = bias(thHatList, thTrueList, item = 4)
      rmseItem4 = rmse(thHatList, thTrueList, item = 4)
      
      biasItem5 = bias(thHatList, thTrueList, item = 5)
      rmseItem5 = rmse(thHatList, thTrueList, item = 5)
      
      biasItem10 = bias(thHatList, thTrueList, item = 10)
      rmseItem10 = rmse(thHatList, thTrueList, item = 10)
      
      biasItem20 = bias(thHatList, thTrueList, item = 20)
      rmseItem20 = rmse(thHatList, thTrueList, item = 20)
      
      biasItem30 = bias(thHatList, thTrueList, item = 30)
      rmseItem30 = rmse(thHatList, thTrueList, item = 30)
      
      rangeByStatistics <- rbind(rangeByStatistics, c(
                                                      biasItem1, biasItem2, biasItem3, biasItem4, biasItem5,
                                                      biasItem10, biasItem20, biasItem30,
                                                      
                                                      rmseItem1, rmseItem2, rmseItem3, rmseItem4, rmseItem5,
                                                      rmseItem10, rmseItem20, rmseItem30,
                                                      
                                                      initValue, (initValue+step)))
      
    } #if
    
  } # for
  
  return(rangeByStatistics)
}