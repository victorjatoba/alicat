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

getStatisticsByEarlyCatStage = function(isr, package, initValue, step) {
  
  file = paste(isr , "-early-cat-stage-results.", "json", sep = "")
  isrPath <- paste("outs/",package,"/",file, sep="")
  
  ## ISR results load
  isr_out = fromJSON(isrPath)
  
  # spliting the isr_out in 10 groups between -2 and 3.5
  ranges <- split(isr_out, cut(as.matrix(isr_out$ThTrue), c(-2, -1, 0, 1, 2, 3.5), include.lowest=TRUE))
  
  rangeByStatistics <- matrix(nrow = 0, ncol = 18)
  colnames(rangeByStatistics) <- c("BiasItem1", "BiasItem2", "BiasItem3", "BiasItem4", "BiasItem5",
                                   "BiasItem10", "BiasItem20", "BiasItem30",
                                   
                                   "RmseItem1", "RmseItem2", "RmseItem3", "RmseItem4", "RmseItem5",
                                   "RmseItem10", "RmseItem20", "RmseItem30",
                                   
                                   "rangeV1", "rangeV2")

  # 5 groups (-2, -1, 0, 1, 2, 3.5)
  for (i in 1:5) {
    
    # getting the group containing examinees id and theta
    range <- as.data.frame(ranges[[i]])
    
    thHatList = range$EstimatedThetas
    thTrueList = range$ThTrue
    if (length(thHatList) > 0) {
      biasItem1 = biasOfItem(thHatList, thTrueList, item = 1)
      rmseItem1 = rmse(thHatList, thTrueList, item = 1)
      
      biasItem2 = biasOfItem(thHatList, thTrueList, item = 2)
      rmseItem2 = rmse(thHatList, thTrueList, item = 2)
      
      biasItem3 = biasOfItem(thHatList, thTrueList, item = 3)
      rmseItem3 = rmse(thHatList, thTrueList, item = 3)
      
      biasItem4 = biasOfItem(thHatList, thTrueList, item = 4)
      rmseItem4 = rmse(thHatList, thTrueList, item = 4)
      
      biasItem5 = biasOfItem(thHatList, thTrueList, item = 5)
      rmseItem5 = rmse(thHatList, thTrueList, item = 5)
      
      biasItem10 = biasOfItem(thHatList, thTrueList, item = 10)
      rmseItem10 = rmse(thHatList, thTrueList, item = 10)
      
      biasItem20 = biasOfItem(thHatList, thTrueList, item = 20)
      rmseItem20 = rmse(thHatList, thTrueList, item = 20)
      
      biasItem30 = biasOfItem(thHatList, thTrueList, item = 30)
      rmseItem30 = rmse(thHatList, thTrueList, item = 30)
      
      rangeByStatistics <- rbind(rangeByStatistics, c(
                                                      biasItem1, biasItem2, biasItem3, biasItem4, biasItem5,
                                                      biasItem10, biasItem20, biasItem30,
                                                      
                                                      rmseItem1, rmseItem2, rmseItem3, rmseItem4, rmseItem5,
                                                      rmseItem10, rmseItem20, rmseItem30,
                                                      
                                                      initValue, (initValue+step)))
      
    } #if
    
    initValue = initValue + step
  } # for
  
  return(rangeByStatistics)
}