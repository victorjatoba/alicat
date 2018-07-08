#' @description Calculate the mean of the values there are on list column
#' 
#' @param list The list of values
#' @param col the column of the list
#' @return the mean of the column values
meanOfCol <- function(list, col) {
  sum = 0
  qtd = length(list)
  for(i in 1:qtd) {
    sum = sum + list[[i]][col]
  }
  mean <- sum/qtd
  
  return (mean)
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
#' @return The intervals of the mean of the selected items quantities
###################

## LIBS ##
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)
library(jsonlite)
##########

getStatisticsByEarlyCatStage = function(isr, package, initValue, stopValue, step) {
  
  # mounting path
  file = paste(isr , "-statistics.", "json", sep = "")
  isrPath <- paste("outs/",package,"/",file, sep="")
  
  ## ISR results load
  isr_out = fromJSON(isrPath)
  
  rangeByStatisticsMean <- matrix(nrow = 0, ncol = 25)
  colnames(rangeByStatisticsMean) <- c("BiasMean1Item", "BiasMean2Item", "BiasMean3Item", "BiasMean4Item", "BiasMean5Item",
                                       "BiasMean10Item", "BiasMean20Item", "BiasMean30Item",
                                       
                                       "RmseMean1Item", "RmseMean2Item", "RmseMean3Item", "RmseMean4Item", "RmseMean5Item",
                                       "RmseMean10Item", "RmseMean20Item", "RmseMean30Item",
                                       
                                       "SeMean2Item", "SeMean3Item", "SeMean4Item", "SeMean5Item",
                                       "SeMean10Item", "SeMean20Item", "SeMean30Item",
                                       
                                       "rangeV1", "rangeV2")
  
  # 6 intervals (-2, -1, 0, ..., 4)
  for (initValue in seq(initValue, stopValue, by = step)) {
    # getting subset of users between initValue and initValue+step
    # thFinal is the isr_out attribute
    range <- subset(isr_out, ThTrue >= initValue & ThTrue < (initValue+step))

    if (length(range$BiasList) > 0) {
      list = range$BiasList
      # the BIAS mean for the first items administered
      biasMean1Item <- meanOfCol(list, col=1)
      biasMean2Item <- meanOfCol(list, col=2)
      biasMean3Item <- meanOfCol(list, col=3)
      biasMean4Item <- meanOfCol(list, col=4)
      biasMean5Item <- meanOfCol(list, col=5)
      biasMean10Item <- meanOfCol(list, col=10)
      biasMean20Item <- meanOfCol(list, col=20)
      biasMean30Item <- meanOfCol(list, col=30)

      list = range$RmseList
      # the BIAS mean for the first items administered
      rmseMean1Item <- meanOfCol(list, col=1)
      rmseMean2Item <- meanOfCol(list, col=2)
      rmseMean3Item <- meanOfCol(list, col=3)
      rmseMean4Item <- meanOfCol(list, col=4)
      rmseMean5Item <- meanOfCol(list, col=5)
      rmseMean10Item <- meanOfCol(list, col=10)
      rmseMean20Item <- meanOfCol(list, col=20)
      rmseMean30Item <- meanOfCol(list, col=30)
      
      list = range$SeThetasList
      # the BIAS mean for the first items administered
      seMean1Item <- meanOfCol(list, col=1)
      seMean2Item <- meanOfCol(list, col=2)
      seMean3Item <- meanOfCol(list, col=3)
      seMean4Item <- meanOfCol(list, col=4)
      seMean5Item <- meanOfCol(list, col=5)
      seMean10Item <- meanOfCol(list, col=10)
      seMean20Item <- meanOfCol(list, col=20)
      seMean30Item <- meanOfCol(list, col=30)
      
      # storing the mean of the items selected (itemsMean)
      # and the range value (initValue and initValue+step)
      rangeByStatisticsMean <- rbind(rangeByStatisticsMean,
                                     c(   biasMean1Item,
                                          biasMean2Item,
                                          biasMean3Item,
                                          biasMean4Item,
                                          biasMean5Item,
                                          biasMean10Item,
                                          biasMean20Item,
                                          biasMean30Item,
                                          
                                          rmseMean1Item,
                                          rmseMean2Item,
                                          rmseMean3Item,
                                          rmseMean4Item,
                                          rmseMean5Item,
                                          rmseMean10Item,
                                          rmseMean20Item,
                                          rmseMean30Item,
                                          
                                          seMean2Item,
                                          seMean3Item,
                                          seMean4Item,
                                          seMean5Item,
                                          seMean10Item,
                                          seMean20Item,
                                          seMean30Item,
                                          
                                          initValue, (initValue+step)
                                     )
      )
    }
  }
  
  return(rangeByStatisticsMean)
}