#' @description Separating users by ranges 
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 jan
#' 
#' @param isr the name of the Item Selection Rule to be runned. Go to
#'              uts/isr_compair/ to see the available rules
#'              Example: "MFI", "KLP", "KL", "MPWI", etc.
#' @param initValue the initialization loop value
#' @param stopValue the stop loop criteria
#' @param step the step velocity of the loop
#' 
#' @return The intervals of the mean of the selected items quantities
getRangeByItemsMean = function(isr, package, initValue, stopValue, step) {
  isr_path <- paste("outs/",package,"/",isr,".out", sep="")
  
  ## Loading ISR results
  isr_out = read.table(isr_path, header = TRUE, sep = " ", stringsAsFactors = FALSE)
  
  rangeByitemsMean <- matrix(nrow = 0, ncol = 3)
  colnames(rangeByitemsMean) <- c("ItemsMean", "rangeV1", "rangeV2")
  
  # 10 intervals (2, 2.1, 2.2, ..., 2.9)
  for (initValue in seq(initValue, stopValue, by = step)) {
    
    # getting subset of users between initValue and initValue+step
    # thFinal is the isr_out attribute
    range <- subset(isr_out, thFinal>=initValue & thFinal<(initValue+step))
    
    # the mean of the range of the selected items quantities
    itemsMean <- mean(range$itemsQttSelected)
    
    # storing the mean of the items selected (itemsMean)
    # and the range value (initValue and initValue+step)
    rangeByitemsMean <- rbind(rangeByitemsMean, c(itemsMean, initValue, (initValue+step)))
  }
  
  return(rangeByitemsMean)
}