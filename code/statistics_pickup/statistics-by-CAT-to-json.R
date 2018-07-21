###################
#' @description Reproducing the Spenassato 2016 work with 2012 ENEM data using JSON file
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018, Jun
#' @references 2016, Spenassato - Testes Adaptativos Computadorizados Aplicados em Avaliacoes Educacionais
###################

## LIBS ##
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)
library('jsonlite')
##########

## FUNCTIONS ##

#' @description Calculate the SE thetas mean
#' 
#' @param seFinalList The list of SE thetas
#' @return number
calculateSeThetasMean <- function(seThetasList) {
  seFinalList <- matrix(nrow = 0, ncol = 1)
  n = length(seThetasList);
  if (n > 0) {
    for(j in 1:n) {
      seFinalList <- rbind(seFinalList, seThetasList[[j]][length(seThetasList[[j]])]) 
    }
  }
  
  seThetasMean <- mean(seFinalList)
  return (
    seThetasMean
  )
}
#########

isr <- 'ourcat'
path <- paste("outs/stability_point/")
isrPath <- paste(path,"data-",isr,".json", sep="")

## Loading ISR results
#isr_out = read.table(isrPath, header = TRUE, sep = " ", stringsAsFactors = FALSE)
isr_out = fromJSON(isrPath)

ranges <- split(isr_out, cut(isr_out$ThFinal, c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5), include.lowest=TRUE))

rangeByitemsMean <- matrix(nrow = 0, ncol = 6)
colnames(rangeByitemsMean) <- c("Interval", "SampleLenght", "ItemsSelectedMean", "ThFinalMean", "ThTrueMean", "SeThetasMean")

initValue <- -2
step <- 0.5
# i = 1
# 10 intervals (2, 2.1, 2.2, ..., 2.9)
for (i in 1:10) {

  # the mean of the range of the selected items quantities
  interval <- paste(']', initValue, '; ', (initValue+step), ']')
  # incrementing initValue
  initValue <- (initValue+step)
  
  # the lenght of the sample in the actual interval
  sampleLenght <- length(ranges[[i]]$ItemsQttSelected)
  
  # the mean of the range of the selected items quantities
  itemsMean <- mean(ranges[[i]]$ItemsQttSelected)
  # the mean of the range of the estimated thetas
  thFinalMean <- mean(ranges[[i]]$ThFinal)
  # the mean of the range of the true thetas
  thTrueMean <- mean(ranges[[i]]$ThTrue)
  
  # the mean of the range of the true SE thetas
  seThetasList <- ranges[[i]]$SeThetasList
  seThetasMean <- calculateSeThetasMean(seThetasList)
  
  # storing the mean of the items selected (itemsMean)
  # and the range value (initValue and initValue+step)
  rangeByitemsMean <- rbind(rangeByitemsMean, c(interval, sampleLenght, itemsMean, thFinalMean, thTrueMean, seThetasMean))

}

jsonFile = toJSON(data.frame(rangeByitemsMean), pretty=T)
write(jsonFile, file=paste(path,isr,"-statistics-by-intervals.json", sep=""))
write.table(rangeByitemsMean, file=paste(path,isr,"-statistics-by-intervals.out", sep=""), row.names=FALSE, col.names=TRUE)
