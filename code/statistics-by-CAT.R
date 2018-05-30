###################
#' @description Reproducing the Spenassato 2016 work with 2012 ENEM data
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev
#' @references 2016, Spenassato - Testes Adaptativos Computadorizados Aplicados em Avaliacoes Educacionais
###################

isr <- 'MFI'
isr_path <- paste("outs/5k_examinees/2012/",isr,".out", sep="")

## Loading ISR results
isr_out = read.table(isr_path, header = TRUE, sep = " ", stringsAsFactors = FALSE)

ranges <- split(isr_out, cut(isr_out$thFinal, c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5), include.lowest=TRUE))

rangeByitemsMean <- matrix(nrow = 0, ncol = 5)
colnames(rangeByitemsMean) <- c("Interval", "SampleLenght", "ItemsSelectedMean", "thFinalSE", "trueThetaSE")

initValue <- -2
step <- 0.5
# 10 intervals (2, 2.1, 2.2, ..., 2.9)
for (i in 1:10) {

  # the mean of the range of the selected items quantities
  interval <- paste(']', initValue, '; ', (initValue+step), ']')
  # incrementing initValue
  initValue <- (initValue+step)
  
  # the lenght of the sample in the actual interval
  sampleLenght <- length(ranges[[i]]$itemsQttSelected)
  
  # the mean of the range of the selected items quantities
  itemsMean <- mean(ranges[[i]]$itemsQttSelected)
  
  # the Standard Error from the stimated theta
  thFinalSE <- se(ranges[[i]]$thFinal)
  
  # the Standard Error from the true theta
  trueThetaSE <- se(ranges[[i]]$trueTheta)
  
  # storing the mean of the items selected (itemsMean)
  # and the range value (initValue and initValue+step)
  rangeByitemsMean <- rbind(rangeByitemsMean, c(interval, sampleLenght, itemsMean, thFinalSE, trueThetaSE))
}

write.table(rangeByitemsMean, file=paste("outs/5k_examinees/2012/MFI-statistics-by-intervals.out", sep=""), row.names=FALSE, col.names=TRUE)
