#' @description Reproducing the Spenassato 2016 work with 2012 ENEM data
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev
#' @references 2016, Spenassato - Testes Adaptativos Computadorizados Aplicados em Avaliacoes Educacionais

trueThetaData = read.table("/home/victorjatoba/adapqr/data/enem.theta")
## Loading true theta estimated by ICL

ranges <- split(trueThetaData, cut(as.matrix(trueThetaData), c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5), include.lowest=TRUE))

rangeByitemsMean <- matrix(nrow = 0, ncol = 6)
colnames(rangeByitemsMean) <- c("Interval", "Lenght", "Min", "Max", "Mean", "SE")

initValue <- -2
step <- 0.5
# 10 intervals (2, 2.1, 2.2, ..., 2.9)
for (i in 1:10) {

  # the mean of the range of the selected items quantities
  interval <- paste(']', initValue, '; ', (initValue+step), ']')

  # incrementing initValue
  initValue <- (initValue+step)
  
  # the lenght of the sample in the actual interval
  sampleLenght <- length(ranges[[i]]$V1)
  
  # the lowest value from sample
  min <- min(ranges[[i]]$V1)
  
  # the biggest value from sample
  max <- max(ranges[[i]]$V1)
  
  # the mean of the range of the selected items quantities
  thetaMean <- mean(ranges[[i]]$V1)
  
  # the Standard Error from the stimated theta
  #thFinalSE <- se(ranges[[i]]$thFinal)
  
  # the Standard Error from the true theta
  trueThetaSE <- se(ranges[[i]]$V1)
  
  # storing the mean of the items selected (itemsMean)
  # and the range value (initValue and initValue+step)
  rangeByitemsMean <- rbind(rangeByitemsMean, c(interval, sampleLenght, min, max, thetaMean, trueThetaSE))
}

write.table(rangeByitemsMean, file=paste("outs/spenassato/2012-enem-tabelas2-3-analysis.out", sep=""), row.names=FALSE, col.names=TRUE)
