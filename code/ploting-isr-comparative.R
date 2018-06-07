#' @description Ploting the result of ISR comparative study
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev


#choice the theta level range you want to plot
#thetaLevel <- "high"
#thetaLevel <- "medium"
#thetaLevel <- "low"
thetaLevel <- "all"
g_range <- matrix()

if (thetaLevel == "high") {
  # Range from (initValue) to (stopValue)
  initValue <- 1.5
  stopValue <- 3
  
  # Calculate range from 0 to max value of the all rules
  g_range[1] <- 6
  g_range[2] <- 18
  
} else if (thetaLevel == "medium") {
  initValue <- 0.1
  stopValue <- 1.5
  
  g_range[1] <- 12
  g_range[2] <- 45
} else if (thetaLevel == "low") {
  initValue <- -1.7
  stopValue <- 0.1
  
  g_range[1] <- 44
  g_range[2] <- 46
} else {
  initValue <- -1.7
  stopValue <- 3
  
  g_range[1] <- 0
  g_range[2] <- 45
}


step <- 0.1
package <- "5k_examinees/implemented_cat/2012/local"
mfiResult  <- getRangeByItemsMean("MFI", package, initValue, stopValue, step)[,1]
klResult   <- getRangeByItemsMean("KL", package, initValue, stopValue, step)[,1]
#meiResult  <- getRangeByItemsMean("MEI", package, initValue, stopValue, step)[,1]
klpResult  <- getRangeByItemsMean("KLP", package, initValue, stopValue, step)[,1]
mlwiResult <- getRangeByItemsMean("MLWI", package, initValue, stopValue, step)[,1]
mpwiResult <- getRangeByItemsMean("MPWI", package, initValue, stopValue, step)[,1]
gdiResult <- getRangeByItemsMean("GDI", package, initValue, stopValue, step)[,1]
gdipResult <- getRangeByItemsMean("GDIP", package, initValue, stopValue, step)[,1]

randomResult <- getRangeByItemsMean("random", package, initValue, stopValue, step)[,1]
random2Result <- getRangeByItemsMean("random2", package, initValue, stopValue, step)[,1]
progressiveResult <- getRangeByItemsMean("progressive", package, initValue, stopValue, step)[,1]

plot(mfiResult, type = "o",
     ylim=g_range,
     col="Blue", axes=FALSE, ann=FALSE)


if (thetaLevel == "high") {
  axis(1, cex.axis=0.7, at=1:15, lab=c("1.5, 1.6", "1.6, 1.7", "1.7, 1.8", "1.8, 1.9", "1.9, 2",
                         "2, 2.1", "2.1, 2.2", "2.2, 2.3", "2.3, 2.4", "2.4, 2.5",
                         "2.5, 2.6", "2.6, 2.7", "2.7, 2.8", "2.8, 2.9", "2.9, 3"))
  
} else if (thetaLevel == "medium") {
  axis(1, cex.axis=0.7, at=1:15, lab=c("0.1, 0.2", "0.2, 0.3", "0.3, 0.4", "0.4, 0.5", "0.5, 0.6",
                         "0.6, 0.7", "0.7, 0.8", "0.8, 0.9", "0.9, 1", "1, 1.1",
                         "1.1, 1.2", "1.2, 1.3", "1.3, 1.4", "1.4, 1.5", "1.5, 1.6"))

} else if (thetaLevel == "low") {
  axis(1, cex.axis=0.7, at=1:19, lab=c("-1.7, -1.6", "-1.6, -1.5", "-1.5, -1.4", "-1.4, -1.3", "-1.3, -1.2",
                         "-1.2, -1.1", "-1.1, -1", "-1, -0.9", "-0.9, -0.8", "-0.8, -0.7",
                         "-0.7, -0.6", "-0.6, -0.5", "-0.5, -0.4", "-0.4, -0.3", "-0.3, -0.2",
                         "-0.2, -0.1", "-0.1, 0", "0, 0.1", "0.1, 0.2"))
  #axis(1, cex.axis=0.8, at=1:18, lab)
} else {
  axis(1, cex.axis=0.7, at=1:47, lab=c("-1.7, -1.6", "-1.6, -1.5", "-1.5, -1.4", "-1.4, -1.3", "-1.3, -1.2",
                                       "-1.2, -1.1", "-1.1, -1", "-1, -0.9", "-0.9, -0.8", "-0.8, -0.7",
                                       "-0.7, -0.6", "-0.6, -0.5", "-0.5, -0.4", "-0.4, -0.3", "-0.3, -0.2",
                                       "-0.2, -0.1", "-0.1, 0", "0, 0.1",
                                       
                                       "0.1, 0.2", "0.2, 0.3", "0.3, 0.4", "0.4, 0.5", "0.5, 0.6",
                                       "0.6, 0.7", "0.7, 0.8", "0.8, 0.9", "0.9, 1", "1, 1.1",
                                       "1.1, 1.2", "1.2, 1.3", "1.3, 1.4", "1.4, 1.5", "1.5, 1.6",
                                       
                                       "1.6, 1.7", "1.7, 1.8", "1.8, 1.9", "1.9, 2",
                                       "2, 2.1", "2.1, 2.2", "2.2, 2.3", "2.3, 2.4", "2.4, 2.5",
                                       "2.5, 2.6", "2.6, 2.7", "2.7, 2.8", "2.8, 2.9", "2.9, 3"))
}

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, cex.axis=0.7, las=2, at=1*0:(g_range[2]))

# Create box around plot
box()

lines(mfiOldResult, type="o", pch=5, lty=4, col="yellow")

# KL Graphic with red dashed line and square points
lines(klResult, type="o", pch=4, lty=2, col="green")
# MEI Graphic with green dashed line and square points
#lines(meiResult, type="o", pch=5, lty=4, col="darkgreen")
# KLP Graphic with green dashed line and square points
lines(klpResult, type="o", pch=25, lty=5, col="darkgreen")
# MLWI Graphic with purple dashed line and square points
lines(mlwiResult, type="o", pch=10, lty=6, col="purple")
# MPWI Graphic with purple dashed line and square points
lines(mpwiResult, type="o", pch=12, lty=7, col="pink")
# GDI Graphic with purple dashed line and square points
lines(gdiResult, type="o", pch=15, lty=15, col="red")
# GDIP Graphic with purple dashed line and square points
lines(gdipResult, type="o", pch=20, lty=18, col="orange")

# Random Graphic with purple dashed line and square points
lines(randomResult, type="o", pch=23, lty=3, col="blue")
# Random2 Graphic with purple dashed line and square points
lines(random2Result, type="o", pch=23, lty=3, col="darkblue")
# GDIP Graphic with purple dashed line and square points
lines(progressiveResult, type="o", pch=33, lty=13, col="gray")

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by the actual plots 

#legend('bottomleft',g_range[2], c("MFI", "KL", "KLP", "MPWI",  "GDI", "GDIP"), cex=0.3, 
#       col=c("blue", "green", "darkgreen", "purple", "red", "orange"), pch=11:12, lty=1:2)

legend('topleft',g_range[2], c("MFI", "KL", "KLP", "MPWI", "MLWI", "GDI", "GDIP", "Random", "Random2", "Progres.."), cex=0.18, 
       col=c("yellow","green", "darkgreen", "purple", "pink", "red", "orange", "blue", "darkblue", "gray"), pch=11:12, lty=1:2)

title(main=paste("ISRs performance from ", thetaLevel ," thetas (th)", sep = ""), sub="[th, th[",
      ylab="Quantities of selected items", xlab="Thetas' level range") 
