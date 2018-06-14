#' @description Ploting the result of ISR comparative study
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev

## FUNCTIONS ##

#' @description Calculate the SE thetas mean
#' 
#' @param seFinalList The list of SE thetas
#' @return number
printLines <- function(g_range, rules, mfi, kl, klp, mlwi, mpwi, gdi, gdip) {
  rulesColors <- matrix(nrow = 1, ncol = 0)
  for( i in 1:length(rules) ) {
    if (rules[i] == "MFI") {
      lines(mfi, type="o", pch=5, lty=4, col="blue")
      rulesColors <- cbind(rulesColors, c("blue"))
    } else if (rules[i] == "KL") {
      lines(kl, type="o", pch=4, lty=2, col="green")
      rulesColors <- cbind(rulesColors, c("green"))
      
    } else if (rules[i] == "KLP") {
      lines(klp, type="o", pch=25, lty=5, col="darkgreen")
      rulesColors <- cbind(rulesColors, c("darkgreen"))
      
    } else if (rules[i] == "MLWI") {
      lines(mlwi, type="o", pch=10, lty=6, col="purple")
      rulesColors <- cbind(rulesColors, c("purple"))
      
    } else if (rules[i] == "MPWI") {
      lines(mpwi, type="o", pch=12, lty=7, col="brown")
      rulesColors <- cbind(rulesColors, c("brown"))

    } else if (rules[i] == "GDI") {
      lines(gdi, type="o", pch=15, lty=15, col="red")
      rulesColors <- cbind(rulesColors, c("red"))
      
    } else if (rules[i] == "GDIP") {
      lines(gdip, type="o", pch=20, lty=18, col="orange")
      rulesColors <- cbind(rulesColors, c("orange"))
    }
  }
  
  #lines(randomResult, type="o", pch=23, lty=3, col="black")
  #lines(random2Result, type="o", pch=23, lty=3, col="darkblue")
  #lines(progressiveResult, type="o", pch=33, lty=13, col="gray")
  
  # legend('topleft',g_range[2], rules , cex=0.25, text.width = 1,
  #       col=c(rulesColors), pch=11:12, lty=1:2)

  legend('topleft',g_range[2], rules , text.width = 2,
        col=c(rulesColors), lty = c(4, 2, 5, 6, 7, 15, 18), pch = c(5, 4, 25, 10, 12, 15, 20))
  # legend('topleft',g_range[2], rules, col = c(rulesColors),
  #         lty = c(2, -1, 1), pch = c(NA, 3, 4),
  #         merge = TRUE, text.width = 2)
}
#########


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
format = "json"

mfiResult  <- getRangeByItemsMean("MFI", package, initValue, stopValue, step, format)[,1]
klResult   <- getRangeByItemsMean("KL", package, initValue, stopValue, step, format)[,1]
#meiResult  <- getRangeByItemsMean("MEI", package, initValue, stopValue, step, format)[,1]
klpResult  <- getRangeByItemsMean("KLP", package, initValue, stopValue, step, format)[,1]
mlwiResult <- getRangeByItemsMean("MLWI", package, initValue, stopValue, step, format)[,1]
mpwiResult <- getRangeByItemsMean("MPWI", package, initValue, stopValue, step, format)[,1]
#gdiResult <- getRangeByItemsMean("GDI", package, initValue, stopValue, step, format)[,1]
#gdipResult <- getRangeByItemsMean("GDIP", package, initValue, stopValue, step, format)[,1]

#randomResult <- getRangeByItemsMean("random", package, initValue, stopValue, step, format)[,1]
#random2Result <- getRangeByItemsMean("random2", package, initValue, stopValue, step, format)[,1]
#progressiveResult <- getRangeByItemsMean("progressive", package, initValue, stopValue, step, format)[,1]

plot(mfiResult, type = "o",
     ylim=g_range,
     col="blue", axes=FALSE, ann=FALSE)


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


printLines(g_range,
            c("MFI", "KL", "KLP", "MPWI", "MLWI"),
            mfi = mfiResult,
            kl = klResult,
            klp = klpResult,
            mlwi = mlwiResult,
            mpwi = mpwiResult
          )

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by the actual plots 

# title(main=paste("ISRs performance from ", thetaLevel ," thetas (th)", sep = ""), sub="[th, th[",
#       ylab="Administered items", xlab="Thetas' level range")

# PT
title(main=paste("Desempenho das Regras de Seleção de Itens", sep = ""), sub= expression(paste("[ ", hat(theta), ", ", hat(theta)," [" )),
      ylab="Quantide de itens selecionados",
      # xlab="Grupos de thetas estimados",
      expression(paste("Grupos de thetas estimados (", hat(theta), ")")))