#' @description Plot the comparative study of the ISRs on the early stage of CAT
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018, jul

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
  

  #legend('topright',g_range[2], rules , merge = TRUE, cex=0.8, text.width = 0.8,
  #      col=c(rulesColors), lty = c(4, 2, 5, 6, 7, 15, 18), pch = c(5, 4, 25, 10, 12, 15, 20))
}
#########


g_range <- matrix()

# x
initValue <- -4
stopValue <- 3

# y
g_range[1] <- 0.5
g_range[2] <- 4


step <- 1
package <- "statistics_from_early_cat_stage"

mfiResult  <- getStatisticsByEarlyCatStage("MFI", package, initValue, stopValue, step)
klResult   <- getStatisticsByEarlyCatStage("KL", package, initValue, stopValue, step)
klpResult  <- getStatisticsByEarlyCatStage("KLP", package, initValue, stopValue, step)
mlwiResult <- getStatisticsByEarlyCatStage("MLWI", package, initValue, stopValue, step)
mpwiResult <- getStatisticsByEarlyCatStage("MPWI", package, initValue, stopValue, step)

plot(as.data.frame(mfiResult)$RmseMean1Item, type = "o",
     ylim=g_range,
     col="blue", axes=FALSE, ann=FALSE)



axis(1, cex.axis=0.7, at=1:6, lab=c("[-2, -1]", "]-1, 0]", "]0, 1]", "]1, 2]", "]2, 3]", "]3, 4]"))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, cex.axis=0.7, las=2, at=g_range[1]:g_range[2])

# Create box around plot
box()

# 1 ITEM
printLines(g_range,
            c("MFI", "KL", "KLP", "MLWI", "MPWI"),
            mfi = as.data.frame(mfiResult)$RmseMean1Item,
            kl = as.data.frame(klResult)$RmseMean1Item,
            klp = as.data.frame(klpResult)$RmseMean1Item,
            mlwi = as.data.frame(mlwiResult)$RmseMean1Item,
            mpwi = as.data.frame(mpwiResult)$RmseMean1Item
          )

# 2 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean2Item,
           kl = as.data.frame(klResult)$RmseMean2Item,
           klp = as.data.frame(klpResult)$RmseMean2Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean2Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean2Item
)

# 3 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean3Item,
           kl = as.data.frame(klResult)$RmseMean3Item,
           klp = as.data.frame(klpResult)$RmseMean3Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean3Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean3Item
)

# 4 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean4Item,
           kl = as.data.frame(klResult)$RmseMean4Item,
           klp = as.data.frame(klpResult)$RmseMean4Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean4Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean4Item
)

# 5 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean5Item,
           kl = as.data.frame(klResult)$RmseMean5Item,
           klp = as.data.frame(klpResult)$RmseMean5Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean5Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean5Item
)


# 10 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean10Item,
           kl = as.data.frame(klResult)$RmseMean10Item,
           klp = as.data.frame(klpResult)$RmseMean10Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean10Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean10Item
)


# 20 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean20Item,
           kl = as.data.frame(klResult)$RmseMean20Item,
           klp = as.data.frame(klpResult)$RmseMean20Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean20Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean20Item
)


# 30 ITEM
printLines(g_range,
           c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$RmseMean30Item,
           kl = as.data.frame(klResult)$RmseMean30Item,
           klp = as.data.frame(klpResult)$RmseMean30Item,
           mlwi = as.data.frame(mlwiResult)$RmseMean30Item,
           mpwi = as.data.frame(mpwiResult)$RmseMean30Item
)

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by the actual plots 

# title(main=paste("ISRs performance from ", thetaLevel ," thetas (th)", sep = ""), sub="[th, th[",
#       ylab="Administered items", xlab="Thetas' level range")

# PT
title(main=paste("a. item 1", sep = ""), xlab = expression(theta),
      ylab="RMSE")