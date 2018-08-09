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
printLines <- function(rules, mfi, kl, klp, mlwi, mpwi, gdi, gdip, ourcat) {
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
#      else if (rules[i] == "CAT proposto") {
#      lines(ourcat, type="o", pch=20, lty=18, col="black")
#      rulesColors <- cbind(rulesColors, c("black"))
#    }
  }
  
  return (rulesColors)
}

#' @description Plot the border and x, y title
#' 
#' @param data The statistics
#' @param g_range the length theta range
#' @param item the item-th
#' @param letter the letter title sequence that will be plot
#' 
printBorder = function(data, g_range, item, letter, ylab='bias') {
  plot(data, type = "o",
       ylim=g_range,
       col="blue", axes=FALSE, ann=FALSE)
  
  axis(1, cex.axis=0.7, at=1:5, lab=c("[-2, -1]", "]-1, 0]", "]0, 1]", "]1, 2]", "]2, 3.5]"))
                                      #-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5
  
  # Make y axis with horizontal labels that display ticks at 
  # every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
  axis(2, cex.axis=0.7, las=2, at=g_range[1]:g_range[2])
  
  # Create box around plot
  box()
  titleUpper = paste(letter, ". Item ", item, " ", sep = "")
  title(main = titleUpper  , xlab = expression(theta),
        ylab=ylab)
  
}

#' @description Plot all graphics of the BIAS results
#' 
#' @param data The statistics
#' @param g_range the length theta range
plotBias = function(g_range, isrsTitle, mfiResult, klResult, klpResult, mlwiResult, mpwiResult, ourcatResult) {
  # 1 ITEM
  printBorder(data = mfiResult$BiasItem1, g_range, 1, 'a')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem1,
             kl = klResult$BiasItem1,
             klp = klpResult$BiasItem1,
             mlwi = mlwiResult$BiasItem1,
             mpwi = mpwiResult$BiasItem1,
             ourcat = ourcatResult$BiasItem1
  )
  
  # 2 ITEM
  printBorder(data = mfiResult$BiasItem2, g_range, 2, 'b')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem2,
             kl = klResult$BiasItem2,
             klp = klpResult$BiasItem2,
             mlwi = mlwiResult$BiasItem2,
             mpwi = mpwiResult$BiasItem2,
             ourcat = ourcatResult$BiasItem2
  )
  
  # 3 ITEM
  printBorder(data = mfiResult$BiasItem3, g_range, 3, 'c')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem3,
             kl = klResult$BiasItem3,
             klp = klpResult$BiasItem3,
             mlwi = mlwiResult$BiasItem3,
             mpwi = mpwiResult$BiasItem3,
             ourcat = ourcatResult$BiasItem3
  )
  
  
  # 4 ITEM
  printBorder(data = mfiResult$BiasItem4, g_range, 4, 'd')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem4,
             kl = klResult$BiasItem4,
             klp = klpResult$BiasItem4,
             mlwi = mlwiResult$BiasItem4,
             mpwi = mpwiResult$BiasItem4,
             ourcat = ourcatResult$BiasItem4)
  
  # 5 ITEM
  printBorder(data = mfiResult$BiasItem5, g_range, 5, 'e')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem5,
             kl = klResult$BiasItem5,
             klp = klpResult$BiasItem5,
             mlwi = mlwiResult$BiasItem5,
             mpwi = mpwiResult$BiasItem5,
             ourcat = ourcatResult$BiasItem5
  )
  
  
  # 10 ITEM
  printBorder(data = mfiResult$BiasItem10, g_range, 10, 'f')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem10,
             kl = klResult$BiasItem10,
             klp = klpResult$BiasItem10,
             mlwi = mlwiResult$BiasItem10,
             mpwi = mpwiResult$BiasItem10,
             ourcat = ourcatResult$BiasItem10
  )
  
  
  # 20 ITEM
  printBorder(data = mfiResult$BiasItem20, g_range, 20, 'g')
  printLines(isrsTitle,
             mfi = mfiResult$BiasItem20,
             kl = klResult$BiasItem20,
             klp = klpResult$BiasItem20,
             mlwi = mlwiResult$BiasItem20,
             mpwi = mpwiResult$BiasItem20,
             ourcat = ourcatResult$BiasItem20
  )
  
  
  # 30 ITEM
  printBorder(data = mfiResult$BiasItem30, g_range, 30, 'h')
  rules = isrsTitle
  rulesColors = 
    printLines(rules,
               mfi = mfiResult$BiasItem30,
               kl = klResult$BiasItem30,
               klp = klpResult$BiasItem30,
               mlwi = mlwiResult$BiasItem30,
               mpwi = mpwiResult$BiasItem30,
               ourcat = ourcatResult$BiasItem30
    )
  legend('topleft',g_range[2], rules , merge = TRUE, cex=1.4, text.width = 1.7,
         col=c(rulesColors), lty = c(4, 2, 5, 6, 7, 15, 18), pch = c(5, 4, 25, 10, 12, 15, 20))
  
  
  title(main=paste("Resultado dos Bias x escores estimados para os itens de 1 a 30 ", sep = ""), xlab = expression(theta),
        ylab="Bias", outer = TRUE)
  
}

#' @description Plot all graphics of the RMSE results
#' 
#' @param data The statistics
#' @param g_range the length theta range
plotRmse = function(g_range, isrsTitle, mfiResult, klResult, klpResult, mlwiResult, mpwiResult, ourcatResult) {
  # 1 ITEM
  printBorder(data = mfiResult$RmseItem1, g_range, 1, 'a', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem1,
             kl = klResult$RmseItem1,
             klp = klpResult$RmseItem1,
             mlwi = mlwiResult$RmseItem1,
             mpwi = mpwiResult$RmseItem1,
             ourcat = ourcatResult$RmseItem1
  )
  
  # 2 ITEM
  printBorder(data = mfiResult$RmseItem2, g_range, 2, 'b', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem2,
             kl = klResult$RmseItem2,
             klp = klpResult$RmseItem2,
             mlwi = mlwiResult$RmseItem2,
             mpwi = mpwiResult$RmseItem2,
             ourcat = ourcatResult$RmseItem2
  )
  
  # 3 ITEM
  printBorder(data = mfiResult$RmseItem3, g_range, 3, 'c', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem3,
             kl = klResult$RmseItem3,
             klp = klpResult$RmseItem3,
             mlwi = mlwiResult$RmseItem3,
             mpwi = mpwiResult$RmseItem3,
             ourcat = ourcatResult$RmseItem3
  )
  
  
  # 4 ITEM
  printBorder(data = mfiResult$RmseItem4, g_range, 4, 'd', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem4,
             kl = klResult$RmseItem4,
             klp = klpResult$RmseItem4,
             mlwi = mlwiResult$RmseItem4,
             mpwi = mpwiResult$RmseItem4,
             ourcat = ourcatResult$RmseItem4
             )
  
  # 5 ITEM
  printBorder(data = mfiResult$RmseItem5, g_range, 5, 'e', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem5,
             kl = klResult$RmseItem5,
             klp = klpResult$RmseItem5,
             mlwi = mlwiResult$RmseItem5,
             mpwi = mpwiResult$RmseItem5,
             ourcat = ourcatResult$RmseItem5
  )
  
  
  # 10 ITEM
  printBorder(data = mfiResult$RmseItem10, g_range, 10, 'f', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem10,
             kl = klResult$RmseItem10,
             klp = klpResult$RmseItem10,
             mlwi = mlwiResult$RmseItem10,
             mpwi = mpwiResult$RmseItem10,
             ourcat = ourcatResult$RmseItem10
  )
  
  
  # 20 ITEM
  printBorder(data = mfiResult$RmseItem20, g_range, 20, 'g', 'rmse')
  printLines(isrsTitle,
             mfi = mfiResult$RmseItem20,
             kl = klResult$RmseItem20,
             klp = klpResult$RmseItem20,
             mlwi = mlwiResult$RmseItem20,
             mpwi = mpwiResult$RmseItem20,
             ourcat = ourcatResult$RmseItem20
  )
  
  
  # 30 ITEM
  printBorder(data = mfiResult$RmseItem30, g_range, 30, 'h', 'rmse')
  rules = isrsTitle
  rulesColors = 
    printLines(rules,
               mfi = mfiResult$RmseItem30,
               kl = klResult$RmseItem30,
               klp = klpResult$RmseItem30,
               mlwi = mlwiResult$RmseItem30,
               mpwi = mpwiResult$RmseItem30,
               ourcat = ourcatResult$RmseItem30
    )
  legend('topleft',g_range[2], rules , merge = TRUE, cex=1.4, text.width = 1.7,
         col=c(rulesColors), lty = c(4, 2, 5, 6, 7, 15, 18), pch = c(5, 4, 25, 10, 12, 15, 20))
  
  
  title(main=paste("Resultado dos RMSE x escores estimados para os itens de 1 a 30 ", sep = ""), xlab = expression(theta),
        ylab="RMSE", outer = TRUE)
  
}
#########

# x
initValue <- -2
step <- 1

package <- "statistics_from_early_cat_stage"
mfiResult  <- getStatisticsByEarlyCatStage("MFI", package, initValue, step)
klResult   <- getStatisticsByEarlyCatStage("KL", package, initValue, step)
klpResult  <- getStatisticsByEarlyCatStage("KLP", package, initValue, step)
mlwiResult <- getStatisticsByEarlyCatStage("MLWI", package, initValue, step)
mpwiResult <- getStatisticsByEarlyCatStage("MPWI", package, initValue, step)
ourcatResult <- ''
  #getStatisticsByEarlyCatStage("OURCAT", package, initValue, step)

g_range <- matrix()
# y
g_range[2] <- 2.5
g_range[1] <- -2

# joint images on a figure with 4 col and 2 lines
par(mfrow = c(2,4))
par(oma = c(0,0,3,0))
par(mar=c(5,4,4,2)+0.1)

#plotBias(g_range, c("MFI", "KL", "KLP", "MLWI", "MPWI", "CAT proposto"), 
plotBias(g_range, c("MFI", "KL", "KLP", "MLWI", "MPWI"), 
         as.data.frame(mfiResult), 
         as.data.frame(klResult), 
         as.data.frame(klpResult), 
         as.data.frame(mlwiResult), 
         as.data.frame(mpwiResult)
         )

#plotRmse(g_range, c("MFI", "KL", "KLP", "MLWI", "MPWI", "CAT proposto"),

#plotRmse(g_range, c("MFI", "KL", "KLP", "MLWI", "MPWI"),
#        as.data.frame(mfiResult), 
#        as.data.frame(klResult), 
#        as.data.frame(klpResult), 
#        as.data.frame(mlwiResult), 
#        as.data.frame(mpwiResult)
#        )