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
printLines <- function(rules, mfi, kl, klp, mlwi, mpwi, gdi, gdip) {
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
  
  return (rulesColors)
}

printBorder = function(data, g_range, item, letter) {
  plot(data, type = "o",
       ylim=g_range,
       col="blue", axes=FALSE, ann=FALSE)
  
  axis(1, cex.axis=0.7, at=1:6, lab=c("[-2, -1]", "]-1, 0]", "]0, 1]", "]1, 2]", "]2, 3]", "]3, 4]"))
  
  # Make y axis with horizontal labels that display ticks at 
  # every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
  axis(2, cex.axis=0.7, las=2, at=g_range[1]:g_range[2])
  
  # Create box around plot
  box()
  titleUpper = paste(letter, ". Item ", item, " ", sep = "")
  title(main = titleUpper  , xlab = expression(theta),
        ylab="Bias")
  
}
#########


g_range <- matrix()

# x
initValue <- -4
stopValue <- 3

# y
g_range[1] <- -3
g_range[2] <- 2


step <- 1
package <- "statistics_from_early_cat_stage"

mfiResult  <- getStatisticsByEarlyCatStage("MFI", package, initValue, stopValue, step)
klResult   <- getStatisticsByEarlyCatStage("KL", package, initValue, stopValue, step)
klpResult  <- getStatisticsByEarlyCatStage("KLP", package, initValue, stopValue, step)
mlwiResult <- getStatisticsByEarlyCatStage("MLWI", package, initValue, stopValue, step)
mpwiResult <- getStatisticsByEarlyCatStage("MPWI", package, initValue, stopValue, step)

# joint images on a figure with 4 col and 2 lines
par(mfrow = c(2,4))
par(oma = c(0,0,3,3))
par(mar=c(5,4,4,2)+0.1)


# 1 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem1, g_range, 1, 'a')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
            mfi = as.data.frame(mfiResult)$BiasItem1,
            kl = as.data.frame(klResult)$BiasItem1,
            klp = as.data.frame(klpResult)$BiasItem1,
            mlwi = as.data.frame(mlwiResult)$BiasItem1,
            mpwi = as.data.frame(mpwiResult)$BiasItem1
          )

# 2 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem2, g_range, 2, 'b')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem2,
           kl = as.data.frame(klResult)$BiasItem2,
           klp = as.data.frame(klpResult)$BiasItem2,
           mlwi = as.data.frame(mlwiResult)$BiasItem2,
           mpwi = as.data.frame(mpwiResult)$BiasItem2
          )

# 3 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem3, g_range, 3, 'c')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem3,
           kl = as.data.frame(klResult)$BiasItem3,
           klp = as.data.frame(klpResult)$BiasItem3,
           mlwi = as.data.frame(mlwiResult)$BiasItem3,
           mpwi = as.data.frame(mpwiResult)$BiasItem3
)


# 4 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem4, g_range, 4, 'd')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem4,
           kl = as.data.frame(klResult)$BiasItem4,
           klp = as.data.frame(klpResult)$BiasItem4,
           mlwi = as.data.frame(mlwiResult)$BiasItem4,
           mpwi = as.data.frame(mpwiResult)$BiasItem4)

# 5 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem5, g_range, 5, 'e')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem5,
           kl = as.data.frame(klResult)$BiasItem5,
           klp = as.data.frame(klpResult)$BiasItem5,
           mlwi = as.data.frame(mlwiResult)$BiasItem5,
           mpwi = as.data.frame(mpwiResult)$BiasItem5
          )


# 10 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem10, g_range, 10, 'f')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem10,
           kl = as.data.frame(klResult)$BiasItem10,
           klp = as.data.frame(klpResult)$BiasItem10,
           mlwi = as.data.frame(mlwiResult)$BiasItem10,
           mpwi = as.data.frame(mpwiResult)$BiasItem10
          )


# 20 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem20, g_range, 20, 'g')
printLines(c("MFI", "KL", "KLP", "MLWI", "MPWI"),
           mfi = as.data.frame(mfiResult)$BiasItem20,
           kl = as.data.frame(klResult)$BiasItem20,
           klp = as.data.frame(klpResult)$BiasItem20,
           mlwi = as.data.frame(mlwiResult)$BiasItem20,
           mpwi = as.data.frame(mpwiResult)$BiasItem20
          )


# 30 ITEM
printBorder(data = as.data.frame(mfiResult)$BiasItem30, g_range, 30, 'h')
rules = c("MFI", "KL", "KLP", "MLWI", "MPWI")
rulesColors = 
  printLines(rules,
           mfi = as.data.frame(mfiResult)$BiasItem30,
           kl = as.data.frame(klResult)$BiasItem30,
           klp = as.data.frame(klpResult)$BiasItem30,
           mlwi = as.data.frame(mlwiResult)$BiasItem30,
           mpwi = as.data.frame(mpwiResult)$BiasItem30
          )
legend('bottomleft',g_range[2], rules , merge = TRUE, cex=0.9, text.width = 1.5,
       col=c(rulesColors), lty = c(4, 2, 5, 6, 7, 15, 18), pch = c(5, 4, 25, 10, 12, 15, 20))


title(main=paste("Resultado dos Bias x thetas estimados para os itens de 1 a 30 ", sep = ""), xlab = expression(theta),
      ylab="Bias", outer = TRUE)


# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by the actual plots 

# title(main=paste("ISRs performance from ", thetaLevel ," thetas (th)", sep = ""), sub="[th, th[",
#       ylab="Administered items", xlab="Thetas' level range")
