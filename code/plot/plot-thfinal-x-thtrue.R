#' @description Plot the comparative between the estimated theta and true theta
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @references Figure 5, Spenassato (2016)
#' @date 2018, jul

## FUNCTIONS ##
plotIsr = function(isr, package) {
  isr_out = getJsonData(isr, package)
  plot(x = isr_out$ThTrue, y = isr_out$ThFinal, col = "blue",
       main = isr, ylab = "", xlab = "" )
  mtext(expression(paste(theta)),side=2,las=1,line=2.5)
  mtext(expression(paste(hat(theta))),side=1,las=1,line=2.5)
}
#########

# x
x_upper <- 3.5
x_lower <- -2

g_range <- matrix()
# y
g_range[2] <- 4
g_range[1] <- -2

# joint images on a figure with 4 col and 2 lines
par(mfrow = c(2,3))
par(oma = c(0,0,3,0))
par(mar=c(5,4,4,2)+0.1)

package = "fixed_stop_rule"
plotIsr("OURCAT-fixed-stop-rule", package)
plotIsr("MFI-fixed-stop-rule", package)
plotIsr("KL-fixed-stop-rule", package)
plotIsr("KLP-fixed-stop-rule", package)
plotIsr("MLWI-fixed-stop-rule", package)
plotIsr("MPWI-fixed-stop-rule", package)
