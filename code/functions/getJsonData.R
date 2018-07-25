###################
#' @description Get the data from a JSON file
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018, jul
#' 
#' @param isr the name of the Item Selection Rule to be runned. Go to
#'              uts/isr_compair/ to see the available rules
#'              Example: "MFI", "KLP", "KL", "MPWI", etc.
#' 
#' @return the JSON file data
###################

## LIBS ##
# Importing packages
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)
library(jsonlite)
##########

getJsonData = function(isr, package) {
  
  # mounting path
  file = paste(isr , ".", "json", sep = "")
  file = paste("data-", file, sep = "")
  isrPath <- paste("outs/",package,"/",file, sep="")
  
  isr_out = fromJSON(isrPath)
  
  return(isr_out)
}