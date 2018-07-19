###################
#' @desc Running the ISRs using a fixed stop rule. It is necessary to calculate the statistics like BIAS and RMSE.
#'
#' @Author: @victorjatoba
#' @Email: victorjatoba[at]usp.br
#' @Organization: University of Sao Paulo (USP)
#' @Date: 2018, mai
###################

## LIBS ##

# Importing packages
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)
library(jsonlite)

if (!require('catR', lib=libDir)) install.packages("catR", lib=libDir)
library(catR)
# Using local functions
# source("answerTheItem.R")
##########

## FUNCTIONS ##

#' @description The Stop Rule lenght for each ISR
#' @references see Table 2 from Jatoba (2018) (SBIE18 paper subimmited)
#' 
#' @param isr The Item Selection Rule
#' @return boolean
stopRuleLenght <- function(isr) {
  lenght <- -1
  
  if ( isr == "MFI" ) {
    lenght <- 35
    
  } else if ( isr == "KL" ) {
    lenght <- 21
  
  } else if ( isr == "KLP" ) {
    lenght <- 24
  
  } else if ( isr == "MLWI" ) {
    lenght <- 23
  
  } else if ( isr == "MPWI" ) {
    lenght <- 18
  }
  
  return (
    lenght
  )
}
###############

## Loading parameters
enem_mat_param = read.table("./data/spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

isr <- "MFI"
# MFI = Maximum Fisher Information
# KL
# KLP
# GDI
# GDIP
# MEI
# MLWI
# MPWI
# random
# progressive
stopRuleLenght <- stopRuleLenght(isr)

# Change to Matrix
bank <- as.matrix(enem_mat_param)

## Loading responses
fileName <- "./data/spenassato-enem-responses-5k.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

## Loading data
fileName <- "./data/spenassato-enem-data-5k.txt";
connData <- file(fileName, open = "r")
linnData <- readLines(connData)

# ## Loading true thetas
fileName <- "./data/spenassato-enem-5k.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 7)
colnames(resList) <- c("UserId", "ThTrue", "ThFinal", "ItemsQttSelected", "AdministeredItemsList", "ThEstimatedList", "SeThetasList")
resList <- data.frame(resList)
# 
# resListVectors <- matrix(nrow = 0, ncol = 3)
# colnames(resListVectors) <- c("Id", "EstimatedThetas", "EstimatedSE")

groupLength <- 1

# The total of examinees that answer more than 40 items
totalOfExaminees <- 0

sumDifferenceOfThetasHatAndTrue <- 0
squareSumDifferenceOfThetasHatAndTrue <- 0

# n = 1
# 10 groups of 500 examinees responses. Total 5000 responses
for (n in 1:10) {
  
  groupResponsesN <- linn[groupLength:((groupLength-1)+500)]
  groupDataN <- linnData[groupLength:((groupLength-1)+500)+1]

  # 500 examinees responses
  # j = 1
  # j = 446
  for (j in 1:500) {
    
    # initializing vector contains the administered items
    removedItems <- matrix(nrow = 0, ncol = 1)
    administeredItems <- matrix(nrow = 0, ncol = 1)
    responsesForAdministeredItems <- matrix(nrow = 0, ncol = 1)
    seThetas <- matrix(nrow = 0, ncol = 1)
    estimatedThetas <- matrix(nrow = 0, ncol = 1)
    
    # change response line to table
    responseDataLine <- read.table(textConnection(groupResponsesN[[j]]))
    matrixResponses <- as.matrix(responseDataLine)

    if ( answerMoreThan40Items(matrixResponses) ) {
      
      totalOfExaminees <- totalOfExaminees + 1
      
      # Initializing the estimated theta
      thetaHat <- 0
      
      # storing the user ID and it true theta
      userId <- read.table(textConnection(groupDataN[[j]]))[1]
      trueTheta <- read.table(textConnection(groupDataN[[j]]))[2]
      
      # loop for fixed Stop Rule
      # i = 1
      for (i in 1:stopRuleLenght) {
        # Selecting the next item.
        itemInfo <- nextItem(bank, theta = thetaHat, out = removedItems, criterion = isr)
        
        # Getting the last item selected
        selectedItem <- itemInfo$item
        
        # Adding the last administered item to the removedItems list
        removedItems <- rbind(removedItems, c(selectedItem))
        
        if ( answerTheItem(matrixResponses[selectedItem]) ) {
          
          # Storing the selected item
          administeredItems <- rbind(administeredItems, c(selectedItem))
          
          # Storing the response of the selected item
          responsesForAdministeredItems <- rbind(responsesForAdministeredItems, c(matrixResponses[selectedItem]))
          
          # EAP estimation, standard normal prior distribution with 10 quadrature points
          # By default, it takes the vector value (-4, 4, 33), that is, 33 quadrature points on the range [-4; 4] (or, by steps of 0.25)
          thetaHat <- eapEst(it = bank[removedItems,], x = as.numeric(responsesForAdministeredItems), nqp = 10)
          estimatedThetas <- rbind(estimatedThetas, c(thetaHat))
          
          # if was collected more than 4 responses
          if ( length(responsesForAdministeredItems) >= 4 ) {
            # Getting the Standard Error from examinees_i and item_j
            seCurrent <- semTheta(thetaHat, it = bank[removedItems,], x = as.numeric(responsesForAdministeredItems),
                                  method = 'EAP', parInt = c(-4,4,10))
            
            seThetas <- rbind(seThetas, c(seCurrent))
          }
        } # if answerTheItem()
        
      } # fixed Stop Rule loop
      
      # storing the result
      # resList <- rbind(resList,
      #                  data.frame(UserId = userId,
      #                             ThTrue = trueTheta,
      #                             ThFinal = thetaHat,
      #                             AdministeredItemsList = I(list(c(administeredItems))),
      #                             ThEstimatedList = I(list(c(estimatedThetas))),
      #                             SeThetasList = I(list(c(seThetas)))
      #                  ))
      
      sumDifferenceOfThetasHatAndTrue <- sumDifferenceOfThetasHatAndTrue + (thetaHat - trueTheta)
      squareSumDifferenceOfThetasHatAndTrue <- squareSumDifferenceOfThetasHatAndTrue + (thetaHat - trueTheta)^2
    } # answer more than 40 items
    
  } #\ 500 examinees responses
  
  #incrementing group length with 500
  groupLength <- (groupLength + 500)
  
} #\ 10 groups

# BIAS <- (sum(resList$ThFinal - trueTheta)) / length(resList$ThFinal)
BIAS <- sumDifferenceOfThetasHatAndTrue / totalOfExaminees
RMSE <- sqrt( squareSumDifferenceOfThetasHatAndTrue / totalOfExaminees)

# To print by local PC
write.table(BIAS, file=paste("outs/5k_examinees/implemented_cat/2012/local/fixed_stop_rule/bias-",isr,"2.out", sep=""))
write(RMSE, file=paste("outs/5k_examinees/implemented_cat/2012/local/fixed_stop_rule/rmse-",isr,"2.json", sep=""))

# To print by Aguia HPC
#write.table(resList, row.names=FALSE, col.names=TRUE)

close(conn)
close(connData)
close(connTheta)
