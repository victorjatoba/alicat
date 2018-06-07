###################
#' @desc Storing the CAT execution results from implemented CAT.
#'
#' @Author: @victorjatoba
#' @Email: victorjatoba[at]usp.br
#' @Organization: University of Sao Paulo (USP)
#' @Date: Mai, 2018
###################

#if (!require('catR', lib="./../../R/library")) install.packages("catR", lib="./../../R/library")
#.libPaths()
#require('catR', lib="./../../R/library")

# Use catR package
library('catR')

## FUNCTIONS ##

#' @description Verify if user anser equals or more than 40 items
#' 
#' @param x The array of responses
#' @return boolean
answerMoreThan40Items <- function(x) {
  return (
    sum(x == '.') <= 5 && sum(x == "NA") <= 5
  )
}

#' @description Verify if exist a valid response
#' 
#' @param response The question answered by user
#' @return boolean
answerTheItem <- function(response) {
  return (
    response == "0" || response == "1"
  )
}

#' @description Verify if the CAT reached the estability point.
#' @references see Section 5.2 from Spenassato, 2016
#' 
#' @param percent The percent value of the estability point to be verify
#' @param seCurrent The actual Standard Error
#' @param sePrev The previous Standard Error
#' @return boolean
reachedTheEstabilityPoint <- function(percent, sePrev, seCurrent) {
  return (
    abs(seCurrent - sePrev) < abs(percent * sePrev)
  )
}
################

# Use catR package
library('catR')


## Loading parameters
enem_mat_param = read.table("./data/spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

isr <- "random"
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

# Change to Matrix
bank <- as.matrix(enem_mat_param)

## Loading responses
fileName <- "./data/spenassato-enem-responses-5k.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

# ## Loading true thetas
fileName <- "./data/spenassato-enem-5k.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 5)
colnames(resList) <- c("Rule", "itemsQttSelected", "thFinal", "seFinal")

groupLength <- 1

# n = 1
# 10 groups of 500 examinees responses. Total 5000 responses
for (n in 1:10) {
  
  groupResponsesN <- linn[groupLength:((groupLength-1)+500)]

  # 500 examinees responses
  # j = 447
  for (j in 1:500) {
    
    # initializing vector contains the administered items
    removedItems <- matrix(nrow = 0, ncol = 1)
    responsesForAdministeredItems <- matrix(nrow = 0, ncol = 1)
    seThetas <- matrix(nrow = 0, ncol = 1)
    
    # change response line to table
    responseDataLine <- read.table(textConnection(groupResponsesN[[j]]))
    matrixResponses <- as.matrix(responseDataLine)
    
    ## FIRST ITEM
    # Selecting 1 starting item, initial ability estimate is 0
    #itemSelected <- startItems(bank)
    ## Selecting 4 starting items for ability levels between -2 and 2
    # startItems(bank, randomesque = 4, theta = c(-2, 2))
    
    # Initializing the estimated theta
    thetaCurrent <- 0
    
    # To identify the estibility point of the CAT. See Equation 2 from Spenassato (2016)
    finalItemsQttSelected <- 0

    if ( answerMoreThan40Items(matrixResponses) ) {
      
      i <- 1
      # loop on 45 items' responses
      while (i <= 45) {
        # Selecting the next item.
        itemInfo <- nextItem(bank, theta = thetaCurrent, out = removedItems, criterion = isr)
        
        # Getting the last item selected
        selectedItem <- itemInfo$item
        
        # Adding the last administered item to the removedItems list
        removedItems <- rbind(removedItems, c(selectedItem))
        
        if ( answerTheItem(matrixResponses[selectedItem]) ) {
          
          # Storing the response of the selected item
          responsesForAdministeredItems <- rbind(responsesForAdministeredItems, c(matrixResponses[selectedItem]))
          
          # EAP estimation, standard normal prior distribution with 10 quadrature points
          # By default, it takes the vector value (-4, 4, 33), that is, 33 quadrature points on the range [-4; 4] (or, by steps of 0.25)
          thetaCurrent <- eapEst(it = bank[removedItems,], x = as.numeric(responsesForAdministeredItems), nqp = 10)
          
          # if was collected more than 4 responses
          if ( length(responsesForAdministeredItems) >= 4 ) {
            # Getting the Standard Error from examinees_i and item_j
            seCurrent <- semTheta(thetaCurrent, it = bank[removedItems,], x = as.numeric(responsesForAdministeredItems),
                                  method = 'EAP', parInt = c(-4,4,10))
            
            # If collected more than 2 SE
            if ( length(seThetas) >= 2 ) {
              
              sePrev <- seThetas[length(seThetas)]
              
              ### The CAT stopping rule
              if ( reachedTheEstabilityPoint(percent = 0.01, sePrev, seCurrent) ) {
                finalItemsQttSelected <- i
                i <- 50 # force to stop the while loop
              }
              
            }
            ###
            
            seThetas <- rbind(seThetas, c(seCurrent))
          }
        }
        
        i <- i + 1
      } #\ 45 itens responses
      
      # storing the result
      resList <- rbind(resList, c(isr, finalItemsQttSelected, thetaCurrent, seCurrent))
    }
    
    #### FINAL ESTIMATION
    # EAP estimation, uniform prior distribution upon range [-4,4]
    #finalEst <- thetaEst(it = bank[removedItems,], x = responsesForAdministeredItems, method = "EAP", priorDist = "norm", parInt = c(-4,4,10))
    
    #seFinal <- semTheta(finalEst, it = bank[removedItems,], x = c(responsesForAdministeredItems),
    #         method = 'EAP', parInt = c(-4,4,10))
    
    #alpha <- 0.0001
    #confIntFinal <- c(finalEst - qnorm(1 - alpha/2) * 
    #                    seFinal, finalEst + qnorm(1 - alpha/2) * 
    #                    seFinal)
    
  } #\ 500 examinees responses
  
  #incrementing group length with 500
  groupLength <- (groupLength + 500)
  
} #\ 10 groups

# To print local
write.table(resList, file=paste("outs/5k_examinees/implemented_cat/2012/local/",isr,"2.out", sep=""), row.names=FALSE, col.names=TRUE)

# To print by Aguia HPC
#write.table(resList, row.names=FALSE, col.names=TRUE)

close(conn)
close(connTheta)
