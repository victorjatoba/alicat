###################
#' @desc Storing the CAT execution results from implemented CAT.
#'
#' @Author: @victorjatoba
#' @Email: victorjatoba[at]usp.br
#' @Organization: University of Sao Paulo (USP)
#' @Date: Mai, 2018
###################

## LIBS ##

#if (!require('catR', lib="./../../R/library")) install.packages("catR", lib="./../../R/library")
#.libPaths()
#require('catR', lib="./../../R/library")

# Use catR package
library('catR')
library(jsonlite)

## FUNCTIONS ##

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

## Loading parameters
enem_mat_param = read.table("./data/spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

isr <- "KLP"
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

resListVectors <- matrix(nrow = 0, ncol = 3)
colnames(resListVectors) <- c("Id", "EstimatedThetas", "EstimatedSE")

groupLength <- 1

# n = 1
# 10 groups of 500 examinees responses. Total 5000 responses
for (n in 1:10) {
  
  groupResponsesN <- linn[groupLength:((groupLength-1)+500)]
  groupDataN <- linnData[groupLength:((groupLength-1)+500)+1]

  # 500 examinees responses
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
      
      # Initializing the estimated theta
      thetaCurrent <- 0
      
      # To identify the estibility point of the CAT. See Equation 2 from Spenassato (2016)
      finalItemsQttSelected <- 0
      
      # storing the user ID and it true theta
      userId <- read.table(textConnection(groupDataN[[j]]))[1]
      trueTheta <- read.table(textConnection(groupDataN[[j]]))[2]
      
      i <- 1
      # loop on 45 items' responses
      while (i <= 45) {
        
        ####### OUR CAT STRATEGY #########
        # change the ISR to extreme theta hat
        if ( i == 4 ) {
          if ( hitFirst3Items(responsesForAdministeredItems) ) {
            isr = "MLWI"
          } else if ( missedFirst3Items(responsesForAdministeredItems) ) {
            isr = "MFI"
          }
        }
        
        # use KLP after 10 administered items
        if ( i > 10 ) {
          isr = "KLP"
        }
        ###################################
        
        # Selecting the next item.
        itemInfo <- nextItem(bank, theta = thetaCurrent, out = removedItems, criterion = isr)
        
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
          thetaCurrent <- eapEst(it = bank[removedItems,], x = as.numeric(responsesForAdministeredItems), nqp = 10)
          estimatedThetas <- rbind(estimatedThetas, c(thetaCurrent))
          
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
        } # if answerTheItem()
        
        i <- i + 1
      } #\ 45 itens loop responsesr
      
      # storing the result
      resList <- rbind(resList, 
                       data.frame(UserId = userId,
                                  ThTrue = trueTheta,
                                  ThFinal = thetaCurrent,
                                  ItemsQttSelected = finalItemsQttSelected,
                                  AdministeredItemsList = I(list(c(administeredItems))),
                                  ThEstimatedList = I(list(c(estimatedThetas))),
                                  SeThetasList = I(list(c(seThetas)))
                       ))
    }
    
  } #\ 500 examinees responses
  
  #incrementing group length with 500
  groupLength <- (groupLength + 500)
  
} #\ 10 groups

# To print by local PC
write.table(resList, file=paste("outs/stability_point/data-ourcat.out", sep=""), row.names=FALSE, col.names=TRUE)
jsonFile = toJSON(resList, pretty=T, auto_unbox = T)

# To print by Aguia HPC
#write.table(resList, row.names=FALSE, col.names=TRUE)

close(conn)
close(connData)
close(connTheta)
