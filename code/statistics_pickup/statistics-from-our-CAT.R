###################
#' @desc Analyze the RMSE, BIAS and SE of the our CAT solution.
#'
#' @Author: @victorjatoba
#' @Email: victorjatoba[at]usp.br
#' @Organization: University of Sao Paulo (USP)
#' @Date: 2018, jul
###################

## LIBS ##
libDir <- "~/R/x86_64-pc-linux-gnu-library"
if (!require('catR', lib=libDir)) install.packages("catR", lib=libDir)
if (!require('jsonlite', lib=libDir)) install.packages("jsonlite", lib=libDir)

library('catR')
library('jsonlite')

##########

## FUNCTIONS ##

###############

## Loading parameters
enem_mat_param = read.table("./data/spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

isr <- "KLP"
# MFI = Maximum Fisher Information
# KL
# KLP
# MLWI
# MPWI
# GDI
# GDIP
# MEI
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

# the list of statistics by each examinee. +2 is userID, thTrue and thHat
resList <- matrix(nrow = 0, ncol = 5)
colnames(resList) <- c("UserId", "ThTrue", "ThFinal", "EstimatedThetas", "SeThetasList")
resList <- data.frame(resList)


## INIT VALUES ##
groupLength <- 1

#the biggest ISR stop rule (from MFI. See: cat-simulation-with-fixed-stop-rule.R)
stopRuleLenght <- 45

# The total of examinees that answer more than 40 items
#totalOfExaminees <- 4979
totalOfExaminees <- 0

sumDifferenceOfThetasHatAndTrue = 0
squareSumDifferenceOfThetasHatAndTrue = 0

# n = 1
# 10 groups of 500 examinees responses. Total 5000 responses
for (n in 1:10) {
  
  groupResponsesN <- linn[groupLength:((groupLength-1)+500)]
  groupDataN <- linnData[groupLength:((groupLength-1)+500)+1]

  # 500 examinees responses
  # j = 3
  # j = 446
  for (j in 1:500) {
    
    # initializing vector contains the administered items
    removedItems <- matrix(nrow = 0, ncol = 1)
    administeredItems <- matrix(nrow = 0, ncol = 1)
    responsesForAdministeredItems <- matrix(nrow = 0, ncol = 1)
    seThetas <- matrix(nrow = 0, ncol = 1)
    biasList <- matrix(nrow = 0, ncol = 1)
    rmseList <- matrix(nrow = 0, ncol = 1)
    estimatedThetas <- matrix(nrow = 0, ncol = 1)
    
    # change response line to table
    responseDataLine <- read.table(textConnection(groupResponsesN[[j]]))
    matrixResponses <- as.matrix(responseDataLine)

    if ( answerMoreThan40Items(matrixResponses) ) {
      
      # Initializing the estimated theta
      thetaHat <- 0
      
      # storing the user ID and its true theta
      userId <- read.table(textConnection(groupDataN[[j]]))[1]
      trueTheta <- read.table(textConnection(groupDataN[[j]]))[2]
      
      # loop for fixed Stop Rule
      # i = 3
      for (i in 1:stopRuleLenght) {
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
        ##############
        
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
          
          differenceOfThetasHatAndTrue <- (thetaHat - trueTheta)
          squareDifferenceOfThetasHatAndTrue <- (thetaHat - trueTheta)^2
          
          # calculating Sums
          sumDifferenceOfThetasHatAndTrue <- sumDifferenceOfThetasHatAndTrue + (thetaHat - trueTheta)
          squareSumDifferenceOfThetasHatAndTrue <- squareSumDifferenceOfThetasHatAndTrue + (thetaHat - trueTheta)^2
          
        } # if answerTheItem()
        
      } # fixed Stop Rule loop
            
      resList <- rbind(resList,
                          data.frame(UserId = userId,
                                     ThTrue = trueTheta,
                                     ThFinal = thetaHat,
                                     EstimatedThetas = I(list(c(estimatedThetas))),
                                     SeThetasList = I(list(c(seThetas)))
                          ))
            
    } # answer more than 40 items
    
  } #\ 500 examinees responses
  
  #incrementing group length with 500
  groupLength <- (groupLength + 500)
  
} #\ 10 groups

BIAS <- as.matrix(sumDifferenceOfThetasHatAndTrue / totalOfExaminees) # using matrix to fix biasList column form resList to use c()
RMSE <- sqrt( squareSumDifferenceOfThetasHatAndTrue / totalOfExaminees)

statistics = matrix(nrow = 0, ncol = 2)
colnames(statistics) = c("BIAS", "RMSE")
statistics <- data.frame(statistics)

statistics = rbind(statistics, data.frame(BIAS = BIAS, RMSE = RMSE))

# To print by local PC
jsonFile = toJSON(resList, pretty=T)
write(jsonFile, file=paste("outs/statistics_from_early_cat_stage/OURCAT.json", sep=""))
jsonFileStatistics = toJSON(statistics, pretty=T)
write(jsonFileStatistics, file=paste("outs/statistics_from_early_cat_stage/OURCAT-statistics.json", sep=""))

# To print by Aguia HPC
#write.table(resList, row.names=FALSE, col.names=TRUE)

close(conn)
close(connData)
close(connTheta)
