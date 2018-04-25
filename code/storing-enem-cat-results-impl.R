## @desc Storing the students sample CAT results building the code by 0 (without randomCAT).
## @test_name ENEM "Matematica e suas tecnologias" area.
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: 2018 apr

#if (!require('catR', lib="./../../R/library")) install.packages("catR", lib="./../../R/library")
#.libPaths()
#require('catR', lib="./../../R/library")

# Use catR package
library('catR')


## Loading parameters
enem_mat_param = read.table("./data/2012-enem-spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

isr <- "MFI"

# Change to Matrix
bank <- as.matrix(enem_mat_param)

## Loading responses
fileName <- "./data/2012-enem-responses-5k.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

# ## Loading true thetas
fileName <- "./data/2012-enem-5k.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 7)
colnames(resList) <- c("Rule", "itemsQttSelected", "thFinal", "trueTheta", "seFinal", "ciFinal_Down","ciFinal_Up")

linnLength <- length(linn)

j = 1 #debug
# loop for 5000 examinees
for (j in 1:linnLength) {
  
  # vector contains the administered items
  removedItems <- matrix(nrow = 0, ncol = 1)
  responsesForAdministeredItems <- matrix(nrow = 0, ncol = 1)
  seThetas <- matrix(nrow = 0, ncol = 1)
  
  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[j]]))
  matrixResponses <- as.matrix(responseDataLine)
  
  ## FIRST ITEM
  # Selecting 1 starting item, initial ability estimate is 0
  #itemSelected <- startItems(bank)
  ## Selecting 4 starting items for ability levels between -2 and 2
  # startItems(bank, randomesque = 4, theta = c(-2, 2))

  # Initializing the estimated theta
  thetaCurrent <- 0
  
  # To identify the estibility point of the CAT. See Equation 2 from Spenassato (2016)
  estabilityPoint1Percent <- 0
  estabilityPoint5Percent <- 0

  
  #i <- 1 #debug
  # Loop for the 45 items responses
  for (i in 1: 45) {
    # Selecting the next item.
    itemInfo <- nextItem(bank, theta = thetaCurrent, out = removedItems, criterion = isr)
    
    # Getting the last item selected
    selectedItem <- itemInfo$item
    
    # Adding the last administered item to the removedItems list
    removedItems <- rbind(removedItems, c(selectedItem))
    
    # Storing the response of the selected item
    responsesForAdministeredItems <- rbind(responsesForAdministeredItems, c(matrixResponses[selectedItem]))
    
    # EAP estimation, standard normal prior distribution with 10 quadrature points
    # By default, it takes the vector value (-4, 4, 33), that is, 33 quadrature points on the range [-4; 4] (or, by steps of 0.25)
    thetaCurrent <- eapEst(it = bank[removedItems,], x = responsesForAdministeredItems, nqp = 10)
    
    if (i > 1) {
      # Getting the Standard Error from examinees_i and item_j
      seCurrent <- semTheta(thetaCurrent, it = bank[removedItems,], x = c(responsesForAdministeredItems),
                            method = 'EAP', parInt = c(-4,4,10))
    
      ### IDENTIFYING THE 1% AND THE 5% estability point of the CAT
      if (i > 2) {
        sePrev <- seThetas[length(seThetas)]
        # checking the 1% stabiliting point. See Equation 2 from Spenassato (2016)
        if ( estabilityPoint1Percent == 0 && (abs(seCurrent - sePrev) < abs(0.01 * sePrev)) ) {
          estabilityPoint1Percent <- i
        }
        
        # checking the 5% stabiliting point. See Equation 2 from Spenassato (2016)
        if ( estabilityPoint5Percent == 0 && (abs(seCurrent - sePrev) < abs(0.05 * sePrev)) ) {
          estabilityPoint5Percent <- i
        }
        
      }
      ###
      
      seThetas <- rbind(seThetas, c(seCurrent))
    }
    
    #i <- i + 1 #debug
  }
  
  
  #### FINAL ESTIMATION
  # EAP estimation, uniform prior distribution upon range [-4,4]
  finalEst <- thetaEst(it = bank[removedItems,], x = responsesForAdministeredItems, method = "EAP", priorDist = "norm", parInt = c(-4,4,10))
  
  seFinal <- semTheta(finalEst, it = bank[removedItems,], x = c(responsesForAdministeredItems),
           method = 'EAP', parInt = c(-4,4,10))

  alpha <- 0.0001
  confIntFinal <- c(finalEst - qnorm(1 - alpha/2) * 
                      seFinal, finalEst + qnorm(1 - alpha/2) * 
                      seFinal)
  
  
  # change theta line to table
  truethetaDataLine <- read.table(textConnection(linnTheta[j]))  
  # # reading the true thetas estimated by ICL software using EAP
  truethetaEAP <- as.matrix(truethetaDataLine[1])
  
  #plot(res, ci = TRUE, trueTh = TRUE, classThr = 2)
  #res
  #resList <- c(i, get("thFinal", res))
  finalItemsQttSelected <- length(get("testItems", res))
  
  resList <- rbind(resList, c(isr, finalItemsQttSelected, get("thFinal", res), get("trueTheta", res), get("seFinal", res), get("ciFinal", res)))
}
#print(paste('The ', isr, ' running time is: ', sep = ""))
#toc()

# Storing in a txt file

# To print local
#write.table(resList, file=paste("outs/5k_examinees/",isr,".out", sep=""), row.names=FALSE, col.names=TRUE)
#To print by Aguia HPC
write.table(resList, row.names=FALSE, col.names=TRUE)
#resList

close(conn)
close(connTheta)
