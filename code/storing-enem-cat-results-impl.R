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

i = 1
for (i in 1:linnLength) {
  
  # vector contains the administered items
  removedItems <- matrix(nrow = 0, ncol = 1)
  responsesForAdministeredItems <- matrix(nrow = 0, ncol = 1)
  
  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[i]]))
  matrixResponses <- as.matrix(responseDataLine)
  
  ## FIRST ITEM
  # Selecting 1 starting item, initial ability estimate is 0
  #itemSelected <- startItems(bank)
  ## Selecting 4 starting items for ability levels between -2 and 2
  # startItems(bank, randomesque = 4, theta = c(-2, 2))

  # Initializing theta
  currentTheta <- 0
  
  for (j in 1: 45) {
    # Selecting the next item.
    itemInfo <- nextItem(bank, theta = currentTheta, out = removedItems, criterion = isr)
    
    # Getting the last item selected
    selectedItem <- itemInfo$item
    
    # Adding the last administered item to the removedItems list
    removedItems <- rbind(removedItems, c(selectedItem))
    responsesForAdministeredItems <- rbind(responsesForAdministeredItems, c(matrixResponses[selectedItem]))
    
    
    # EAP estimation, standard normal prior distribution
    currentTheta <- eapEst(it = bank[removedItems,], x = responsesForAdministeredItems, nqp = 10)
    
    #################
    ## GETTING SE
    seProv <- semTheta(thProv, PAR, x = PATTERN, 
                       model = model, D = test$D, method = test$method, 
                       priorDist = test$priorDist, priorPar = test$priorPar, 
                       parInt = test$parInt, constantPatt = test$constantPatt)
    SETH <- c(SETH, seProv)
    
    ## STOP RULE
    stop.cat <- checkStopRule(th = thProv, se = seProv, N = length(PATTERN), it = itemBank[-ITEMS,], model = model, stop = stop)
    if (stop.cat$decision | length(PATTERN) == nrow(itemBank)) 
      break
    #################
    
    ### IDENTIFYING 1% AND 5% precision point
    semTheta()
    EP1 <- x
    EP2 <- y

    if (1% j) {
      parada1% <- j
    }
    
    if (5%) {
      parada5% <- j
    }
    ###
  }
  
  
  
  #### FINAL ESTIMATION
  finalEst <- thetaEst(PAR, PATTERN, model = model, 
                       D = final$D, method = final$method, priorDist = final$priorDist, 
                       priorPar = final$priorPar, range = final$range, 
                       parInt = final$parInt)
  seFinal <- semTheta(finalEst, PAR, x = PATTERN, 
                      model = model, D = final$D, method = final$method, 
                      priorDist = final$priorDist, priorPar = final$priorPar, 
                      parInt = final$parInt)
  confIntFinal <- c(finalEst - qnorm(1 - final$alpha/2) * 
                      seFinal, finalEst + qnorm(1 - final$alpha/2) * 
                      seFinal)
  
  
  if (!stop.cat$decision) 
    endWarning <- TRUE
  else endWarning <- FALSE
  RES <- list(trueTheta = trueTheta, model = model, 
              testItems = ITEMS, itemPar = PAR, pattern = PATTERN, 
              thetaProv = TH, seProv = SETH, ruleFinal = stop.cat$rule, 
              thFinal = finalEst, seFinal = seFinal, ciFinal = confIntFinal, 
              genSeed = genSeed, startFixItems = start$fixItems, 
              startSeed = start$seed, startNrItems = start$nrItems, 
              startTheta = start$theta, startD = start$D, 
              startRandomesque = start$randomesque, startRandomSeed = start$random.seed, 
              startSelect = start$startSelect, startCB = startCB, 
              provMethod = test$method, provDist = test$priorDist, 
              provPar = test$priorPar, provRange = test$range, 
              provD = test$D, itemSelect = test$itemSelect, 
              infoType = test$infoType, randomesque = test$randomesque, 
              testRandomSeed = test$random.seed, AP = test$AP, 
              constantPattern = test$constantPatt, cbControl = cbControl, 
              cbGroup = cbGroup, stopRule = stop$rule, stopThr = stop$thr, 
              stopAlpha = stop$alpha, endWarning = endWarning, 
              finalMethod = final$method, finalDist = final$priorDist, 
              finalPar = final$priorPar, finalRange = final$range, 
              finalD = final$D, finalAlpha = final$alpha, 
              save.output = save.output, output = output, 
              assigned.responses = assigned.responses)
  class(RES) <- "cat"
  
  
  
  
  
  
  
  
  
  
  
  
  # change theta line to table
  truethetaDataLine <- read.table(textConnection(linnTheta[i]))  
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
