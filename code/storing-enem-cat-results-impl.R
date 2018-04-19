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
# fileName <- "./data/2012-enem-5k.theta";
# connTheta <- file(fileName, open = "r")
# linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 7)
colnames(resList) <- c("Rule", "itemsQttSelected", "thFinal", "trueTheta", "seFinal", "ciFinal_Down","ciFinal_Up")

linnLength <- length(linn)

# vector contains the administered items
removedItems <- matrix(nrow = 1, ncol = 1)


i = 1
for (i in 1:linnLength) {
  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[i]]))
  matrixResponses <- as.matrix(responseDataLine)
  
  ## selecting 4 items

  # Estimating current examinee hability
  # Using EAP with 10 quadrature points (same of BILOG)
  currentTheta <- eapEst(bank, responses, nqp = 10)
  
  # # change theta line to table
  # truethetaDataLine <- read.table(textConnection(linnTheta[i]))  
  # # reading the true thetas estimated by ICL software using EAP
  # truethetaEAP <- as.matrix(truethetaDataLine[1])
  

  # Selecting the next item.
  itemInfo <- nextItem(bank, theta = currentTheta, out = removedItems, criterion = isr)
  
  # Removing the item selected above
  itemRemoved <- itemInfo$item
  
  # Adding removed item to the list
  removedItems <- rbind(removedItems, c(itemRemoved))
  
  # EAP estimation, standard normal prior distribution
  th <- thetaEst(bank, x = matrixResponses, method = "EAP")
  c(th, semTheta(th, tcals, x, method = "EAP"))
  
  # RUN CAT
  #res <- randomCAT(trueTheta = truethetaEAP,
  #                 itemBank = Bank,
  #                 responses = matrixResponses,
  #                 start = Start, test = Test,
  #                 stop = Stop, final = Final)
  
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
