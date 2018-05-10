## @desc Storing the students sample CAT results.
## @test_name ENEM "Matematica e suas tecnologias" area.
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: 2018 jan

#if (!require('catR', lib="./../../R/library")) install.packages("catR", lib="./../../R/library")
#.libPaths()
#require('catR', lib="./../../R/library")

# Use catR package
library('catR')


## Loading parameters
enem_mat_param = read.table("./../data/spenassato.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

# Change to Matrix
Bank <- as.matrix(enem_mat_param)

Start <- list(nrItems = 1, theta = 0, startSelect = "MFI")

isr <- "MFI"
Test <- list(method = "EAP", itemSelect = isr)
# MFI = Maximum Fisher Information
# KL
# KLP
# GDI
# GDIP
# MEI
# MLWI
# MPWI

### STOP RULE ###
# Creation of a stopping rule: precision criterion, standard error to be reached 0.n
Stop <- list(rule = "precision", thr = 0.5)
# Stop rule by adding a length criterion, with threshold of n items
#Stop <- list(rule = "length", thr = 26
# Update of the stopping rule: by adding a length criterion, with threshold of 15 items
#Stop <- list(rule = c("precision", "length"), thr = c(0.6, 45))
# Classification criterion, with classification threshold 0 and alpha level 0.05
#Stop <- list(rule = "classification", thr = 0.5, alpha = 0.05)

Final <- list(method = "EAP", alpha = 0.05)
#testList(Final, type = "final")

## Loading responses
fileName <- "./../data/spenassato-enem-responses-5k.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

## Loading true thetas
fileName <- "./../data/spenassato-enem-5k.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 7)
colnames(resList) <- c("Rule", "itemsQttSelected", "thFinal", "trueTheta", "seFinal", "ciFinal_Down","ciFinal_Up")

linnLength <- length(linn)
#linnLength <- 2
#i <- 1

## count running time
#if (!require('tictoc')) install.packages('tictoc')
#library(tictoc)
#tic()

for (i in 1:linnLength) {
  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[i]]))
  matrixResponses <- as.matrix(responseDataLine)
  
  # change theta line to table
  truethetaDataLine <- read.table(textConnection(linnTheta[i]))
  
  # reading EAP estimated
  truethetaEAP <- as.matrix(truethetaDataLine[1])
  
  # RUN CAT
  res <- randomCAT(trueTheta = truethetaEAP,
                   itemBank = Bank,
                   responses = matrixResponses,
                   start = Start, test = Test,
                   stop = Stop, final = Final)
  
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
