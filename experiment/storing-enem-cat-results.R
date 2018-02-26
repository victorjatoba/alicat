## @desc Storing the students sample CAT results.
## @test_name ENEM "Matematica e suas tecnologias" area.
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: 2018 jan

if (!require('catR')) install.packages('catR')
require('catR')

# Use catR package
library("catR")

## Loading parameters
enem_mat_param = read.table("data/enem-math.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

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

# low knowledge
#i = 293 #th -1.182060
#i = 1853 #th -1.07 
#i = 1798 #th -1.173081
#i = 2147 #th -1.170712
#i = 2352 #th -1.176256

# medium knowledge
#i = 621 #th -1.177539

# high knowledge
#i = 3790 #th 2.631965
#i = 43 #th 1.76
#Selecionar os 800 maiores e avaliar por 10 intervalos (2,0 - 2,1; 2,1 - 2.2)

# weird behaviour
# i = 846
# i = 867
#i = 1245
#i = 31
#i = 888
#i = 123
#i = 12
#i = 329
#i = 4189

# very close
#i = 7
#i = 9
#i = 13
#i = 19
#i = 226
#i = 3294
#i = 3297
#i = 3234
#i = 4111
#i = 439

# very distant
#i = 224

# low administered items
#i = 3534
#i = 433


## Loading responses
fileName <- "./data/2012-enem-responses.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

## Loading true thetas
fileName <- "./data/enem-math.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

## List of the results
resList <- matrix(nrow = 0, ncol = 7)
colnames(resList) <- c("Rule", "itemsQttSelected", "thFinal", "trueTheta", "seFinal", "ciFinal_Down","ciFinal_Up")

linnLength <- length(linn)
#linnLength <- 2
i <- 1

## count running time
if (!require('tictoc')) install.packages('tictoc')
library(tictoc)
tic()

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
print(paste('The ', isr, ' running time is: ', sep = ""))
toc()

# Storing in a txt file
write.table(resList, file=paste("outs/spenassato/",isr,".out", sep=""), row.names=FALSE, col.names=TRUE)

close(conn)
close(connTheta)