## Example using a sample of only the "Matematica e suas tecnologias" area from 2012 enem.
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: oct 2017

if (!require('catR')) install.packages('catR')
require('catR')

# Use catR package
library("catR")

#path <- "."
#setwd(path)

## Loading parameters
enem_mat_param = read.table("data/enem_math.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

# Change to Matrix
Bank <- as.matrix(enem_mat_param)

# MFI = Maximum Fisher Information
Start <- list(nrItems = 1, theta = 0, startSelect = "MFI")
#testList(Start, type = "start")

Test <- list(method = "EAP", itemSelect = "MPWI")
#testList(Test, type = "test")

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
fileName <- "./data/enem_2012_responses.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

## Loading true thetas
fileName <- "./data/enem_math.theta";
connTheta <- file(fileName, open = "r")
linnTheta <- readLines(connTheta)

  # low knowledge
  #i = 555 #th -1.793918 7
  #i = 579 #th -1.843908 7
  ##i = 849 #th -1.973992 -5.999638 7
  responserId = 849;

  # medium knowledge
  #i = 459 #th -0.009814 19
  #i = 622 #th -0.003207 16
  ##i = 363 #th -0.000403 15
  #responserId = 363;
  
  # high knowledge
  #i = 53 #th 2.17
  #i = 371 #th 2.237750 2.344757 38
  ##i = 358 #th 2.251985 38
  #responserId = 358;

  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[responserId]]))
  # reading only math responses
  onlyMathResponses <- as.matrix(responseDataLine[91:136])
  
  
  # change theta line to table
  truethetaDataLine <- read.table(textConnection(linnTheta[responserId]))
  # reading EAP estimated
  truethetaEAP <- as.matrix(truethetaDataLine[1])
  
  # RUN CAT
  res <- randomCAT(trueTheta = truethetaEAP,
                   itemBank = Bank,
                   responses = onlyMathResponses,
                   start = Start, test = Test,
                   stop = Stop, final = Final)
  
  plot(res, ci = TRUE, trueTh = TRUE, classThr = 2)
  res


close(conn)
close(connTheta)