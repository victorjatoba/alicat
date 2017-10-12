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

Start <- list(nrItems = 1, theta = 0, startSelect = "MFI")
#testList(Start, type = "start")

Test <- list(method = "EAP", itemSelect = "MFI")
#testList(Test, type = "test")

### STOP RULE ###
# Creation of a stopping rule: precision criterion, standard error to be reached 0.n
Stop <- list(rule = "precision", thr = 0.5)
# Stop rule by adding a length criterion, with threshold of n items
#Stop <- list(rule = "length", thr = 26)
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

for (i in 1:length(linn)) {
  #i = 821 #th -1.75
  #i = 53 #th 2.17
  
  # change response line to table
  responseDataLine <- read.table(textConnection(linn[[i]]))
  # reading only math responses
  onlyMathResponses <- as.matrix(responseDataLine[91:136])

  
  # change theta line to table
  truethetaDataLine <- read.table(textConnection(linnTheta[i]))
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
}

close(conn)
close(connTheta)