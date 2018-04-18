## Example using a sample of only the "Matematica e suas tecnologias" area from 2012 enem.
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: nov 2017

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
i = 555 #th -1.793918 7
#i = 579 #th -1.843908 7
#i = 849 #th -1.973992 -5.999638 7

# medium knowledge
#i = 459 #th -0.009814 19
#i = 622 #th -0.003207 16
##i = 363 #th -0.000403 15

# high knowledge
#i = 53 #th 2.17
#i = 371 #th 2.237750 2.344757 38
##i = 358 #th 2.251985 38

responserId = i;

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

# a = c(-1.793918, -1.843908, -1.973992, -1.986, -1.971, -2.19)
#b = c(-1.986, -1.971, -2.19)
#c = matrix(a, ncol = 2, nrow = 3)
#plot(c)


#F = c(-0.497,-0.816,-1.082,-1.271,-1.411,-1.539,-1.706,-1.791,-1.857,-1.907,-1.951,-2.003,-2.048,-2.087,-2.12,-2.138,-2.07,-2.102,-2.118,-2.133,-2.15,-2.17,-2.181,-2.189,-2.197,-2.205,-2.179,-2.186,-2.191,-2.195,-2.198,-2.201,-2.203,-2.198,-2.193,-2.194,-2.191,-2.189,-2.19,-2.19,-2.19,-2.191,-2.19,-2.19,-2.19)
#KLP = c(-0.497,-0.816,-1.082,-1.271,-1.411,-1.539,-1.706,-1.791,-1.857,-1.907,-1.951,-2.003,-2.048,-2.081,-2.12,-2.138,-2.07,-2.102,-2.118,-2.133,-2.154,-2.17,-2.181,-2.189,-2.197,-2.204,-2.211,-2.186,-2.191,-2.195,-2.198,-2.201,-2.196,-2.198,-2.193,-2.194,-2.195,-2.192,-2.19,-2.19,-2.19,-2.191,-2.191,-2.19,-2.19)
#MPWI = c(-0.497,-0.816,-1.082,-1.271,-1.411,-1.496,-1.612,-1.698,-1.846,-1.907,-1.951,-2.003,-2.048,-2.081,-2.12,-2.138,-2.07,-2.086,-2.118,-2.133,-2.154,-2.17,-2.181,-2.189,-2.196,-2.204,-2.178,-2.186,-2.191,-2.195,-2.198,-2.201,-2.196,-2.197,-2.193,-2.194,-2.195,-2.192,-2.19,-2.19,-2.19,-2.191,-2.191,-2.19,-2.19)

#plot(KLP, MPWI)


data = (c(-1.793918, -1.843908, -1.973992,
          -1.986, -1.971, -2.19,
          -1.986, -1.971, -2.19,
          -1.986, -1.971, -2.19))

mat = matrix(data, nrow = 4, ncol = 3)
rownames(mat) = c("TRUE", "MPWI", "KLP", "MFI")
plot(mat)
