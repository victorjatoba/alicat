## Read lines example
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Date: oct 2017

fileName <- "./data/enem_2012_responses.txt";
conn <- file(fileName, open = "r")
linn <- readLines(conn)

for (i in 1:length(linn)) {
  dataline<-read.table(textConnection(linn[[i]]))
  onlyMathResponses <- data[91:136]
}

close(conn)