#' @description Building ENEM data responses by a sample
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev
#' @references 2012, INEP

# Reading ENEM data
enem2012 <- './../../outs/DADOS_ENEM_2012.csv'
enem2014 <- '/media/victorjatoba/Data/Google\ Drive/Master\ Degree/[VictorJ]\ Project/Enem/microdados_enem2014/DADOS/MICRODADOS_ENEM_2014.csv'
is2012 <- TRUE
model <- read.csv(enem2012, nrows = 1000000)

# Obtained only Math responses
res <- subset(model, IN_PRESENCA_MT == 1 & TX_RESPOSTAS_MT != ".............................................")

# Obtained only responses column
if (is2012) {
  only_responses <- res[c("TX_RESPOSTAS_MT", "DS_GABARITO_MT")]
  
} else {
  only_responses <- res[c("TX_RESPOSTAS_MT", "GABARITO_MT")]
  
}

# Building a sample
samp <- only_responses[ sample(nrow(only_responses), 200000), ]

# Change the sample to a matrix
mat <- as.matrix(samp)

n <- nrow(mat)

# Init final matrix contains the hit answer (0 or 1) for each examinee
finalMatrixOfResponses <- matrix(nrow = 0, ncol = 45)

for(i in 1:n){
  # Converting lines to character vector
  responses <- unlist(strsplit(mat[i,1], ""))
  answerSheet <- unlist(strsplit(mat[i,2], ""))
  
  # Obtaining vector with only 0's and 1's
  answerHitted <- vector(mode = "integer" , length = length(responses))
  for(q in 1:length(responses)) {
    if (responses[q] == answerSheet[q]) {
      answerHitted[q] <- 1
    } else {
      answerHitted[q] <- 0
    }
  }
  
  # Putting the examinee responses on final matrix
  finalMatrixOfResponses <- rbind(finalMatrixOfResponses, c(answerHitted))
}

# Storing in a txt file
if (is2012) {
  write.table(finalMatrixOfResponses, row.names=FALSE, col.names=FALSE)
} else {
  write.table(finalMatrixOfResponses, file="../data/2014_enem_responses.txt", row.names=FALSE, col.names=FALSE)
}
