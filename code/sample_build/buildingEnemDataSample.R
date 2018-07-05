#' @description Building ENEM data responses by a sample
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 fev
#' @references 2012, INEP

# Reading ENEM data
enem2012 <- './../../outs/DADOS_ENEM_2012.csv'
#enem2012 <- '/media/victorjatoba/Data/Google\ Drive/Master\ Degree/[VictorJ]\ Project/Enem/microdados_enem2012/DADOS/DADOS_ENEM_2012.csv'
#enem2014 <- '/media/victorjatoba/Data/Google\ Drive/Master\ Degree/[VictorJ]\ Project/Enem/microdados_enem2014/DADOS/MICRODADOS_ENEM_2014.csv'
#enem2016 <- '/media/victorjatoba/Data/Google\ Drive/Master\ Degree/[VictorJ]\ Project/Enem/Microdados_enem_2016/DADOS/microdados_enem_2016.csv'
#enem2016 <- './../../outs/microdados_enem_2016.csv'

enemChose = 2012

# Foram 5.791.065 de inscritos e cerca de 4 milhoes de participantes 
qttDataToBeExtractFromCSV <- 5000000

if (enemChose == 2012) {
  model <- read.csv(enem2012, nrows = qttDataToBeExtractFromCSV)
  
  #extracting data from users with the rules:
  res <- subset(model,
                # 1. caderno rosa;
                ID_PROVA_MT == 152 &
                # 2. compareceu no dia da prova;
                IN_PRESENCA_MT == 1 &
                # 3. Nao deixou as respostas em branco;
                TX_RESPOSTAS_MT != "............................................." &
                # 4. Nao teve dupla marcacao para todas as questoes;
                TX_RESPOSTAS_MT != "*********************************************")
  
  only_responses <- res[c("TX_RESPOSTAS_MT", "DS_GABARITO_MT")]
  
} else if (enemChose == 2014) {
  model <- read.csv(enem2014, nrows = qttDataToBeExtractFromCSV)
  res <- subset(model, IN_PRESENCA_MT == 1 & TX_RESPOSTAS_MT != ".............................................")
  only_responses <- res[c("TX_RESPOSTAS_MT", "GABARITO_MT")]
  
} else {
  model <- read.csv(enem2016, nrows = qttDataToBeExtractFromCSV, sep = ";")
  res <- subset(model, TP_PRESENCA_MT == 1 & TX_RESPOSTAS_MT != ".............................................")
  only_responses <- res[c("TX_RESPOSTAS_MT", "TX_GABARITO_MT")]
}

# Building a sample with 1M of examinees
qttExamineesSelected = 1000000

samp <- only_responses[ sample(nrow(only_responses), qttExamineesSelected), ]

# Change the sample to a matrix
mat <- as.matrix(samp)

n <- nrow(mat)

# Init final matrix contains the hit answer (0 or 1) for each examinee
finalMatrixOfResponses <- matrix(nrow = 0, ncol = 45)

# building the responses pattern with 0's, 1's or '.'
for(i in 1:n) {
  # Converting lines to character vector
  responses <- unlist(strsplit(mat[i,1], ""))
  answerSheet <- unlist(strsplit(mat[i,2], ""))
  
  # Obtaining a vector with 0's, 1's or '.'
  answerHitted <- vector(length = length(responses))
  
  for(q in 1:length(responses)) {
    
    if (responses[q] != '*' & responses[q] != '.') {
      # examinee answered
      if (responses[q] == answerSheet[q]) {
        # correct
        answerHitted[q] <- 1
      } else {
        # incorrect
        answerHitted[q] <- 0
      }
      
    } else {
      # The default character indicating a missing response on the ICL is a period
      answerHitted[q] <- '.' 
      
    }
  }
  
  # Putting the examinee responses on final matrix
  finalMatrixOfResponses <- rbind(finalMatrixOfResponses, c(answerHitted))
}

# Storing in a txt file
write.table(finalMatrixOfResponses, row.names=FALSE, col.names=FALSE)