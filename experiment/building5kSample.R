#' @description Getting 500 responses from each group (10 total). To mount the 5k responses file
#'
#' @author victorjatoba
#' @email victorjatoba[at]usp.br
#' @organization University of Sao Paulo (USP)
#' @date 2018 mar
#' @references 2016, Spenassato - Testes Adaptativos Computadorizados Aplicados em Avaliacoes Educacionais

trueThetaData = read.table("/home/victorjatoba/adapqr/data/enem.theta")
## Loading true theta estimated by ICL

ranges <- split(trueThetaData, cut(as.matrix(trueThetaData), c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5), include.lowest=TRUE))

## Loading all examinees' responses
fileName <- "/home/victorjatoba/adapqr/data/2012-enem-responses-1M.txt"
conn <- file(fileName, open = "r")
linnFromAllResponses <- readLines(conn)
close(conn)

# the matrix that will store the id, true theta and the responses of the selected examinees
examineesSelectedData <- matrix(nrow = 0, ncol = 3)
colnames(examineesSelectedData) <- c("Id", "TrueTheta", "Responses")

# the matrix that will store the respones
finalMatrixOfResponses <- matrix(nrow = 0, ncol = 45)

step <- 0.5

# id <- 888336 #fechou a prova

# the quantity of examinees should be extract for each group
sampleQtdForEachGroup = 500

trueThetaData = as.matrix(trueThetaData)

# 10 intervals' group (-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3.5)
for (i in 1:10) {
  
  # getting the group containing examinees id and theta
  group <- as.matrix(ranges[[i]])
  
  # only the examinees' id
  groupIds <- (rownames(group))

  # getting the sample of the group containing the set of ids
  examineesId <- as.matrix(sample(groupIds, sampleQtdForEachGroup, replace = FALSE))
  
  # finding the examinees' responses and theta by id. Then store.
  j <- 1
  for(j in 1:sampleQtdForEachGroup) {
    id = as.integer(examineesId[j])
    theta = trueThetaData[id]
    responses = linnFromAllResponses[id]
    
    # storing the id theta and the responses of the selected examinee
    examineesSelectedData <- rbind(examineesSelectedData, c(id, theta, responses))
    
    # storing only the responses
    responsesVector = as.integer(strsplit(responses, " ")[[1]])
    finalMatrixOfResponses <- rbind(finalMatrixOfResponses, c(responsesVector))
  }
}

write.table(finalMatrixOfResponses, file="/home/victorjatoba/adapqr/data/2012-enem-responses-5k.txt", row.names=FALSE, col.names=FALSE)
write.table(examineesSelectedData, file="/home/victorjatoba/adapqr/data/2012-enem-data-5k.txt", row.names=FALSE, col.names=TRUE)