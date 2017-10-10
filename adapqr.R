## AdapQ R code
##
## Author: @victorjatoba
## Email: victorjatoba[at]usp.br
## Organization: University of Sao Paulo (USP)
## Data: 08/23/17

if (!require('catR')) install.packages('catR')
require('catR')

# reading data
data = read.table("enem_2012.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
#data = read.table("Data-TCALS-1998-2000.txt", header = TRUE, sep = " ", stringsAsFactors = FALSE)
bank <- as.matrix(data) #optional

# create a bank with 3PL items
Bank <- genDichoMatrix(items = 500, cbControl = NULL, model = "3PL",
                       seed = 1)

# list of four parameters that characterize a CAT: start, test, stop, final
# these lists will feed the randomCAT function to generate a response pattern

# one first item selected, ability level starts at 0, criterion for
# selecting first items is maximum Fisher information
Start <- list(nrItems = 1, theta = 0, startSelect = "MFI")

# use weighted likelihood, select items through MFI (see previous comment)
Test <- list(method = "WL", itemSelect = "MFI")

# stopping rule by classication, meaning that the test will stop when the
# CI no longer holds the threshold inside it anymore.
# Also, to stop the test when the provisional standard error of ability becomes less than or equal to
# the pre-specified value; "classification"
#Stop <- list(rule = "classification", thr = 2, alpha = 0.05)
Stop <- list(rule = "precision", thr = 0.5, alpha = 0.02)

# how estimates of ability are calculated
Final <- list(method = "EAP", alpha = 0.05)

# set true ability at 1, calls lists above
#res <- randomCAT(trueTheta = rnorm(n=1,mean=0,sd=1), itemBank = Bank,
#                 start = Start, test = Test,
#                 stop = Stop, final = Final)
res <- randomCAT(trueTheta = 1, itemBank = Bank, start = Start,
                   test = Test, stop = Stop, final = Final)

# plotting the response pattern
plot(res, ci = TRUE, trueTh = TRUE, classThr = 2)
#plot(res, ci = FALSE, alpha = 0.05, trueTh = TRUE, classThr = NULL)

res