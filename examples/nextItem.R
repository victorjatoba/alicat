## Dichotomous models ##


if (!require('catR')) install.packages('catR')
require('catR')

# Use catR package
library("catR")


## Loading parameters
enem_mat_param = read.table("./data/2012_bilog/2012-enem.par", header = TRUE, sep = " ", stringsAsFactors = FALSE)

# Change to Matrix
bank <- as.matrix(enem_mat_param)

## MFI criterion
# Selecting the next item, current ability estimate is 0
nextItem(bank, theta = 3) # item 63 is selected

nextItem(bank, theta = 3, out = c(33,37,39,13,34)) # item 10 is selected