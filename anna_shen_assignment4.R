## Page 61

# Question 1:
# n Section 1.12.1.2, the reader was reminded that the results of a cross- validation are random, 
# due to the random partitioning into training and test sets. Try doing several runs of the linear 
# and k-NN code in that section, comparing results.

install.packages("freqparcoord")
library(freqparcoord)

xvalpart <- function(data,p) {
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n,ntrain,replace=FALSE)
  list(train = data[trainidxs,],
       valid = data[-trainidxs,])
}

xvallm <- function(data,ycol,predvars,p,meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %% coef(lmout)
  realy <- valid[,ycol]
  if(meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

data(mlb)
xvallm(mlb,5,c(4,6),2/3)

# Using linear: 
# Output 1: 250.7944, Output 2: 250.3148
# This is sort of different but not meaningfully different

install.packages("regtools")
install.packages("preprocessx")

xvalknn <- function(data,ycol,predvars,k,p,meanabs=TRUE) {
  data <- data[,c(predvars,ycol)]
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout,valid[,ycol],TRUE)
  realy <- valid[,ycol]
  if(meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

library(regtools)
set.seed(9999)
xvalknn(mlb,5,c(4,6),25,2/3)

# Using KNN:
# Output failed because preprocessx is not available for R version 3.4.1...


# Question 2
# Extend (1.28) to include interaction terms for age and gender, and age2 and gender. Run the new model, 
# and find the estimated effect of being female, for a 32-year-old person with a Master’s degree.

library(freqparcoord)
data(prgeng)
prgeng$age2 <- prgeng$age ^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe <- as.matrix(pe)

lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem, data = prgeng)





