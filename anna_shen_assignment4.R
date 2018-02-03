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

## Coefficients:
# (Intercept)          age         age2      wkswrkd           ms          phd          fem  
# -81136.70      3900.35       -40.33      1196.39     15431.07     23183.97    -11484.49 
# index starts at intercept = 0

# Adding interaction terms:

gender_educ <- lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem+phd:fem+ms:fem+age:fem+age2:fem, data = prgeng)

# female 32 masters
wage.lm <- lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem+age:fem+age2:fem,data=prgeng)
predict_data <- data.frame(age=32,age2=32^2,wkswrkd=52,ms=1,phd=0,fem=1,agefem=32*1,agefem2=(32^2)*1)
predict(wage.lm,predict_data,interval="confidence")

# output: 69086.59

# male 32 masters
predict_data <- data.frame(age=32,age2=32^2,wkswrkd=52,ms=1,phd=0,fem=0,agefem=32*0,agefem2=(32^2)*0)
predict(wage.lm,predict_data,interval="confidence")

# output: 79831.75

# Therefore, female engineers who are 32 with a masters degree earn about 10.7k less than males with the same age and education.


