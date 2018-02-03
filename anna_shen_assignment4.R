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

gender_educ <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + phd:fem + ms:fem + age:fem + age2:fem, data = prgeng)

# female 32 masters
wage.lm <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + age:fem + age2:fem, data = prgeng)
predict_data <- data.frame(age=32, age2=32^2, wkswrkd=52, ms=1, phd=0, fem=1, agefem=32*1, agefem2=(32^2)*1)
predict(wage.lm,predict_data,interval="confidence")

# output: 69086.59

# male 32 masters
predict_data <- data.frame(age=32, age2=32^2, wkswrkd=52, ms=1, phd=0, fem=0, agefem=32*0, agefem2=(32^2)*0)
predict(wage.lm,predict_data,interval="confidence")

# output: 79831.75

# Therefore, female engineers who are 32 with a masters degree earn about 10.7k less than males with the same age and education.


# Question 3:
# Consider the bodyfat data mentioned in Section 1.2. Use lm() to form a prediction equation for density from the other
# variables (skipping the first three), and comment on whether use of indirect methods in this way seems feasible.

bodyfat <- read.csv("bodyfat.csv")
head(bodyfat)
bodyfat2 <- bodyfat[,c(5:18)] # to drop case, brozek, siri

fatlm <- lm(density ~ ., data = bodyfat2)
summary(fatlm)
# output:
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.139e+00  4.030e-02  28.248  < 2e-16 ***
#  age         -1.203e-04  7.515e-05  -1.601  0.11062    
# weight       2.395e-04  1.243e-04   1.926  0.05528 .  
# height       1.498e-04  2.230e-04   0.672  0.50243    
# neck         1.075e-03  5.401e-04   1.991  0.04765 *  
#  chest        1.232e-04  2.303e-04   0.535  0.59339    
# abdomen     -2.277e-03  2.008e-04 -11.335  < 2e-16 ***
#  hip          5.513e-04  3.390e-04   1.626  0.10521    
# thigh       -6.149e-04  3.354e-04  -1.833  0.06799 .  
# knee        -4.844e-05  5.622e-04  -0.086  0.93141    
# ankle       -6.314e-04  5.145e-04  -1.227  0.22094    
# biceps      -5.755e-04  3.976e-04  -1.448  0.14907    
# forearm     -1.017e-03  4.626e-04  -2.198  0.02891 *  
#  wrist        3.959e-03  1.243e-03   3.185  0.00164 ** 

# it looks like measuring abdomen and wrist is strongly indicative of body fat %

# confirming 
fatlm2 <- lm(density ~ wrist + abdomen, data = bodyfat2)
summary(fatlm2)
# output:
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.111668   0.013579  81.867  < 2e-16 ***
#   wrist        0.005611   0.000924   6.073 4.67e-09 ***
#   abdomen     -0.001711   0.000080 -21.391  < 2e-16 ***

# still highly significant p-value


# Question 4:
# In Section 1.19.5.2, we gave this intuitive explanation:
# In other words, the national mean height is a weighted average of the state means, with the weight for 
# each state being its proportion of the national population. Replace state by gender in the following.

# Write English prose that relates the overall mean height of people and the gender-specific mean heights.
# Write English prose that relates the overall proportion of people taller than 70 inches to the gender-specific proportions.

# Take the % of females and multiply by their mean height, take % of males and multiply by their mean height, and add them together.
# Take the % of tall (over 70 in) people and multiply by their mean height, take the % of short people (<70 in) and multiply by their 
# mean height, and add them together.


## Page 120
# Question 1:
# Consider the census data in Section 1.16.1.
# Form an approximate 95% confidence interval for β6 in the model (1.28).    
# Form an approximate 95% confidence interval for the gender effect for Master’s degree holders, β6 + β7, in the model (1.28).  

# Beta6 = fem
lmodel <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + ms:fem + phd:fem, data = prgeng)
summary(lmodel)
confint(lmodel, 'fem', level=0.95)

# output:
#         2.5 %    97.5 %
# fem -11853.68 -8699.915

confint(lmodel, 'fem', level=0.95) + confint(lmodel, 'ms:fem', level=0.95)

# output:
#        2.5 %    97.5 %
#  fem -19398.6 -9469.501


# Question 2:
# The full bikeshare dataset spans 3 years’ time. Our analyses here have only used the first year. 
# Extend the analysis in Section 2.8.5 to the full data set, adding dummy variables indicating the 
# second and third year. Form an approximate 95% confidence interval for the difference between the 
# coefficients of these two dummies.



