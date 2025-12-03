###  Alex Ruvolo
###
###  484/584 Intro to ML
###        Assignment 6 on Ch10 Neural Network
###
####################################################
##  Due Wed Nov 06 11:00pm
##
##
##  Write R code that performs tasks listed below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignment 6".


# 1. ----------------
#    Load concrete.csv from https://nmimoto.github.io/datasets/ as a tibble.
#    Rename "CCS" column as "resp".
library(MASS)
library(tidyverse)
Concrete <- read_csv("https://nmimoto.github.io/datasets/concrete.csv")
Concrete

Concrete <- as_tibble(Concrete)
Concrete

Concrete2 <- Concrete %>% rename(resp=CCS) %>% relocate(resp)
Concrete2

Concrete5 <- Concrete2 %>% mutate_each(base::scale)
Concrete5

# Same as above, but this way you keep means and SD for unscaling
Concrete.means <- Concrete2 %>% mutate_each(mean)
Concrete.means
Concrete.SDs   <- Concrete2 %>% mutate_each(sd)
Concrete.SDs
Concrete6 <- tibble((Concrete2 - Concrete.means)/Concrete.SDs)
Concrete6

Orig <- Concrete6

#- Check for N/A in data. Remove if there's any.
summary(Orig)
sum(is.na(Orig))
# If there is na in the data, run below
dim(Orig)
Orig <- Orig %>% na.omit()
dim(Orig)


# 2. Using seed "2356", prepare the dataset for training/test fit in (3), using
#    source("https://nmimoto.github.io/R/ML-00.txt") and CreateCV() function.
#    Make sure you are using unscaled data.
#    What is the size of training set?  What is the size of test set?

source("https://nmimoto.github.io/R/ML-00.txt")   # load CreateCV()

# Use this function to create CV dataset
CreateCV(Concrete2, numFolds=5, seed=2356) #unscaled
my.seed = 2356

#The size of each training fold is 171.
#The size of the test set is 175.

# 3. ----------------
#    Perform Training/Test fit of multiple regression with all predictors in the model.
#    Use all training set to fit, and predict the response variable in the test set.
#    Report training RMSE and test RMSE.

Fit01 = lm(resp ~. , data=Train.set)
summary(Fit01)

# get training / validation fit
Train.fitted = predict(Fit01, newdata = Train.set)
Test.pred    = predict(Fit01, newdata = Test.set)

library(caret)          
OLS <- data.frame(
  RMSE.tr      = caret::RMSE(Train.fitted, as.matrix(Train.resp)),
  Rsquare.tr   = caret::R2(  Train.fitted,           Train.resp),
  RMSE.test    = caret::RMSE(Test.pred,    as.matrix(Test.resp)),
  Rsquare.test = caret::R2(  Test.pred,              Test.resp)
)
OLS

#Training RMSE is 10.3715.
#Test RMSE is 10.3440.

# 4a. ----------------
#    Using seed "2356", prepare the dataset for 5-fold CV, using
#    source("https://nmimoto.github.io/R/ML-00.txt") and CreateCV() function.
#    Make sure you are using scaled data.
#    What is the size of each fold?  What is the size of test set?

CreateCV(Orig, numFolds=5, seed=2356) #scaled

#The size of each fold is 171 observations.
#The size of the test set is 175 observations.

# 4b. ----------------
#    Using Cross Validation, fit Neural Network model with
#    all variables in the data set, using 1 hidden layer with 3 nodes.
#    Make sure the data set you feed into NN is scaled.
#    Report av. training RMSE and av. validation RMSE.

library(neuralnet)            # install.packages('neuralnet', repos='https://cran.case.edu/')
sigmoid <- function(x) 1 / (1 + exp(-x))

layout(matrix(1:6, 2, 3, byrow=TRUE))    # to plot 5 in 1 page
CVFitDiagnosis <- numeric(0)
for (k in 1:5) {
  
  set.seed(my.seed)
  Fit00 = neuralnet::neuralnet(resp ~.,            # <===== Try using different set of columns here
                               CV.train[[k]],
                               hidden=3,             # <===== Try different num of nodes here
                               learningrate=1e-2,    # <===== For numerical problem try changing number here
                               act.fct=sigmoid,
                               linear.output=TRUE)   # This should be TRUE for regression
  # linear.output FALSE means activation function is applied to output node
  # summary(Fit00)
  # plot(Fit00)  # Use only if NN is small enough (hidden <10)
  
  #--- Get training / validation fit
  Train.fitted = predict(Fit00, newdata=CV.train[[k]], type="vector")
  Valid.fitted = predict(Fit00, newdata=CV.valid[[k]], type="vector")
  
  #--- Plot Y vs Yhat
  plot( Train.fitted, as.matrix(CV.train[[k]]$resp), xlab="Fitted", ylab="Actual",main=paste("K=",k))
  lines(Valid.fitted, as.matrix(CV.valid[[k]]$resp), type="p", xlab="Fitted", ylab="Actual", col="red", pch=20)
  abline(0,1)
  
  library(caret)            # install.packages("caret")
  CVFitDiagnosis1 <- data.frame(
    tr.RMSE   = caret::RMSE(Train.fitted, as.matrix(CV.train[[k]]$resp)),
    tr.Rsquare  = caret::R2(Train.fitted,           CV.train[[k]]$resp),
    val.RMSE  = caret::RMSE(Valid.fitted, as.matrix(CV.valid[[k]]$resp)),
    val.Rsquare = caret::R2(Valid.fitted,           CV.valid[[k]]$resp)
  )
  CVFitDiagnosis <- rbind(CVFitDiagnosis, CVFitDiagnosis1)
}
layout(1)

CVFitDiagnosis
Av.CVFitDiagnosis = apply(CVFitDiagnosis, 2, mean)
Av.CVFitDiagnosis

#The Average Training RMSE is 0.3539.
#The Average Validation RMSE is 0.3756.


# 5. ----------------
#    Look for a better model by changing the number of hidden layers
#    and number of nodes. (Use CV, and use av. valid. RMSE as measure of fit.)

sigmoid <- function(x) 1 / (1 + exp(-x))

layout(matrix(1:6, 2, 3, byrow=TRUE))    # to plot 5 in 1 page
CVFitDiagnosis <- numeric(0)
for (k in 1:5) {
  
  set.seed(my.seed)
  Fit00 = neuralnet::neuralnet(resp ~.,            # <===== Try using different set of columns here
                               CV.train[[k]],
                               hidden=0,             # <===== Try different num of nodes here
                               learningrate=1e-2,    # <===== For numerical problem try changing number here
                               act.fct=sigmoid,
                               linear.output=TRUE)   # This should be TRUE for regression
  # linear.output FALSE means activation function is applied to output node
  # summary(Fit00)
  # plot(Fit00)  # Use only if NN is small enough (hidden <10)
  
  #--- Get training / validation fit
  Train.fitted = predict(Fit00, newdata=CV.train[[k]], type="vector")
  Valid.fitted = predict(Fit00, newdata=CV.valid[[k]], type="vector")
  
  #--- Plot Y vs Yhat
  plot( Train.fitted, as.matrix(CV.train[[k]]$resp), xlab="Fitted", ylab="Actual",main=paste("K=",k))
  lines(Valid.fitted, as.matrix(CV.valid[[k]]$resp), type="p", xlab="Fitted", ylab="Actual", col="red", pch=20)
  abline(0,1)
  
  library(caret)            # install.packages("caret")
  CVFitDiagnosis1 <- data.frame(
    tr.RMSE   = caret::RMSE(Train.fitted, as.matrix(CV.train[[k]]$resp)),
    tr.Rsquare  = caret::R2(Train.fitted,           CV.train[[k]]$resp),
    val.RMSE  = caret::RMSE(Valid.fitted, as.matrix(CV.valid[[k]]$resp)),
    val.Rsquare = caret::R2(Valid.fitted,           CV.valid[[k]]$resp)
  )
  CVFitDiagnosis <- rbind(CVFitDiagnosis, CVFitDiagnosis1)
}
layout(1)

CVFitDiagnosis
Av.CVFitDiagnosis = apply(CVFitDiagnosis, 2, mean)
Av.CVFitDiagnosis

#Average Training RMSE is 0.6198.
#Average Validation RMSE is 0.6282.

# 6. ----------------
#    Perform the final Training/Test fit using the model in (5).
#    Report training RMSE and test RMSE.

sigmoid <- function(x) 1 / (1 + exp(-x))

set.seed(my.seed)
Fit01 = neuralnet::neuralnet(resp ~.,
                             Train.set,
                             hidden=0,
                             learningrate=1e-2,
                             act.fct=sigmoid,
                             linear.output=TRUE)
# linear.output FALSE means activation function is applied to output node

summary(Fit01)
# plot(Fit01)  # Use only if NN is small enough (hidden <10)

#--- Get training / validation fit
Train.fitted = predict(Fit01, newdata=Train.set, type="vector")
Test.fitted  = predict(Fit01, newdata=Test.set, type="vector")

#--- Plot Y vs Yhat
plot( Train.fitted, as.matrix(Train.set$resp), xlab="Fitted", ylab="Actual",main="Final Test.set Fit")
lines(Test.fitted,  as.matrix(Test.set$resp), type="p", xlab="Fitted", ylab="Actual", col="red", pch=20)
abline(0,1)

library(caret)            # install.packages("caret")
FinalFitDiagnosis <- data.frame(
  tr.RMSE   = caret::RMSE(Train.fitted,  as.matrix(Train.set$resp)),
  tr.Rsquare  = caret::R2(Train.fitted,            Train.set$resp),
  test.RMSE  = caret::RMSE(Test.fitted,  as.matrix(Test.set$resp)),
  test.Rsquare = caret::R2(Test.fitted,            Test.set$resp)
)
FinalFitDiagnosis

#Average Training RMSE is 0.6208.
#Average Test RMSE is 0.6192.

# 7. ----------------
#    How many parameters does your NN model have?
#    If feasible, plot the model with parameters.
#    If not, print out the parameters inside the NN on the screen.

summary(Fit01)
plot(Fit01)

#The NN model has 8 parameters.

# 8. ----------------
#    Scale back the predicted Test set response of (6), and
#    re-calculate the test RMSE in the original unit. Compare it to the test RMSE from (3).
#    Which model has more prediction power?
#    Which model is easier to interpret?

Train.fitted = predict(Fit01, newdata=Train.set, type="vector")
Test.fitted = predict(Fit01, newdata=Test.set, type="vector")
Train.resp = Train.set$resp
Test.resp = Test.set$resp

Train.fitted.unscaled <- Concrete.means$resp[1] + Train.fitted * Concrete.SDs$resp[1]
Train.resp.unscaled   <- Concrete.means$resp[1] + Train.resp   * Concrete.SDs$resp[1]
Test.fitted.unscaled  <- Concrete.means$resp[1] + Test.fitted  * Concrete.SDs$resp[1]
Test.resp.unscaled    <- Concrete.means$resp[1] + Test.resp    * Concrete.SDs$resp[1]

plot( Train.fitted.unscaled, as.matrix(Train.resp.unscaled), xlab="Fitted", ylab="Actual",main="Final Test.set fit - Unscaled")
lines(Test.fitted.unscaled,  as.matrix(Test.resp.unscaled), type="p", xlab="Fitted", ylab="Actual", col="red", pch=20)
abline(0,1)

library(caret)            # install.packages("caret")
FinalFitDiagnosis.unsc <- data.frame(
  tr.RMSE   = caret::RMSE(Train.fitted.unscaled, as.matrix(Train.resp.unscaled)),
  tr.Rsquare  = caret::R2(Train.fitted.unscaled,           Train.resp.unscaled),
  test.RMSE  = caret::RMSE(Test.fitted.unscaled,  as.matrix(Test.resp.unscaled)),
  test.Rsquare = caret::R2(Test.fitted.unscaled,            Test.resp.unscaled)
)
FinalFitDiagnosis.unsc

#The Training RMSE is 10.3715.
#The Test RMSE is 10.3438.

#The test RMSE from the regression fit with unscaled data is is 10.3440, which is slightly better than the test RMSE here which is 10.3438.
#Since 10.3440 > 10.3438, the regression model has more prediction power than the scaled back model.
#The regression model is easier to interpret because the coefficients make interpretations and relationships very clear, while neural networks are considered very complex and difficult to interpret.
