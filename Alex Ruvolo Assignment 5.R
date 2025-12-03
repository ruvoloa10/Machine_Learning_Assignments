###
###
###  484/584 Intro to ML
###        Assignemnt 5 on Ch8 Decision Tree and
###                        Ch9 SVM
####################################################
##  Due Wed Oct 23 11:00pm
##
##
##  Write R code that performs tasks lited below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignemnt 5".


##  Refer to Iris Data section of the class webpage.
##  Ch9-Lab3 goes over the Prelim and Logistic Regression for Iris Data.


# 1. ----------------
#    Load iris dataset that comes with R.
#    (Refer to Prelim file under Iris Data section on Webpage)
#    Turn the dataset into tibble.

library(datasets)    
data(iris)
summary(iris)

library(tidyverse)            
Iris <- as_tibble(iris)
Iris


# 2. ----------------
#    In the dataset, mutate entries of "Species" column as "Yes" if Species were
#    equal to "versicolor", and as "No" otherwise.
#    Rename "Species" column as "resp" and move it to the 1st column.

Iris2 <- Iris %>%
  mutate(Species=ifelse(Species=="versicolor", "Yes", "No")) %>%
  mutate(Species=as.factor(Species)) %>%
  rename(resp=Species) %>%     # Rename "Species" column as "resp"
  relocate(resp)               # move "resp" columnm to 1st
Iris2

# 3. ----------------
#    Using seed "1234", prepare the dataset for 5-fold cross validation using
#    source("https://nmimoto.github.io/R/ML-00.txt") and CreateCV() function.
#    What is the size of each fold?  What is the size of test set?

Orig <- Iris2

summary(Orig)
sum(is.na(Orig))
dim(Orig)
Orig <- Orig %>% na.omit()
dim(Orig)

Orig <- Orig             
source("https://nmimoto.github.io/R/ML-00.txt")
CreateCV(Orig, numFolds = 5, seed = 1234)

#Each fold has a size of 25 observations 
#The size of the test set is 25 observations

# 4. ----------------
#    Fit the "resp" column with Logistic Regression using all remaining columns
#    as a predictor in the model .
#    There's no need to use CV training/validation, just Training / Testing fit is fine.
#    This is our 'base model'.  What is the test AUC of the model?

Fit01 <- glm(resp ~., family=binomial, data=Train.set )
summary(Fit01)
coef(Fit01)

###---
Fit00 = Fit01

#- Extract fitted response (training)
Train.prob =predict(Fit00, type ="response")
head(Train.prob)

#- Predict in Test Set
Test.prob = predict(Fit00, newdata=Test.set, type="response")
head(Test.prob)

library(pROC)
#- Training Set
plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.train[["byClass"]][["Sensitivity"]], v=CM.train[["byClass"]][["Specificity"]], col="red")
auc.train = auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Train AUC=",round(auc.train, 3)))

#- Test Set
plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
# point corresponding to CM.test
abline(h=CM.test[["byClass"]][["Sensitivity"]], v=CM.test[["byClass"]][["Specificity"]], col="red")
auc.test = auc(factor(as.matrix(Test.resp)), Test.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Test AUC=",round(auc.test, 3)))

c(auc.train, auc.test)

#The test AUC of this model is 0.8810.

# 5. ----------------
#    Repeat (4) using Decision Tree model (Grow and Prune).
#    No need to use CV. Just Training / Testing fit.  How many
#    terminal node is in your tree?  What is the test AUC?
#    Any improvement over our base model?

library(tree)
tree1 = tree(resp~., Train.set)
summary(tree1)
tree1

plot(tree1)
text(tree1, pretty=0, cex=1)

Train.prob = predict(tree1, type="vector")[,"Yes"]
Train.pred = ifelse(Train.prob > threshold, "Yes", "No")
Test.prob  = predict(tree1, Test.set, type="vector")[,"Yes"]
Test.pred  = ifelse(Test.prob > threshold, "Yes", "No")

set.seed(my.seed)
cv.for.pruning = cv.tree(tree1, FUN=prune.misclass, K=5)   
names(cv.for.pruning)

plot(cv.for.pruning$size, cv.for.pruning$dev, type="b")
plot(cv.for.pruning$k,    cv.for.pruning$dev, type="b")
cv.for.pruning

pruned1 = prune.tree(tree1, best=3)
plot(pruned1)
text(pruned1, pretty=0, cex=1)


Chosen.model <- pruned1  # tree1 (unpruned) or pruned1
threshold = .6   # pick a threshold


library(caret)     # install.packages('caret', repos='https://cran.case.edu/')
Train.prob = predict(Chosen.model, type="vector")[,"Yes"]
Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  # Turn the fitted values to Up/Down using the threshold
Test.prob  = predict(Chosen.model, Test.set, type="vector")[,"Yes"]
Test.pred  = ifelse(Test.prob > threshold, "Yes", "No")
CM.train <- caret::confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
CM.test <- caret::confusionMatrix(factor(Test.pred), factor(as.matrix(Test.resp)), positive="Yes")

CM.train            # Training set result
CM.train$table      # output just the table

CM.train[["byClass"]][["Sensitivity"]]
CM.train[["byClass"]][["Specificity"]]

CM.test             # Testing set
CM.test$table      # output just the table

colSums(CM.test$table) / sum(colSums(CM.test$table))    # % of Actual Yes/No
rowSums(CM.test$table) / sum(rowSums(CM.test$table))    # % of predicted Yes/No


library(pROC)
#- Training Set
plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.train[["byClass"]][["Sensitivity"]], v=CM.train[["byClass"]][["Specificity"]], col="red")
auc.train = auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Train AUC=",round(auc.train, 3)))

#- Test Set
plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
# point corresponding to CM.test
abline(h=CM.test[["byClass"]][["Sensitivity"]], v=CM.test[["byClass"]][["Specificity"]], col="red")
auc.test = auc(factor(as.matrix(Test.resp)), Test.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Test AUC=",round(auc.test, 3)))

GrowAndPrune = c(auc.train, auc.test)
GrowAndPrune

# Training ROC and Test ROC side by side
layout(matrix(1:2, 1, 2))
plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Train AUC=",round(auc.train, 3)))
plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
text(.2, .2, paste("Test AUC=",round(auc.test, 3)))
layout(1)

GrowAndPrune

#There are 3 terminal nodes on this tree.
#The test AUC is 0.9444.
#There is an improvement compared to the base model with a test AUC of 0.9444 compared to the 0.8810 test AUC of the base model.

# 6. ----------------
#    Repeat (4) using Support Vector Machine with Linear Kernel, and Radial Kernel.
#    Use tune() function with Auto 5-fold CV to find the best parameter values
#    for each kernel. List parameter values here. What is the test AUC?

library(e1071)
Fit01 <- e1071::svm(resp~., data=Train.set, kernel="linear",  cost=5, scale=FALSE)
summary(Fit01)

set.seed (1234)
Tuned01 = e1071::tune(svm, resp~., data=Train.set, kernel ="linear",
                      ranges=list(
                        cost=c(0.0001, 0.001, 0.01, 0.1, 1, 10)),
                      scale=FALSE,
                      tunecontrol=tune.control(cross=5))
summary(Tuned01)

summary(Tuned01$best.model)

# Final Fit with Training Set vs Test Set using the best model
Final.fit   <- Tuned01$best.model   # results from CV tuning


#- Training Confusion Matrix
Train.prob0 = predict(Final.fit, Train.set, decision.values=TRUE)
Train.prob = attributes(Train.prob0)$decision.values
head(Train.prob) # Check col name for No/Yes or Yes/No

#- Testing Confusion Matrix
Test.prob0 = predict(Final.fit, Test.set, decision.values=TRUE)
Test.prob = attributes(Test.prob0)$decision.values
head(Train.prob) # Check col name for No/Yes or Yes/No

#- Pick a threshold
threshold <- .5

Train.pred = ifelse(Train.prob > threshold, "No", "Yes")
#Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  #-
Test.pred = ifelse(Test.prob > threshold, "No", "Yes")
#Test.pred = ifelse(Test.prob > threshold, "Yes", "No")

CM.train <- caret::confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
CM.train

CM.test <- caret::confusionMatrix(factor(Test.pred), factor(as.matrix(Test.resp)), positive="Yes")
CM.test

#- ROC curve and AUC
layout(matrix(1:2,1,2))
pROC::plot.roc(factor(as.matrix(Train.resp)), as.vector(Train.prob), levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.train[["byClass"]][["Sensitivity"]], v=1-CM.train[["byClass"]][["Specificity"]], col="red")
auc.train = pROC::auc(factor(as.matrix(Train.resp)), as.vector(Train.prob), levels=c("No", "Yes"))
text(.2, .2, paste("AUC=",round(auc.train, 3)))

#- Testing
pROC::plot.roc(factor(as.matrix(Test.resp)),
               as.vector(Test.prob), levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.test[["byClass"]][["Sensitivity"]], v=1-CM.test[["byClass"]][["Specificity"]], col="red")
auc.test = pROC::auc(factor(as.matrix(Test.resp)), as.vector(Test.prob), levels=c("No", "Yes"))
text(.2, .2, paste("AUC=",round(auc.test, 3)))
layout(1)

c(auc.train, auc.test)


library (e1071)   

# Pick best Cost with automatic 5-fold CV
set.seed (1234)
Tuned01 = e1071::tune(svm, resp~., data=Train.set, kernel ="radial",
                      ranges=list(
                        gamma = 2^(-1:4),
                        cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)),
                      scale=TRUE,
                      tunecontrol=tune.control(cross=5))
summary(Tuned01)

#--- Extract the model with best cost
summary(Tuned01$best.model)
# plot(resp~., Tuned01$best.model, data=Train.set)


#------------------
# Final Fit with Training Set vs Test Set using the best model
Final.fit   <- Tuned01$best.model   # results from CV tuning


#- Training Confusion Matrix
Train.prob0 = predict(Final.fit, Train.set, decision.values=TRUE)
Train.prob = attributes(Train.prob0)$decision.values
head(Train.prob) # Check col name for No/Yes or Yes/No

#- Testing Confusion Matrix
Test.prob0 = predict(Final.fit, Test.set, decision.values=TRUE)
Test.prob = attributes(Test.prob0)$decision.values
head(Train.prob) # Check col name for No/Yes or Yes/No

#- Pick a threshold
threshold <- .5

Train.pred = ifelse(Train.prob > threshold, "No", "Yes")
#Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  #-
Test.pred = ifelse(Test.prob > threshold, "No", "Yes")
#Test.pred = ifelse(Test.prob > threshold, "Yes", "No")

CM.train <- caret::confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
CM.train

CM.test <- caret::confusionMatrix(factor(Test.pred), factor(as.matrix(Test.resp)), positive="Yes")
CM.test

#- ROC curve and AUC
layout(matrix(1:2,1,2))
pROC::plot.roc(factor(as.matrix(Train.resp)), as.vector(Train.prob), levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.train[["byClass"]][["Sensitivity"]], v=1-CM.train[["byClass"]][["Specificity"]], col="red")
auc.train = pROC::auc(factor(as.matrix(Train.resp)), as.vector(Train.prob), levels=c("No", "Yes"))
text(.2, .2, paste("AUC=",round(auc.train, 3)))

#- Testing
pROC::plot.roc(factor(as.matrix(Test.resp)),
               as.vector(Test.prob), levels=c("No", "Yes"))
# point corresponding to CM.train
abline(h=CM.test[["byClass"]][["Sensitivity"]], v=1-CM.test[["byClass"]][["Specificity"]], col="red")
auc.test = pROC::auc(factor(as.matrix(Test.resp)), as.vector(Test.prob), levels=c("No", "Yes"))
text(.2, .2, paste("AUC=",round(auc.test, 3)))
layout(1)

c(auc.train, auc.test)

#Best parameter value for linear is cost = 1.
#The test AUC is 0.833 for linear.
#Best parameter value for radial is gamma = 1 and cost = 5. 
#The test AUC is 0.984 for radial.


# 7. ----------------
#    Out of the models you fit in this assignment, which one is the best model,
#    and why?

#Out of the models fit in this assignment, the best model is the radial SVM model because it has the highest test AUC of 0.984.

# 8. ----------------
#    For your best model chosen in (7),
#    Choose your cost function for the confusion matrix, and suggest the threshold value to use.
#    Copy the test confusion matrix for your chosen threshold here

cost.list = c(0,0,3,1)/4          # order of (TP, TN, FP, FN)

threshold.list = seq(0.01,.99,.01)    
cost=0
library(caret)      
for (i in 1:length(threshold.list)){
  
  threshold = threshold.list[i]
  
  Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
  CM.test  <- confusionMatrix(factor(Test.pred),
                              factor(as.matrix(Test.resp)),
                              positive="Yes")
  TP = CM.test$table[2,2]   # True  Pos
  TN = CM.test$table[1,1]   # True  Neg
  FP = CM.test$table[2,1]   # False Pos
  FN = CM.test$table[1,2]   # False Neg
  
  cost[i] = sum(c(TP, TN, FP, FN) * cost.list)
}
plot(threshold.list, cost, xlab="threshold")

cost.list
which.min(cost)
min(cost)
threshold.list[which.min(cost)]

threshold = .99

# Pick best Cost with automatic 5-fold CV
set.seed (1234)
Tuned01 = e1071::tune(svm, resp~., data=Train.set, kernel ="radial",
                      ranges=list(
                        gamma = 2^(-1:4),
                        cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)),
                      scale=TRUE,
                      tunecontrol=tune.control(cross=5))
summary(Tuned01)

#--- Extract the model with best cost
summary(Tuned01$best.model)
# plot(resp~., Tuned01$best.model, data=Train.set)


#------------------
# Final Fit with Training Set vs Test Set using the best model
Final.fit   <- Tuned01$best.model   # results from CV tuning

#- Testing Confusion Matrix
Test.prob0 = predict(Final.fit, Test.set, decision.values=TRUE)
Test.prob = attributes(Test.prob0)$decision.values
head(Train.prob) # Check col name for No/Yes or Yes/No

#- Pick a threshold
threshold <- .99

Test.pred = ifelse(Test.prob > threshold, "No", "Yes")
#Test.pred = ifelse(Test.prob > threshold, "Yes", "No")

CM.test <- caret::confusionMatrix(factor(Test.pred), factor(as.matrix(Test.resp)), positive="Yes")
CM.test

#I recommend using a threshold of 0.99 because it is the lowest value for the threshold and cost function.