###   Alex Ruvolo
###
###  484/584 Intro to ML
###        Assignemnt 4
###
####################################################
##  Due Wed Oct 02, 2024 11:00pm
##
##
##  Write R code that performs tasks listed below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignment 4".
##
##  Refer to  video Ch4Lab-1,2,3 and 4.
##  This assignment can be completed by changing few lines from the code found in
##  "Logistic Regression" under "ISLR Heart Data" section in the class Website.
##  (Covered in Ch4Lab-4.)


# 1. ----------------
#    Load heart.csv from https://nmimoto.github.io/datasets/.
#    There's no link on the page, and the file must be load directly.
#    Make sure you are using "tidyverse" package and function read_csv() instead of read.csv().
   
     library('tidyverse')
     x = "https://nmimoto.github.io/datasets/heart.csv"
     data = read_csv(x)
     head(data)

# 2. ----------------
#    In the dataset, remove column "X1".
#    Rename "AHD" column as "resp" and move it to the 1st column
#    Make sure following columns have class "factor": "Sex", "ChestPain", "Fbs", "RestECG", "ExAng", "Thal"
#    remove rows that has NA.
     
     attach(data)
     select(-"X1")
     data2 <- data %>% rename(resp = AHD) %>% relocate(resp) %>%              
     mutate(resp=as.factor(resp), 
            ChestPain=as.factor(ChestPain),
            Thal=as.factor(Thal),
            Sex=as.factor(Sex),
            Fbs=as.factor(Fbs),
            RestECG=as.factor(RestECG),
            ExAng=as.factor(ExAng))
     orig <- data2
     summary(orig)
     sum(is.na(orig))
     dim(orig)
     orig = orig %>% na.omit()
     dim(orig)
     

# 3. ----------------
#    Using seed "2354", prepare the dataset for 5-fold cross validation using
#    source("https://nmimoto.github.io/R/ML-00.txt") and CreateCV() function.
#    What is the size of each fold?  What is the size of test set?
     
     source("https://nmimoto.github.io/R/ML-00.txt")
     CreateCV(orig, numFolds = 5, seed = 2534)
     #The size of each fold is 49 observations
     #The size of the test set is 52 observations

# 4. ----------------
#    In Ch4Lab-4 video, only the model that takes in all the columns was investigated.
#    search for the best Logistic Regression model that gives the highest validation AUC.
     
     AUCs <- matrix(0, 5, 2)
     colnames(AUCs) = c("Train AUC", "Valid AUC")
     for (k in 1:5) {
       
       Fit00 <- glm(resp ~ Sex + ChestPain + RestBP + MaxHR + Oldpeak + Ca + Thal, family=binomial, data=CV.train[[k]])     
       
       
       Train.prob =predict(Fit00, type ="response") 
      
       Valid.prob = predict(Fit00, newdata=CV.valid[[k]], type="response")
       
    
       library(caret)
       library(pROC)
       
       
       AUCs[k,] <- round(c(auc(factor(as.matrix(CV.train.resp[[k]])), Train.prob, levels=c("No", "Yes")),
                           auc(factor(as.matrix(CV.valid.resp[[k]])), Valid.prob, levels=c("No", "Yes"))), 4)
       
     }
     AUCs
     
     Av.AUCs = apply(AUCs, 2, mean)
     names(Av.AUCs) = c("Av.Train AUC", "Av.Valid AUC")
     Av.AUCs
     
    #The best model for the highest validation AUC has Sex + ChestPain + RestBP + MaxHR + Oldpeak + Ca + Thal, and has an average validation AUC of 0.91382

# 5. ----------------
#    Using your best model from (4), perform the final Training/Test fit.
#    What is the test AUC?
#    Is your test AUC in reasonable range?

     
     ### Best model
     Fit03 <- glm(resp ~ Sex + ChestPain + RestBP + MaxHR + Oldpeak + Ca + Thal, family=binomial, data=CV.train[[k]])
     summary(Fit03)
     
    
     Fit00 = Fit03
     
    
     Train.prob =predict(Fit00, type ="response")
     head(Train.prob)
     
    
     Test.prob = predict(Fit00, newdata=Test.set, type="response")
     head(Test.prob)
     library(pROC)
  
     plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
     
     abline(h=CM.train[["byClass"]][["Sensitivity"]], v=CM.train[["byClass"]][["Specificity"]], col="red")
     auc.train = auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
     text(.2, .2, paste("Train AUC=",round(auc.train, 3)))
     
   
     plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
     
     abline(h=CM.test[["byClass"]][["Sensitivity"]], v=CM.test[["byClass"]][["Specificity"]], col="red")
     auc.test = auc(factor(as.matrix(Test.resp)), Test.prob, levels=c("No", "Yes"))
     text(.2, .2, paste("Test AUC=",round(auc.test, 3)))
     
     c(auc.train, auc.test)
     
     
     layout(matrix(1:2, 1, 2))
     plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
     text(.2, .2, paste("Train AUC=",round(auc.train, 3)))
     plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
     text(.2, .2, paste("Test AUC=",round(auc.test, 3)))
     layout(1)
     
     
     plot(AUCs[,2], col="red", ylim=c(.5,1))
     lines(AUCs[,1], type="p")
     abline(h=auc.test)
     
     AUCs
     Av.AUCs
     c(auc.train, auc.test)
     
     #The test AUC is 0.8022
     #The test AUC is not in a reasonable range of the average valid AUC

# 6. ----------------
#    For somebody who is actually using your 'best' logistic regression model
#    to predict the presense of the heart disease, recommend the value of threshold (b/w .1 - .9).
#    You must come up with your own cost function for (TP, TN, FP, FN).
     
     cost.list = c(0,0,1,4)/5          # order of (TP, TN, FP, FN)
     
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
     
     threshold = .09   
     
     library(caret)
     Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  
     Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
     CM.train <- confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
     CM.test  <- confusionMatrix(factor(Test.pred),  factor(as.matrix(Test.resp)),  positive="Yes")
     
     CM.train            
     CM.train$table      
     
     CM.train[["byClass"]][["Sensitivity"]]
     CM.train[["byClass"]][["Specificity"]]
     
     CM.test             
     CM.test$table      
     
     colSums(CM.test$table) / sum(colSums(CM.test$table))    # % of Actual Yes/No
     rowSums(CM.test$table) / sum(rowSums(CM.test$table))    # % of predicted Yes/No
     
     #I recommend the value of 0.09 for the threshold, so almost all of the people with heart disease get the needed treatment, with the cost of some people getting treatment when they do not have heart disease
     