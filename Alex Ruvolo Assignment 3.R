###  Alex Ruvolo
###
###  484/584 Intro to ML
###        Assignemnt 3
###
####################################################
##  Due Wed Sep 18, 2024 11:00pm
##
##
##  Write R code that performs tasks listed below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignemnt 3".
##
##  Refer to Intro-R Cross Validation video.
##  This assignment can be completed by changing few lines of code in
##  Ch3-lab



# 1. ----------------
#   Load concrete.csv from https://nmimoto.github.io/datasets/
#   using the command below.  There's no link on the page, and the file must be loaded directly.
#   Make sure you are using "tidyverse" package and function read_csv() instead of read.csv().
    
    library('tidyverse')
    x = "C:\Users\alexr\Downloads\adult.zip"
    concrete = read_csv(x)
    head(concrete)
    concrete <- as_tibble(concrete)
    concrete

#   In this assignment, we will fit CCS column with polynomial regression using Age as predictor.
#   In Ch3-Lab example, "medv" was the response variable, and "lstat" was the predictor.
#   This means that all "$medv" in Ch3-Lab should be replaced with "CCS", and all "lstat"
#   should be replaced with "Age".

    concrete = concrete %>% rename(resp = CCS) %>% relocate(resp)

# 2. ----------------
#    Using seed "8346", prepare the dataset for 5-fold cross validation using
#    source("https://nmimoto.github.io/R/ML-00.txt") and CreateCV() function.
#    What is the size of each fold?  What is the size of test set?
   
    attach(concrete)
    source("https://nmimoto.github.io/R/ML-00.txt")
    CreateCV(concrete, numFolds = 5, seed = 8346)
    
    #Size of each fold: 171
    #Size of test set: 175

# 3. ----------------
#    Using k=3 for the k-fold CV, plot scatterplot of Age vs CCS.
#    Plot Training set with open black circle, and Validation set with solid red circle.
#    (You can use section 3a of Ch3-Lab)
    
    fit1 <- lm( resp ~ poly(Age, deg.poly), data=CV.train[[k]] )
    summary(Fit01)
    k = 3
    plot(CV.train[[k]]$Age,  CV.train[[k]]$resp, xlab="X", ylab="Y")
    lines(CV.valid[[k]]$Age, CV.valid[[k]]$resp, col="red", type="p", pch=19)
    ix = sort(CV.train[[k]]$Age, index.return=TRUE)$ix
    lines(CV.train[[k]]$Age[ix], fit1$fitted[ix], lwd=2, col="black" )


# 4. ----------------
#    For each of the k-fold, (k=1,...5), fit CCS using 3rd degree polynomial of Age.
#    (You can use (section 3.a for k=1,...5) or (section 3.b for deg.poly=3)
#    List values of Training MSE and Validation MSE for each fold.
#    How many observations were there in each training set?
#    How many observations were there in each validation set?
    
    dev.off()
  
    deg.poly = 3
    MSE.train <- MSE.valid <- matrix(0, 5, 10)
    
      for (k in 1:5) {
        
        Fit01 <- lm( resp ~ poly(Age, deg.poly), data=CV.train[[k]])
        summary(Fit01)
        
        
        MSE.train[k, deg.poly] <- mean(Fit01$residuals^2)
        
        
        Fit01.pred<- predict(Fit01, newdata=CV.valid[[k]])
        MSE.valid[k, deg.poly] <- mean((CV.valid[[k]]$resp - Fit01.pred)^2)
      }
    
    MSE.train
    MSE.valid
    
    # k = 1 Train MSE = 191.56  Valid MSE = 200.99
    # k = 2 Train MSE = 192.11  Valid MSE = 199.72
    # k = 3 Train MSE = 187.17  Valid MSE = 218.46
    # k = 4 Train MSE = 193.08  Valid MSE = 195.08
    # k = 5 Train MSE = 202.38  Valid MSE = 157.89
    # Each Training Set has 684 observations
    # Each Validation Set has 171 observations
    
    
    # 5. ----------------
#    Produce deg.poly vs MSE.valid plot, for CCS vs Polynomial of Age.
#    (You can use section 3.b)
#    Using the plot, decide on the best value of deg.poly. Comment on why you chose this specific value.
    
    MSE.train <- MSE.valid <- matrix(0, 5, 10)
    for (deg.poly in 1:10) {
      for (k in 1:5) {
        
        Fit01 <- lm( resp ~ poly(Age, deg.poly), data=CV.train[[k]])
        summary(Fit01)
        
        MSE.train[k, deg.poly] <- mean(Fit01$residuals^2)
        
        Fit01.pred<- predict(Fit01, newdata=CV.valid[[k]])
        MSE.valid[k, deg.poly] <- mean((CV.valid[[k]]$resp - Fit01.pred)^2)
      }
    }
    MSE.train
    MSE.valid
    
    Av.MSE.train = apply(MSE.train, 2, mean)
    Av.MSE.valid = apply(MSE.valid, 2, mean)
    
    plot(Av.MSE.train, type="o", ylab="MSE")
    
    lines(Av.MSE.valid, type="o", col="red")
    
    legend(7, 240, lty=1, c("Av. Train MSE","Av. Valid MSE"), col=c("black", "red"))
    
    #The best value of deg.poly is 4. 
    #I chose this because the average valid MSE is relatively low, and also the degree is relatively low. 
    #Plus, the MSE does not change much for higher degrees, so not much is gained from choosing a higher degree.

# 6. ----------------
#    Perform the final test fit using the test set.
#    How many observations were in the training set?
#    How many were in the test set?
#    What is the final test MSE?
#    Is the test MSE comparable to the av. of validation MSE and av. of CV training MSE?
    
    deg.poly = 4
    Fit2 <- lm( resp ~ poly(Age, deg.poly), data=Train.set)
    summary(Fit2)
    
    MSE.train <- mean(Fit2$residuals^2)
    
    pred<- predict(Fit2, newdata=Test.set)
    MSE.test <- mean((Test.set$resp - pred)^2)
    
    c(MSE.train, MSE.test)
    
    c(Av.MSE.train[4], Av.MSE.valid[4]) 
    
    #There were 855 observations in the training set
    #There were 175 observations in the test set
    #Final Test MSE = 167.73
    #The test MSE is not very comparable to the other two MSEs with around 167 compared to around 184 and 186.
    
# 7. ----------------
#    Suppose you are to obtain new set of predictors, (n=150) and use them with the final
#    model in (7) to predict the response variable CSS.  After the prediction, actual values
#    of CSS will be obtained, and the prediction errors are calculated in terms of MSE.
#    What is your best estimate about the value of the prediction MSE?
     
     #My best estimate for the value of the prediction MSE is around 185 because it falls right around the training MSE and validation MSE.

# 8. ----------------
#    What is the mathematical equation of the final polynomial? (type in this file)
     
     summary(Fit2)
     # y = 35.87 + 158.19x - 186.53x^2 + 123.61x^3 - 86.15x^4 