###
###
###  484/584 Intro to ML
###        Assignemnt 2
###
####################################################
##  Due Wed Sep 11 11:00pm
##
##
##  Write R code that performs tasks lited below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignment 2".
##
##  Refer to Intro-R Regression video.
##  For particular function, you may have to google.


# 1. ----------------
#    Load concrete.csv from https://nmimoto.github.io/datasets/.
#    There's no link on the page. Load it directly to R using source() command.
#    Make sure you are using "tidyverse" package and function read_csv() instead
#    of read.csv().
  library('tidyverse')
  x = "https://nmimoto.github.io/datasets/concrete.csv"
  concretedata = read_csv(x)
  head(data)
  
# 2. ----------------
#    What is the class of each columns?  Is there any qualitative variable?
  head(concretedata)
  str(concretedata)
  #Each column is of class dbl or double. There is not a qualitative variable.

# 3. ----------------
#    Plot scatter plot and histogram of the column "CCS".
  attach(concretedata)
  plot(CCS)
  hist(CCS)

# 4. ----------------
#    Plot scatter plot of "Water" vs "CCS", and "Age" vs "CCS".
#    Variable "CSS" has to be on the Y-axis.
  attach(concretedata)
  plot(Water, CCS)
  plot(Age, CCS)

# 5. ----------------
#    Fit multivariate regression model with OLS using lm() function.
#    CSS is the response variable.  Use all other columns as covariates in the model.
#    How good is the fit?  Any variable that should be removed from the model?
  attach(concretedata)
  Reg <- lm(CCS ~. , data = concretedata)
  summary(Reg)
  #The fit is very good with a p-value < 0.05. 
  #Coarse and Fine do not appear to be significant towards explaining the relationship with CCS, so they should be removed from the model. Also R^2 does not really change when the variables are removed from the model.

# 6. ----------------
#    In (5) above, is there any indication of non-linearity?  Data supports the
#    assumption of the regression model?  (Hint: Check residual plot)
  plot(Reg)
  #There is an indication of non-linearity. It looks okay but it still has some kind of a megaphone shape. It sort of supports the assumption of the regression model. 
    

    
  