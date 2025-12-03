###  Alex Ruvolo
###
###  484/584 Intro to ML
###        Assignment 1
###
####################################################
##  Due Wed Aug 31 11:00pm
##
##
##  Write a R code that performs tasks listed below.
##  Use this text file as template. Submit on Brightspace
##  under "Assessment" -> "Assignment 1".
##
##  Refer to Intro to R-studio video list for basic idea of how to do these tasks.
##  For particular functions, you may have to Google.


#------------------------------
# 1. Write a R code that calculates the probability that (X > 4) when X is
#    a chi-squared distribution with degrees of freedom 5.
  df = 5
  p = 1 - pchisq(4, df)
  p
  
# 2. Write a R code that calculates the 78th percentile of chi-square
#    distribution with degrees of freedom 5.
  df = 5
  qchisq(0.78, df)

# 3. Generate random sample of size 1000 from chi-square distribution with
#    degrees of freedom 5.  Set seed to "1234" right before the generation.
  df = 5
  set.seed(1234)
  data = rchisq(1000, df)
  data
  
# 4. Plot histogram of above sample.
  hist(data)

# 5. Overlay theoretical pdf of Chi-square with df 5 over the histogram plot above.
#    Overlayed pdf must be in green, and must span entire x-axis of the histogram.
  df = 5
  x = seq(0, 25)
  y = dchisq(x, df)
  hist(data, freq = FALSE)
  lines(y, col = "green")

# 6. Use following code to generate 100 by 30 matrix:
set.seed(653)
D = rexp(3000, 1/5)
X = matrix(D, 100, 30)

# Write a code that looks up 3rd column, from row 10 to 20.
  X[10:20, 3]


# 7. Write a code that counts how many rows in the 25th column are greater than 5.
  a = (X[ , 25] > 5)
  a
  sum(a)

# 8. Create a new matrix Y by the following rule: If a row of Matrix X has entry
#    less than 7 on the 15th column, then all column for that row should be
#    included in new matrix Y.  What are the dimensions of Y?
  Y = X[(X[ , 15] < 7), ]
  b = dim(Y)
  b
#There are 72 rows and 30 columns
  