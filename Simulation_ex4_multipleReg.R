#-------------------------------------------------------------------------
# Simulation Exercise 4 by DVM Bishop
# 21st March 2017
# Simulating data for multiple regression
#-------------------------------------------------------------------------

# N.B. the data simulation part is based on Exercise 2
# The logic is very similar: if you include many variables in a multiple regression
# the likelihood that one of them will be 'significant' at .05 is higher than 5%
# Multiple regression gives an overall R2 value with an associated p-value
# and you will find this gives a realistic estimate of whether the combined variables
# are predictive of the outcome measure. However, it is dangerous to interpret p-value
# for individual predictors.
# Many people who use multiple regression and structural equation modeling approaches
# seem unaware of the high rate of false positive findings that can arise.

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')

# Remember! you will need to download these packages if they aren't already downloaded
library(MASS) #for mvrnorm function to make multivariate normal distributed vars
library(gridExtra) #for plotting output in a grid
library(grid) #https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

options(scipen=999) #disable scientific notation.
plot.new()
#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate 7 variables (but you could change this number)
#-------------------------------------------------------------------------

nVar<-7 #number of simulated variables (arbitrary but useful for figure that will fit on screen)
# We will treat variable 1-6 as predictors and variable 7 as dependent variable (DV)
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
myVar<-1 #Remember variance is SD^2. For z-scores, SD is 1, so variance is also 1
myN<-30 #set sample size per group (You can vary this to see the effect)
myCorr<-0.5 #correlation between 6 predictor variables
myCorr2<- c(0,0,0,0,0,0,1) #correlation between each predictor and DV 
#(the final 1 is the variance of the DV, in effect, correlation of variable with itself)

mynSims <- 1000 # Set number of simulated datasets
ptable=matrix(rep(NA,(mynSims*10)),nrow=mynSims) #initialising a matrix that will hold p values in each run
# We also have columns to denote if any p-value in a run is <.05 (anysig), or < .007 (Bonferroni corrected)
colnames(ptable)<- c("run","pX1","pX2","pX3","pX4","pX5","pX6","p.overall","anysig","Bonfsig")

# We're going to use the mvrnorm command which generates a multivariate normal distribution
# It's similar to rnorm: you specify mean and variance for each variable
# But you also need to specify how variables are intercorrelated
# mvrnorm requires you to specify the covariance matrix
# For simplicity, we'll work with zscores with mean 0 and SD 1
# This means that the correlation and covariance are the same, and the variance and SD are the same

myCov<-matrix(rep(myCorr,nVar*nVar),nrow=nVar) 
# Look at myCov after running this line; you have 7 x 7 matrix with all entries myCorr
# Now we change the final row and column to match myCorr2
myCov[nVar,]<-myCorr2  #note that we specify nVar to indicate last row, but leave 
#blank after the comma: this means we want all columns
myCov[,nVar]<-myCorr2 #note that we leave blank before comma to indicate we want 
#all rows; nVar indicates we want last column

# Look again at myCov after these two steps to check you understand the commands

# We need to fix our matrix so that the diagonal values correspond to variance of each variable
diag(myCov)<-rep(myVar,nVar) # now look at myCov again


# NB for simplicity, we've made all intercorrelations the same, but you could hand craft your matrix
# to have different correlations between different variables. 

#----------------------------------------------------------------------------------------
for (i in 1:mynSims){ # 'for loop' command: runs loop mynSims times
  # Each time the code between { and } is run, the index, i, increases by one
  #N.B. if running code one line at a time, any line with { in it will 
  # not execute until a corresponding } has been found (or entered on console)
  
  #----------------------------------------------------------------------------------------
  # Generate a sample from a multivariate normal distribution with the specified correlation matrix
  #----------------------------------------------------------------------------------------
  
  mydata<-data.frame(mvrnorm(n = myN, rep(myM,nVar), myCov))
  colnames(mydata)<-paste(rep("X",nVar),c(1:nVar),sep="") 
  colnames(mydata)[nVar]<-'Y'
  #this is a clever way to assign sequential column names, X1, X2, ....etc .
  #----------------------------------------------------------------------------------------
 # Now run a multiple regression
  # see http://www.statmethods.net/stats/regression.html
 #----------------------------------------------------------------------------------------
 
  fit <- lm(Y ~ X1 + X2 + X3 +X4+X5+X6, data=mydata)
  mysummary<-summary(fit)
  ptable[i,1]<-1
  ptable[i,2:7] <-mysummary$coefficients[2:7,4] #extract the p-values 
  #We can also get the overall significance when all predictors included
  myfstat<-mysummary$fstatistic #gives F ratio, df1, df2 in a vector
  ptable[i,8]<-1-pf(myfstat[1],myfstat[2],myfstat[3]) #gives corresponding p value
  ptable[i,9:10]<-0 #initialise col 9-10 which will categorise each run in terms of whether *any* sig effects
  sigp<-which(ptable[i,2:7]<.05) #find whether any p-values are < .05
  if(length(sigp)>0)
    {ptable[i,9]<-1} # if so, assign a 1 to column 9
  sigp<-which(ptable[i,2:7]<.008) #now do the same with p < .008, i.e. .05/6
  if(length(sigp)>0)
  {ptable[i,10]<-1} #result is stored in column 10
  # diagnostic plots - optional step - see http://www.statmethods.net/stats/regression.html
  # layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page
  # plot(fit)
 
  
  } #repeat for next simulation

percent05<-100*(sum(ptable[,9])/mynSims) 
percentBon<-100*(sum(ptable[,10])/mynSims)
pptable<-round(ptable,3) #round to 3 decimal places
grid.table(head(pptable)) #head gives just the first few rows of output

paste('% analyses with one sigificant predictor < .05 = ',percent05)
paste('% analyses with one significant predictor < .007 = ',percentBon)
  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
overallNsig<-length(which(ptable[,8]<.05)) 
paste('% analyses with overall sig prediction =',100*overallNsig/mynSims)
  
#   

# Try setting myCorr to zero, and the first 3 values of myCorr2 to .8
# What happens? See if you can work out why!

# Further exercise: Look at impact of high intercorrelations between predictor
# on the results - this is known as collinearity and is a recognised problem
# in multiple regression.
# For more on regression diagnostics in R see http://www.statmethods.net/stats/rdiagnostics.html

