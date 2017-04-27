#-------------------------------------------------------------------------
# Simulation Exercise 6 by DVM Bishop
# 25th March 2017
# Simulating data for discriminant function analysis (DFA)
#-------------------------------------------------------------------------
# Discriminant function analysis is a method for categorising a sample into 2 or more
# groups on the basis of measured variables.
# Here we focus on the 2-group classification
# E.g you might want to categorise people in terms of pass/fail an exam on the basis
# of scores on some aptitude tests.
# DFA finds the optimal combination of measures for the classification and creates
# a new predictor variable that is a weighted sum of the predictors.
# This is used to classify those above a cutoff as group 1 and those below as group 0

# Thought experiment: before running the simulation
  # If there is no relationship between predictors and group in the population,
  # and there are two groups of equal size, how accurate do you think the DFA
  # function will be (in terms of percentage of cases correctly classified)?

# see http://www.statmethods.net/advstats/discriminant.html
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html


setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')

# Remember! you will need to download these packages if they aren't already downloaded
library(MASS) #for mvrnorm function to make multivariate normal distributed vars
library(gridExtra) #for plotting output in a grid
library(grid) #https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
library(car)
options(scipen=999) #disable scientific notation.
dev.off() #clear plots

#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate 7 variables (but you could change this number)
#-------------------------------------------------------------------------

nVar<-7 #number of simulated variables (arbitrary but useful for figure that will fit on screen)
# We will treat variable 1-6 as predictors and variable 7 as category to be predicted (DV)
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
myVar<-1 #Remember variance is SD^2. For z-scores, SD is 1, so variance is also 1
myN<-30 #set sample size per group (You can vary this to see the effect)
myCorr<-0.2 #correlation between 6 predictor variables (can also vary this)
myCorr2<- c(0,0,0,0,0,0,1) #correlation between each predictor and DV 
#the final 1 is the variance of the DV, in effect, correlation of variable with itself)
# We start the simulation assuming no relationship between predictors and group

mynSims <- 1000 # Set number of simulated datasets
myperctable=matrix(rep(NA,(mynSims*2)),nrow=mynSims) #initialising a matrix that will hold % correct for each run
colnames(myperctable)<- c("run","percentcorrclass")


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
  
  #Now we are going to change the last column to give 0 and 1 values
  mygp1<-which(mydata[,nVar,]<0)
  mydata[mygp1,nVar]<-0
  mydata[-mygp1,nVar]<-1
  # lastcolumn (nVar) gives group identity: 0 or 1; to do this make those with zscore <- 0 first group (0)
  # and the remainder 2nd group (1); this should give roughly equal Ns for 0 and 1
 # scatterplotMatrix(mydata[1:7]) #uncomment this line to see scatterplot
  #----------------------------------------------------------------------------------------
 # Now run a discriminant function analysis to predict group ID from vars X1-X6
   
 #----------------------------------------------------------------------------------------
  
  myfit <- lda(Y ~ X1 + X2 + X3 +X4+X5+X6, data=mydata)# do discrim function analysis
  mypredict <- predict(myfit)  # extract predictive scores: weighted combination of X1-X6 
                               # that gives best discrimination between groups
  mydata$predscore<-mypredict$x #discriminant score from DFA
  mydata$predgp<-mypredict$class #predicted group from DFA
  mydata$correctpred<-0 #column to indicate if prediction correct - default is zero
  temp<-which(mydata$Y==mydata$predgp) #find rows where group correctly predicted
  mydata$correctpred[temp]<-1 #set these rows to one
  if(i==mynSims){ #illustrative outputs : just do for final run
   ldahist(data = mydata$predscore, g=mydata$Y) #histogram of predictive scores vs actual group
  table(mypredict$class,mydata$Y)
  }
  myperctable[i,1]<-i #first column gives run number
  myperctable[i,2]<-100*sum(mydata$correctpred)/myN #2nd col gives percent correctly predicted
}
hist(myperctable[,2],main='Distribution of % correctly classified',xlab='% correct') #This shows the percentage correct group assignment across all simulations

# Now try varying the sample size per group (myN) and other parameters to see how
# they affect the prediction
# You can also vary myCorr2 to see what happens if there are true correlations between 
# predictor variables and the group (Y)
# Also, you could vary command on line 95 that assigns group membership so that
# the groups are of unequal size - can you work out how to do that?
  
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
