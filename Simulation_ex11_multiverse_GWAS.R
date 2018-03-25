#-------------------------------------------------------------------------
# Based on Simulation Exercise 2 by DVM Bishop
# 25th March 2018; 
# Using Q-Q plot to look at distribution of p-values in a large set of variables
# Should help understand methods used for testing genome-wide association (GWA)
#-------------------------------------------------------------------------
# For basic intro to GWAS see 
# physiology.med.cornell.edu/people/banfelder/qbio/resources_2013/2013_1_Mezey.pdf
# http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1002822#s2

#setwd() - put your own directory here - or use Session|Set Working Directory|To Source Location

# Remember! you will need to download this package if it aren't already downloaded
library(MASS) #for mvrnorm function to make multivariate normal distributed vars

options(scipen=999) #disable scientific notation.

#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate nVar variables 
#-------------------------------------------------------------------------

nVar<-40 #number of simulated variables - can change this
#NB first variable will be our outcome measure of interest (eg phenotype);
#other variables will be predictor variables (e.g. SNPs)
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
myVar<-1 #Remember variance is SD^2. For z-scores, SD is 1, so variance is also 1
myN<-900 #set sample size per group (You should vary this to see the effect)
myCorr<-0 #correlation between variables: this specifies uncorrelated variables
sigvar<-2:6 #for these vars we will have an association with outcome
myES<-.1 #effect size as correlation between a sig variable and outcome
#in GWAS it's unusual to get large effect sizes

everyp<-data.frame(matrix(ncol = 2, nrow = nVar-1)) #initialise a dataframe to hold all the pvalues
colnames(everyp)<-c('Var','p')

  myCov<-matrix(rep(myCorr,nVar*nVar),nrow=nVar) #initialise covariance matrix
  diag(myCov)<-rep(myVar,nVar) # put one on the diagonals.
  
  #now alter r to equal myES for those variables correlated with outcome
  myCov[1,sigvar]<-myES
  myCov[sigvar,1]<-myES
  #NB we are assuming no correlation between the predictor variables
  #----------------------------------------------------------------------------------------
  # Generate a sample from a multivariate normal distribution with the specified correlation matrix
  #----------------------------------------------------------------------------------------
  
  mydata<-mvrnorm(n = myN, rep(myM,nVar), myCov)
  #we'll turn V1 into a binary categorical variable by splitting at 0
  #this is a clunky way to do this, but it show the logic
  temp<-mydata[,1]#save values in temporary vector
  mydata[,1]<-0 #default is group 0
  w<-which(temp<0) #;  identify rows with negative values
  mydata[w,1]<-1; #overwrite those rows with 1
  
  #----------------------------------------------------------------------------------------
  # Now do t-test for each variable, testing association with V1, and save the p-value
  #----------------------------------------------------------------------------------------
  for (i in 2:nVar){
    myt <- t.test(mydata[,i]~mydata[,1]) #tests for difference in means between
    #groups specified in column 1
    everyp[(i-1),1]<-i #everyp has the variable number in column 1
    everyp[(i-1),2]<-myt$p.value #and the associated p-value in column 2
  }
  lo.p<-which(everyp$p<.05) #find low pvalues
  pbit<-round(100*length(lo.p)/nrow(everyp),2) #% pvalues less than .05
  #We aren't particularly interested in this, but pbit tells you what % of
  #p-values are below .05. Can be used as sanity check.
   #--------------------------------------------------------------------------------
  # A Q-Q plot will show whether distribution of p-values departs from null expectation
  #--------------------------------------------------------------------------------
  #First we'll just add a column that allows us to display the sigvar variables in red
  everyp$colorcode<-1
  everyp$colorcode[(sigvar-1)]<-2
  
  #Now we are going to rank order the p-values according to size
  everyp<-everyp[order(everyp$p),] #rank order file according to pvalues
  np<-nrow(everyp)-1
  pexp<-seq(0.001,1.001,1/np) #expected pvalues under null hypothesis

  lpexp<-log10(pexp) #log10 transform used as we are interested in most extreme values
  #which are easier to distinguish on log scale

  everyp$lpob<- log10(everyp$p)

  #Q-Q plot is quantile-quantile plot: checks if distribution of p-values is uniform
  #as would be expected under null hypothesis
  
  #for syntax of plot command see:
 # https://www.statmethods.net/graphs/line.html
 # https://www.statmethods.net/advgraphs/parameters.html
  # More generally, if you can't understand something in R, Google it!
  # Specifically here, pch specifies symbol type, cex specifies size, lty is line type
  # You can play with these values: change them and re-run this bit of script to see the effect
  
  plot(-lpexp,-everyp$lpob,pch=16,cex=.7,
       ylab='Observed p-values (-log10)',
       xlab='Expected p under null (-log10)',
       main='Q-Q plot',
       col=everyp$colorcode)
  lines(-lpexp,-lpexp) #expected distribution of p-values under null
  abline(h=1.301,lty=2,col='red') #cutoff for p = .05, because log10(.05) = 1.301
  text(2,0.4,paste0('Red are true effects\nTrue effect size (r) =  ',myES,'\nN = ',myN),cex=.7)
  #The first 2 numbers in the text command give x and y coordinates on the plot
  #Here they are selected just to put text in convenient spot on this plot
  
  myoutput<-'Variables with low p are:'
  myoutput
 everyp$Var[lo.p]

 # If r is low and sample size is small, you will find that you get 'significant' p-values
 # for variables other than 2-6. This is Type I error.
 # You will also see red dots that are not 'significant' - these are genuine effects that
 # are missed, i.e. type II error.

 # You could use a similar approach to looking at p-value distribution for any large
 # dataset with multiple variables. 
 # 
 # Questions to consider
 # 1. Does adopting a more extreme p-value help overcome inferential problems when we have many
 #    variables?
 # 2. How useful is it to divide a sample into two random halves to see if results replicate 
 #    across both subsets?
 # 3. How does the GWA approach with Q-Q plots tie in with suggestions in other subject areas 
 #    that we should 'analyse everything' in a multiverse approach?
 #    http://journals.sagepub.com/doi/abs/10.1177/1745691616658637
 
 
  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/

  