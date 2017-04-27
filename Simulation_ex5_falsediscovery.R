
# False discovery rate demo
# DVM Bishop
# 23rd March 2017

# See: Colquhoun, D. (2014). An investigation of the false discovery rate 
#  and the misinterpretation of p-values. Royal Society Open Science, 1. 
#  doi:http://dx.doi.org/10.1098/rsos.140216

# Note: before running this one, drag the left margin of your Plots window so it
# takes up at least 1/3 of your screen - we're going to have two plots side
# by side: if they don't fit you get a error message saying margins too small

# We're using the plots window here, so you can easily see how FDR changes with 
# things like sample size, but in most contexts it makes sense to save plots
# in pdf or png format, which gives you better control over the formatting.
# For information on that, see 
#  http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')
library(yarrr) # for pirate plots - you may need to install yarrr from Tools|Install Packages
plot.new() # open graphics window. 

# A p-value indicates the probability of the data given the null hypothesis
# This is NOT the same as the probability of the null hypothesis given the data!
# The latter is the False Discovery Rate (FDR)
# The p-value is not the FDR, but it is often misinterpreted in that way.

# Consider: We observe a hand and have to determine if male or female
# The only evidence we have is that the fingernails are painted.
# Let's assume that 30% of females and 2% of males paint their fingernails.
# This can be written as P(paint|F) = .3,P(paint|M) = .02.
# Let's also assume that the population is 50:50 male:female
# Then in sample of 100 male and 100 female we have 
# 2 males painted, 98 males bare, 30 females painted, 70 females bare
# If we observe bare nails, the likelihood of male is 98/168 = .58
# This probability can be written as P(M|bare) [probability of male given bare nails]
# If we observe painted nails, the likelihood of male is 2/32 = .06
# This probability can be written as P(M|paint)
# This example is just to demonstrate that P(A|B) is not same as P(B|A)
# When we do a study, we want to know what is the probability of the hypothesis given the data

# We can only estimate the FDR, though, if we have some notion of how likely it is that our
# hypothesis is true.
# To demonstrate how FDR works, we have to simulate data from a mixture of null and non-null hypotheses

options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r


myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
mySD<-1 #
myN<-20 #set sample size per group (You can vary this to see the effect)
mynSims <- 500 # Specify number of simulated datasets. Need 100000 for accurate estimates
# We'll start with simulating 100 datasets, but can later update this number for greater accuracy
mypropreal <-.5 # Specify proportion of simulations where groups drawn from different populations
myrealES<-.3 # Effect size when there is a real difference

sumtable=data.frame(matrix(rep(NA,(mynSims*6)),nrow=mynSims)) #initialising a dataframe that will hold p values in each run
colnames(sumtable)<- c("run","Npergp","ES","meandiff","t","p")

for (i in 1:mynSims){
  #----------------------------------------------------------------------------------------
  # We'll generate a dataset in 'wide' format, i.e. one row per subject, as this is easy to read
  #----------------------------------------------------------------------------------------
  mydata <-data.frame(matrix(rnorm(n = myN*2, mean = myM, sd = mySD),nrow=myN*2))
  truediff<-0 #default is no difference between groups
  if((i/mynSims)<mypropreal) {
   truediff<-1
   mydata[1:myN,1]<-mydata[1:myN,1]+myrealES #for proportion of simulations, add effect size to grp1
 }
myt<-t.test(mydata[1:myN,1],mydata[(myN+1):(myN*2),1])
 sumtable[i,1]<-i
 sumtable[i,2]<-myN
 sumtable[i,3]<-myrealES*truediff
 sumtable[i,4]<-myt$estimate[1]-myt$estimate[2]
 sumtable[i,5]<-myt$statistic
 sumtable[i,6]<-myt$p.value
}
 sigp<-which(sumtable$p<.05)
 noeffect<-which(sumtable$ES==0)
 falsedisc<-intersect(sigp,noeffect)
 FDR<-round(length(falsedisc)/length(sigp),3)
  sumtable$ES<-as.factor(sumtable$ES)
  myheader<-paste("Size per gp =",myN)
   #pirateplot(t~ES,data=sumtable,main=myheader,   xlab="Effect", ylab="t-value")
  # Option of plotting t-values
#-------------------------------------------------------------------------
  #Prepare for plotting
  #quartz() #open a new window
  # if working on a PC substitute this command: windows()
  # You can omit this command and just use the Plots pane; this
  # avoids lots of new windows, but you may need to stretch the pane,
  # by dragging its margins, to see all the output properly
  
  par(mfrow=c(1,2)) #set up plot frame with 2 plots in 1 row
  #-------------------------------------------------------------------------
  
  pirateplot(p~ES,data=sumtable,main=myheader,   xlab="Effect", ylab="p-value")
  abline(h=.05, col="blue", lty=2) #The dotted blue line shows p .05
  myheader<-paste("FDR = ",FDR)
  pirateplot(log10(p)~ES,data=sumtable, main=myheader,  xlab="Effect", ylab="log10 p-value")
  abline(h= -1.301, col="blue", lty=2) #The dotted blue line shows p .05
  abline(h= -2, col="maroon", lty=2) #The dotted maroon line shows p .01
  abline(h= -3, col="red", lty=2) #The dotted red line shows p .001
  
  # The two plots show the same information, but right hand plot uses log
  # scale so you can see cutoffs for .05 (blue), .01 (maroon) and .001 (red)
  
  # The left hand plot shows the distribution of p-values for runs where there
  # is no true effect
  # The right hand plot shows the distribution of p-values for runs where there
  # is an effect of effect size ES
  
  # The FDR is the proportion of all p-values below a cutoff that are in the
  # pink plot. The dotted blue line corresponds to p <. 05
  
  # Try changing the values of myN, mypropreal, and myES
  # Note how with other parameters held constant, FDR is higher with small samples
  
  # Statistical tests are designed to ensure that type 1 and type 2 errors 
  # are the same regardless of sample size
  # But this is NOT the case for FDR - so the likelihood that a pvalue < .05
  # corresponds to no true difference is greater for small samples
  
  # Note that the simulation of a mixture of true and null effects is 
  # easy to relate to if you are doing something like drug testing and
  # have several compounds to test: some are effective and some are not.
  # In other contexts, however, it can be hard to estimate mupropreal, i.e. 
  # the likelihood of a true effect vs the null hypothesis; this relates to the concept
  # of a prior, which is key in Bayesian statistics
  
  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  