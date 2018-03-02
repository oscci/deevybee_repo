
# Double dipping
# DVM Bishop
# 29th March 2017

# See: Kriegeskorte, N., Simmons, W. K., Bellgowan, P. S. F., & Baker, C. I. (2009).
# Circular analysis in systems neuroscience: the dangers of double dipping. 
# Nature Neuroscience, 12(5), 535-540. doi:http://www.nature.com/neuro/journal/v12/n5/suppinfo/nn.2303_S1.html

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')
plot.new() #initialise new graphics window

# We will simulate data from a single electrode in two conditions, A and B
# in an electrophysiological experiment measuring event-related activity over time
# We want to see whether A and B lead to different waveforms

# Double dipping refers to the situation where the same data is used
# both to identify an effect of interest and to evaluate it.
# Here we will first inspect the waveforms to identify the point at which
# there is maximum divergence of the A vs B waveforms, and then
# do a test to see if the mean waveform difference at that point differs from zero.
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
#----------------------------------------------------------------------------------
# We will simulate data to form a time series
# This first part of the script just demonstrates how this works.
# An easy way to simulate a time series is to base each point on the previous point
# with a random number added. Here's an example that generates 2 time series

mytimepts<-100
myautocorrindex<-.95 #determines how similar each point is to preceding point
mynum<-matrix(rep(0,mytimepts*2),ncol=2) #initialise a matrix with 2 columns of zeros
 for (i in 2:mytimepts){
   mynum[i,1]<- myautocorrindex*mynum[(i-1),1]+(1-myautocorrindex)*rnorm(1) #rnorm generates a random normal deviate
   mynum[i,2]<- myautocorrindex*mynum[(i-1),2]+(1-myautocorrindex)*rnorm(1) #rnorm generates a random normal deviate
 #each point in the series is 95% of value of prior point, plus .05 times a random zscore
 }
#We'll now look at the two columns we've created; axes are labelled as if this is an ERP signal
plot(mynum[,1],pch=NA,xlab='Time',ylab='Amplitude',ylim=range(mynum*1.2))
lines(mynum[,1],col='blue')
lines(mynum[,2],col='red')

# If you repeatedly run the above section of code, you'll see how common it is for two randomly
# generated time series to diverge at some point.

# Can you predict what would change if you altered myautocorrindex to .5. Or made it zero?
# Try it and see.
#----------------------------------------------------------------------------------

# Now we'll simulate a whole set of waveforms corresponding to a collection of subjects
mynSims<-1000 #To start with , you can set mynSims to 1, to just simulate one dataset, but once you understand the script
           # we can increase mynSims to go on to repeatedly simulate data and look at p-values
sumtable<-matrix(rep(NA,mynSims*4),nrow=mynSims) #initialise matrix to hold summary results with NA 
colnames(sumtable)<-c('Run','meandiff','t','p')


for (k in 1: mynSims){
#Now we will generate similar series for myNsub subjects
#We'll create a 3D array to hold the data
#The dimensions are Subject, Timepoint, A/B/difference
myNsub<-10

myarray<- array(0, dim=c(myNsub,mytimepts,3)) #3rd dimension used to hold 3 values: series for A, B and difference A-B
 for (j in 1:myNsub){
  for (i in 2:mytimepts){
  myarray[j,i,1]<- myautocorrindex*myarray[j,(i-1),1]+(1-myautocorrindex)*rnorm(1) #rnorm generates a random normal deviate
  myarray[j,i,2]<- myautocorrindex*myarray[j,(i-1),2]+(1-myautocorrindex)*rnorm(1) #rnorm generates a random normal deviate
  #For each subject, generates 2 autocorrelated series, as above
  myarray[j,i,3]<-myarray[j,i,1]-myarray[j,i,2] #difference bewteen A and B
  }
 }
mymeandiff<-apply(myarray[,,3],2,mean) #mean amplitude x time in the A-B wave, averaged across subjects

mypeak<-max(mymeandiff) #find peak in grand average A-B waveform
# We'll use the peak difference to define the window for analysis
mypt<-which(mymeandiff==mypeak) #find timepoint containing peak

mypeakvector<-myarray[,mypt,3]

myt<-t.test(mypeakvector,mu=0)


 sumtable[k,1]<-k
 sumtable[k,2]<-myt$estimate
 sumtable[k,3]<-myt$statistic
 sumtable[k,4]<-myt$p.value
}
# Uncomment the next 2 lines to plot the difference wave from the final simulated dataset
# plot(mymeandiff,pch=NA,xlab='Time',ylab='Amplitude',main='Difference wave')
# lines(mymeandiff,col='blue')
fpr<-length(which(sumtable[,4]<.05))/mynSims

#Now plot the histogram showing obtained p-values
mytitle<-paste('P-value distribution: % below .05 = ',fpr)
mybreaks=seq(0,1,.025)
hist(sumtable[,4],breaks=mybreaks,main=mytitle,xlab='p-value')
abline(v=.05, col="red")

# What should this distribution look like if we did not have double-dipping, 
# given that there is no true difference between groups?

# You can test that by prespecifying a value of mypt.
# E.g. add a new line after line 75 : mypt <- 50 : rerun and compare


  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  