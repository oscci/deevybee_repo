#-------------------------------------------------------------------------
# Simulation introduction by DVM Bishop
# 2nd May 2017
#-------------------------------------------------------------------------

# I'd heard that use of median splits in data analysis was a bad idea
# because they aren't reproducibile (i.e. the median in my sample
# will be different from the median in yours)
# I suspected, but wasn't sure if they also affected the false positive rate.
# But rather than making spurious p-values more likely, they seemed likely to 
# reduce rate of significant results by losing information and turning a 
# quantitative variable into a category.
# So I made this simulation to find out if this was so.
# For newbies, exercise 1 has a lot of comments in the script to explain 
# R basics, so do that first if you are unclear about syntax
#-------------------------------------------------------------------------
# Preliminaries
#-------------------------------------------------------------------------
setwd("~/deevybee_repo") #set working directory. You should change this for your computer

require(MASS)
# It's also a good idea to disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r

# ------------------------------------------------------------------------
# Simulate random numbers sampled from a normal distribution
# ------------------------------------------------------------------------
# You just specify the number (myN), mean (myM) and SD (mySD)
# Naming conventions vary: I tend to preface my variables with 'my' to avoid confusion with
# any pre-existing R functions etc

# We're going to simulate two normally distributed variables with a given correlation between them.
# Then we'll test the correlation for significance, and then use variable B as the
# basis for dividing into two groups, split at the median, and do a t-test to compare
# the groups on A.


myM <- c(0,0)
corvals<-c(0,.1,.2,.3,.4,.5,.6)
Nvals<-c(20,40,80,100)
Ncors<-length(corvals)
Nns<-length(Nvals)
corsummary<-matrix(rep(NA,Ncors*Nns*4),ncol=4)
colnames(corsummary)<-c('N','r','pcorr','p.t')
j<-0
for (myCorr in corvals){
  for (myN in Nvals){
  j<-j+1
myCov<-matrix(c(1,myCorr,myCorr,1),ncol=2)#Warning! Covariance and correlation same only if SD=1
nrun<-1000
myp<-matrix(rep(NA,nrun*2),ncol=2) #we'll save r and p-values for each run

  for (i in 1:nrun){
  #create sample of 2 normally distributed variables from from population with correlation of myCorr
  mydf<-data.frame(mvrnorm(n = myN, myM, myCov))
  colnames(mydf)<-c('A','B')

#Now we are going to divide the first one at the median to create two groups
mymed<-median(mydf$A)
mydf$group<-0 #default group is zero
mydf$group[mydf$A>mymed]<-1

mycor<-cor.test(mydf$A,mydf$B)
myp[i,1]<-mycor$p.value #p-value for correlation between A and B

myt<-t.test(mydf$B~mydf$group)
myp[i,2]<-myt$p.value #p-value for same data with A treated as category (median split)
}


corsummary[j,1]<-myN
corsummary[j,2]<-myCorr
corsummary[j,3]<-length(which(myp[,1]<.05))/nrun
corsummary[j,4]<-length(which(myp[,2]<.05))/nrun
  }
}

