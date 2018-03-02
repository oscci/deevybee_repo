
# Trading N and alpha
# DVM Bishop
# 4th May 2017

# Inspired by discussion with Kate Button about best strategy when constraints on N


setwd("~/deevybee_repo")
library(xlsx)
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
myNvalues<-c(20,40,80,160,320)
myESvalues<-c(0,.1,.3,.5)
mytruehyps<-c(1)
myalphas<-c(.001,.01,.05,.1)
#create matrix to hold info re each run through loops
myallsims<-data.frame(matrix(rep(NA,9*length(myNvalues)*length(myESvalues)*length(mytruehyps)),ncol=8))
colnames(myallsims)<-c('Nsims','N','ES','true%','PPV.001','PPV.01','PPV.05','PPV.1')
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
mySD<-1 #
mynSims <- 100 # Specify number of simulated datasets. Need 100000 for accurate estimates

#Create matrix to hold data from each set of simulations - this will be recycled each time through loop
sumtable=data.frame(matrix(rep(NA,(mynSims*6)),nrow=mynSims)) #initialising a dataframe that will hold p values in each run
colnames(sumtable)<- c("run","Npergp","ES","meandiff","t","p")
j<- 0 #increments for each combination in loops
for (myN in myNvalues){
  for (myrealES in myESvalues) {
  for (mypropreal in mytruehyps){
         j<- j+1
# Simulate data for one run through loop
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
         k<-0
         for (myalpha in myalphas){
           k<- k+1
           sigp<-length(which(sumtable$p<myalpha))

           myallsims[j,4+k]<-sigp/mynSims #proportion sig in last 4 columns
 
         }
         
#-------------------------------------------------------------------------
         
    myallsims[j,1]<-mynSims
    myallsims[j,2]<-myN
    myallsims[j,3]<-myrealES
    myallsims[j,4]<-mean(sumtable[,6])
 
  }
    colnames(myallsims)<-c('Nsims','N','ES','myp','p.001','p.01','p.05','p.1')
    
  }
}
write.xlsx(myallsims, "myallsims4alpha.xlsx",row.names=FALSE)


  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  