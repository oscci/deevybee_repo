#-------------------------------------------------------------------------
# Generate multiple pirate plots from simulation and save as pdf
# Based on intro script by DVM Bishop
# 7th October 2017
#-------------------------------------------------------------------------

library(yarrr) # for pirate plots (see below)

# disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
myNs<-c(20,100) #This creates a vector with 2 values, 20 and 100. We will simulate 10 samples of each size 
myES<-0.5 #effect size for difference between groups (you can try changing this)

# To run the simulation repeatedly, we will use a loop. 
for (i in 1:2){ #This is the syntax to START a loop. The loop ends when a matching } is reached
  myN<-myNs[i] #For the first run through the loop, i = 1, so myN =20. On 2nd run, i=2 and myN =100
  outname<-paste0('pirates_N',myN,'.pdf') #Make a name for the pdf file to hold the plots
  pdf(outname,width=15,height=10) #Size of the output pdf
  par(mfrow=c(2,5)) #arrange output in 2 rows and 5 columns

  for (j in 1:10){ #inner loop, nested within outer loop. This runs 10 times. 
    #Each time it generates a new sample using script like that of Simulation_ex_1_intro.R
    myvectorA<-rnorm(n = myN, mean = 0, sd = 1) 
    myvectorB<-rnorm(n = myN, mean = myES, sd = 1)
    mydf <- data.frame(matrix(data=NA,nrow=myN*2,ncol=2)) #NA indicates missing data
    colnames(mydf)<-c('Group','Score') #c is concatenate function, puts what follows in a sequence
    range1<-1:myN #range of rows for group 1 data
    range2<-(myN+1):(2*myN) #range of rows for group 1 data
    mydf$Group[range1]<-1 # First set of rows allocated to group 1
    mydf$Group[range2]<-2 # Next block of rows allocated to group 2
    mydf$Score[range1] <- myvectorA # First block in Score column is vectorA
    mydf$Score[range2]<-myvectorB # 2nd block in Score column is vectorB
    myt <- t.test(myvectorA,myvectorB) #see http://www.statmethods.net/stats/ttest.html
    myheader=paste0('t = ', format(myt$statistic,digits=2),'; p = ',format(myt$p.value,digits=3))
    pirateplot(Score~Group,theme=1,cex.lab=2,data=mydf,main=myheader, point.o=1, xlab="Group", ylab="Score")
  } #end of inner loop
dev.off() #close the pdf plot
} #end of outer loop

