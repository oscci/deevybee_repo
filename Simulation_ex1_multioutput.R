#-------------------------------------------------------------------------
# Generate multiple pirate plots from simulation 
# Based on intro script by DVM Bishop
# 7th October 2017
#-------------------------------------------------------------------------

library(yarrr) # for pirate plots (see below)

# It's also a good idea to disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
myNs<-c(20,100)
myES<-0.3
for (i in 1:2){
  outname<-paste('pirates_N',myNs[i],'.pdf')
pdf(outname,width=15,height=10) #
par(mfrow=c(2,5))

  for (j in 1:10){
    myvectorA<-rnorm(n = myNs[i], mean = 0, sd = 1) 
    myvectorB<-rnorm(n = myNs[i], mean = 0.3, sd = 1)
    mydf <- data.frame(matrix(data=NA,nrow=myN*2,ncol=2)) #NA indicates missing data
    colnames(mydf)<-c('Group','Score') #c is concatenate function, puts what follows in a sequence
    mydf$Group[1:myN]<-1 # First set of rows allocated to group 1
    mydf$Group[(myN+1):(2*myN)]<-2 # Next block of rows allocated to group 2
    mydf$Score[1:myN] <- myvectorA # First block in Score column is vectorA
    mydf$Score[(myN+1):(2*myN)]<-myvectorB # 2nd block in Score column is vectorB
    myt <- t.test(myvectorA,myvectorB) #see http://www.statmethods.net/stats/ttest.html
    myheader=paste('t = ', format(myt$statistic,digits=2),'; p = ',format(myt$p.value,digits=3))
    pirateplot(Score~Group,theme=1,cex.lab=2,data=mydf,main=myheader, point.o=1, xlab="Group", ylab="Score")
  }
dev.off()
}

