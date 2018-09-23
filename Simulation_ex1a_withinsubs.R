#-------------------------------------------------------------------------
# Generate multiple pirate plots from simulation and save as pdf
# This version generates same data but treats as within rather than between groups
# Based on Simulation_ex1_multioutput script by DVM Bishop
# 23rd Sept 2018
#-------------------------------------------------------------------------

library(beeswarm) # beeswarm easy to use to show all data without needing to create data.frame

# disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
myNs<-c(20,100) #c denote 'concatenate' and is used to create a vector of values
myES<-0.5 #effect size for treatment 
mycor<-.5 #correlation between 2 measures for same person assuming no treatment effect
# To run the simulation repeatedly, we will use a loop. 
for (i in 1:2){ #This is the syntax to START a loop. The loop ends when a matching } is reached
  myN<-myNs[i] #For the first run through the loop, i = 1, so myN =20. On 2nd run, i=2 and myN =100
  outname<-paste0('beeswarm_diffs_N',myN,'.pdf') #Make a name for the pdf file to hold the plots
  pdf(outname,width=15,height=10) #Size of the output pdf
  par(mfrow=c(2,5)) #arrange output in 2 rows and 5 columns

  for (j in 1:10){ #inner loop, nested within outer loop. This runs 10 times. 
    #Each time it generates a new sample using script like that of Simulation_ex_1_intro.R
    myvectorA<-rnorm(n = myN, mean = 0, sd = 1) 
    #we need to create a set of scores correlated with myvectorA, correlation of mycor
    #Formula below achieves that: myvectorA2 is partly based on myvectorA and partly on a random number
    myvectorA2<-mycor*myvectorA+(sqrt(1-mycor^2))*rnorm(length(myvectorA))
    #You can check what the actual correlation is with the code; cor(myvectorA,myvectorA2)
    #Now we add the effect size to myvectorA2
    myvectorB<-myvectorA2+myES
    myt <- t.test(myvectorA,myvectorB,paired=TRUE) #see http://www.statmethods.net/stats/ttest.html
    
    #For plotting, we compute the difference score for each subject
    mydiff<-myvectorB-myvectorA 
    #In effect the matched pairs t-test tests if that mean difference is different from zero
    
    #set mystars as a text string with asterisks to denote conventional levels of significance
    mystars<-''
    if(myt$p.value<.05) {mystars<-'*'}
    if(myt$p.value<.01) {mystars<-'**'}
    if(myt$p.value<.001) {mystars<-'***'}
    myheader=paste0('t = ', format(myt$statistic,digits=2),'; p = ',format(myt$p.value,digits=3))
    beeswarm(mydiff,main=myheader, ylab="Difference",pch=16,col='blue',cex=3)
    abline(h=0)
    text(0.8,2,mystars,col='red',cex=3) #put mystars text between groups 1 and 2 at top of chart where z=3
                                        #cex controls size of text
      } #end of inner loop
 dev.off() #close the pdf plot
} #end of outer loop

