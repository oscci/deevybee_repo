#-------------------------------------------------------------------------
# Simulation Exercise 3 by DVM Bishop
# 20th March 2017; updated 4th March 2018 with thanks to Pieter Moors 
# Simulating data for multiway ANOVA
#-------------------------------------------------------------------------

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')

# Remember! you will need to download these packages if they aren't already downloaded
library(reshape2) # for converting data from wide to long form (see below)
library(gridExtra) #for plotting output in a grid
library(grid) #https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
library(formatR)
options(scipen=999) #disable scientific notation.

#-------------------------------------------------------------------------
# We are going to simulate data for a 3 way mixed anova: between/within/within
# Our dependent variable is Score. 
# We have two equal groups (0 and 1), as between subjects variables
# Each subject is measured on two occasions (1 and 2), on easy and hard task (1 and 2)
# This gives 4 scores (easytime1, hardtime1, easytime2, hardtime2) for each subject
# We are just going to generate random numbers - there is no real effect
# We will look for effects of factors group, occasion, and difficulty, 
# as well as interactions between these

# Be warned: as this website notes: http://www.statmethods.net/stats/anova.html
# 'If you have been analyzing ANOVA designs in traditional statistical packages, 
#  you are likely to find R's approach less coherent and user-friendly'
# Our aim here, though, is not to teach you to do ANOVA in R, so much as to 
# reveal some of the pitfalls in interpreting multiway ANOVA
#-------------------------------------------------------------------------

myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
mySD<-1 #
myN<-10 #set sample size per group (You can vary this to see the effect)
mynSims <- 20 # Specify number of simulated datasets
  # We'll start with simulating 20 datasets, but can later update this number
ptable=matrix(rep(NA,(mynSims*9)),nrow=mynSims) #initialising a matrix that will hold p values in each run
# There are 7 p-values: 3 main effects, 3 2-way interactions, and 1 3-way interaction
# The last two columns will denote if any p-value in a run is <.05, or < .007 (Bonferroni corrected)
colnames(ptable)<- c("A","B","C","AB","AC","BC","ABC","anysig","Bonfsig")
  
for (i in 1:mynSims){
  #----------------------------------------------------------------------------------------
  # We'll generate a dataset in 'wide' format, i.e. one row per subject, as this is easy to read
  #----------------------------------------------------------------------------------------
mydata <-data.frame(matrix(rnorm(n = myN*2*5, mean = myM, sd = mySD),nrow=myN*2))
mydata[,1]=1 #overwrite column 1 with ones
mydata[1:myN,1]<-0  #overwrite first half of column 1 with zeros
colnames(mydata)<-c('Group','Easy1','Hard1','Easy2','Hard2')
mydata$Group <-as.factor(mydata$Group)  #this is important!
  # You can look at mydata by clicking on its label in the Environment tab
  #----------------------------------------------------------------------------------------
  # Save your simulated data as tab-separated text
  #----------------------------------------------------------------------------------------
    write.table(mydata, "simulated_data.txt", sep="\t") 
  # You could use saved data if you want to double-check against output of Anova from another package
  # But see comments in script 2: advisable to comment out this line once you've got the idea
  # And note that on each run, the file will be overwritten with latest output
  # (Easy to fix by changing the file name on each run, but you don't want your computer
  # clogged up with loads of unwanted files - though you might like to see if you could
  # work out how to achieve this, just as an exercise)
  #----------------------------------------------------------------------------------------
  # There are various ways of doing ANOVA in R: most require data to be in long form
  #  For explanation see http://seananderson.ca/2013/10/19/reshape.html
  #----------------------------------------------------------------------------------------
  
 mylongdata <- melt(mydata) #converts to long form, with one column with all scores
 mylongdata$Subject <- as.factor(c(seq(1:(myN*2)),seq(1:(myN*2)),seq(1:(myN*2)),seq(1:(myN*2))))
 # Add a column giving subject ID: this is repeated for each combination of conditions
 # NB important this is made into a factor! This was omitted in original version of script, corrected
 # 18th April 2017
 
 mylongdata$time <-c(rep(1,myN*2*2),rep(2,myN*2*2)) #adds column giving time
 mylongdata$difficulty <-c(rep(1,myN*2),rep(2,myN*2),rep(1,myN*2),rep(2,myN*2))#adds column giving difficulty
 # Note that there are more general ways of reshaping data: this is just simple for this design
 # Look now at longdata by clicking on its name in the Environment tab

 #----------------------------------------------------------------------------------------
 # Now run an ANOVA
 #----------------------------------------------------------------------------------------
 myaov<-summary(aov(value~(time*difficulty*Group)+Error(Subject/(time*difficulty)),data=mylongdata))
 myaovbit<-unlist(myaov)[grep(pattern = "Pr", x = names(unlist(myaov)))] # tortuous way to extract the relevant bit of output
 myaovbit <- myaovbit[which(!is.na(myaovbit))] #take values that are not NA
 ptable[i,1:7] <-myaovbit #extract the p-values which happen to be values 33-39 in this output  ptable[i,8:9]<-0 #initialise col 8-9 which will categorise each ANOVA in terms of whether *any* sig effects
  sigp<-which(ptable[i,1:7]<.05) #find whether any p-values are < .05
  if(length(sigp)>0)
    {ptable[i,8]<-1} # if so, assign a 1 to column 8
  sigp<-which(ptable[i,1:7]<.007) #now do the same with p < .007, i.e. .05/7
  if(length(sigp)>0)
  {ptable[i,9]<-1} #result is stored in column 9
 
  } #repeat for next simulation
# The way ptable is populated is a pretty clunky way of doing things, 
#  but I hope it is reasonably easy to follow
# This is a much more elegant alternative for lines 83-90
# suggested by my colleague Paul Thompson:
# ptable[i,1:7]<-myaov[["Error: Within"]][[1]]$'Pr(>F)'[1:7]
# ptable[i,8]<-ifelse(any(ptable[i,1:7]<.05)==TRUE,1,0)
# ptable[i,9]<-ifelse(any(ptable[i,1:7]<.007)==TRUE,1,0)

mypf<-round(ptable,digits=2) #get rid of extraneous digits; This doesn't always work- not sure why!
if (mynSims<21){
grid.table(mypf) #doing gridtable gets v slow with more than 20 rows
}
percent05<-100*(sum(ptable[,8])/mynSims) 
percentBon<-100*(sum(ptable[,9])/mynSims)
paste('% ANOVAs with one effect or interaction < .05 = ',percent05)
paste('% ANOVAs with one effect or interaction < .007 = ',percentBon)

# For an explanation of the issues raised by this exercise see:
# http://deevybee.blogspot.co.uk/2013/06/interpreting-unexpected-significant.html

  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
#   

