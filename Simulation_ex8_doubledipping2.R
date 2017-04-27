
# Double dipping -2
# DVM Bishop
# 29th March 2017

# See: Bishop, D. V. M. (1990). How to increase your chances of obtaining a 
# significant association between handedness and disorder. 
# Journal of Clinical and Experimental Ñeuropsychology, 12, 786-790. 

# I argued that the literature on associations between handedness and disorder
# was unreliable because:
# a) Handedness was a 'bonus' variable- very easy to gather data and only report it
#    if a significant association was found (publication bias)
# b) There was no agreement about how to form handedness categories
#    e.g. suppose you had a 3 -item inventory where score was N items done with R
#       We refer to scores as follows: 0=L, 1=l, 2=r, 3 = R
#   you could have extreme R-handers vs all others; (L+l+r vs R)
#                        R biased- vs Left-biased  (L_l vs r+R)
#                        mixed laterality vs others (l+r vs L+R)
#                        etc
# In this context, double dipping occurs if the decision of how to
# form hand preference groups is made after inspecting the data for 2 groups.
# This is a simple case of a flexible analytic pipeline.
# This example shows how this distorts interpretation of p-values

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')
new.plot() #open graphics window. 

# We will simulate data from a 3-item handedness inventory for two groups
# 90% of people are right-biased, with p(Rhanded)= .9 for each item
# 10% of people are left-biased, with p(Rhanded)= .2
#(You can vary these parameters: they are not crucial: just selected to give the
# kind of J-shaped handedness distribution that you get in real life)


options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
#----------------------------------------------------------------------------------

# Now we'll simulate Ns in each handedness category for two groups
mynSims<-1000 # To start with , you can set mynSims to 1, to just simulate one dataset, 
              # but once you understand the script we can increase mynSims 
              # to go on to repeatedly simulate data and look at the distribution of p-values
sumtable<-data.frame(matrix(rep(0,mynSims*11),nrow=mynSims)) #initialise matrix to hold summary results with NA 
# sumtable will hold counts for hand preference scores for groups A and B
# e.g A0 is N from group A with score =0; B2 is N from group B with score = 2
colnames(sumtable)<-c('Run','A0','A1','A2','A3','B0','B1','B2','B3', 'pattern','p')
myNsub<-10 #Number of subjects per group
mynitem<-3;  #N items in handedness inventory
mypattern<-matrix(c(0,0,0,1,
             0,0,1,0,
             0,0,1,1,
             0,1,0,0,
             0,1,1,0,
             0,1,1,1,
             1,0,0,0,
             1,0,0,1,
             1,0,1,1,
             1,1,0,0,
             1,1,1,0),nrow=11)
# my pattern has all the plausible ways of forming handedness groups
# L is extreme L (0), l is mod L (1), r is mod R (2) and R is strong R (3)
# then these patterns show how grouped, e.g. 0,0,0,1 is L+l+r vs R
#                                           0,1,1,0 is L+R vs l+r
#                                           1,0,0,0 is L vs l+r+R
for (k in 1: mynSims){
  for (g in 1:2){ #two groups simulated
    goffset<-2 #offset (2 or 6) used to write counts to correct column in sumtable
    if(g==2){goffset<-6}
    for (j in 1:myNsub){
     myrand<-.9 #myrand is probability of R choice; default is .9
     if(runif(1)<.1){myrand<-.2} #10 percent L biased, so myrand is .2 instead of .9
      handprobs<-runif(mynitem) #random value from 0 to 1 for each test item
         Ritems<-length(which(handprobs<myrand)) # count how many items R sided; this is hand score for this person 
                                 #(defined by whether runif number is below myrand)
         myindex<-Ritems+goffset #specify which column to increment for this subject
         sumtable[k,Ritems+goffset]<-sumtable[k,Ritems+goffset]+1 #add to totals for this run
    } #end of subject loop
  } #end of group loop
  sumtable[k,1]<-k #column 1 of sumtable gives simulation number
  
  #now test the data to find out which pattern gives biggest group difference
  Npatt<-nrow(mypattern)
  lowp<-1 #initialise pvalue from Fisher to one. We'll look for values below this
  for (i in 1:Npatt){ #loop through all possible patterns
    
    a=0;b=0;c=0;d=0 #a,b,c,d are cells in 2 x 2 table for Fisher exact test
  
    for (m in 1:4){
    a<-a+sumtable[k,(1+m)]*mypattern[i,m]
    c<-c+sumtable[k,(5+m)]*mypattern[i,m]
    }
    b<-myNsub-a
    d<-myNsub-c
    #now we do a fisher exact test on cells a, b, c, d
    thisfish<-fisher.test(matrix(c(a,b,c,d),nrow=2))
    thisp<-thisfish$p.value
    if(thisp<lowp){ #check if this is the lowest p value so far
      lowp<-thisp   #if so, store this p-value and pattern
      lowpattern<-paste(mypattern[i,1],mypattern[i,2],mypattern[i,3],mypattern[i,4],sep='')
    }
  }
  sumtable[k,10]<-lowpattern #store the pattern that maximises group difference
  sumtable[k,11]<-lowp #store the associated p value
    
}
  mysigp<-length(which(sumtable[,11]<.05)) #over all simulations, how many p < .05
  sigrate<-mysigp/mynSims #what % runs give significant p
  mytitle<-paste('P-value distribution: % below .05 = ',sigrate)
  mybreaks=seq(0,1,.025) #breaks for histogram plot
  hist(sumtable[,11],breaks=mybreaks,main=mytitle,xlab='p-value')
  abline(v=.05, col="red")
  
  #Note that although there is no true difference between groups, the 
  # distribution of p-values is skewed, with excess in left end.
  # This is the result of flexible categorisation that biases analysis

  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
  