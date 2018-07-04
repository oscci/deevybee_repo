#-------------------------------------------------------------------------
# Simulation Exercise 2 by DVM Bishop
# 20th March 2017; updated to include P-P plot 24 March 2018
# Simulating and saving a set of correlated variables
#-------------------------------------------------------------------------


#setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')

# Remember! you will need to install these packages if they aren't already installed
library(MASS) #for mvrnorm function to make multivariate normal distributed vars
library(Hmisc) #for rcorr function: gives correlation matrix and p values
library(gridExtra) #for plotting output in a grid
library(grid) #https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html


options(scipen=999) #disable scientific notation.

#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate 7 variables (but you could change this number)
#-------------------------------------------------------------------------

nVar<-7 #number of simulated variables (arbitrary but useful for figure that will fit on screen)
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
myVar<-1 #Remember variance is SD^2. For z-scores, SD is 1, so variance is also 1
myN<-30 #set sample size per group (You can vary this to see the effect)
myCorr<-0 #correlation between variables
everyp<-vector() #initialise a vector to hold all the pvalues
# Rather than repeatedly running the script, we are going to make a 'for' loop
# This automatically runs the sections of script between { and } repeatedly mynSims times

mynSims <- 10 # Set number of simulated datasets
  # We'll start with simulating a single dataset, but then later update this number
sigpcounter=rep(0,mynSims) #initialising a vector that will count significant p values in each run
  
  # We're going to use the mvrnorm command which generates a multivariate normal distribution
  # It's similar to rnorm: you specify mean and variance for each variable
  # But you also need to specify how variables are intercorrelated
  # mvrnorm requires you to specify the covariance matrix
  # For simplicity, we'll work with zscores with mean 0 and SD 1
  # This means that the correlation and covariance are the same, and the variance and SD are the same
  
  myCov<-matrix(rep(myCorr,nVar*nVar),nrow=nVar) #rep(x,y) means generate y values of x
  # Look at myCov after running this line; you have 7 x 7 matrix with all entries equal to myCorr
  # Try just running a command like rep(3,9) - you get a vector
   # We need to turn the vector into a 2 x 2 matrix :the matrix command achieves that
  # 'Matrix' turns a vector into a nrow x ncol array: you need to specify n rows as above
  
  # We need to fix our matrix so that the diagonal values correspond to variance of each variable
  diag(myCov)<-rep(myVar,nVar) # now look at myCov again
  
  # NB for simplicity, we've made all intercorrelations the same, but you could hand craft your matrix
  # to have different correlations between different variables. 
  # Remembering that you can use square brackets to select particular rows and columns of an array,
  # how would you modify this matrix so that correlation between variables 1 and 2 was .1?
  # (Hint: not as simple as it looks: remember correlation matrix must be symmetric)
  
  #----------------------------------------------------------------------------------------
  for (i in 1:mynSims){ # 'for loop' command: runs loop mynSims times
    # Each time the code between { and } is run, the index, i, increases by one
    #N.B. if running code one line at a time, any line with { in it will 
    # not execute until a corresponding } has been found (or entered on console)
    
  #----------------------------------------------------------------------------------------
  # Generate a sample from a multivariate normal distribution with the specified correlation matrix
  #----------------------------------------------------------------------------------------
  
  mydata<-mvrnorm(n = myN, mu = rep(myM,nVar), Sigma= myCov)
  # You can look at mydata by clicking on its label in the Environment tab
  #----------------------------------------------------------------------------------------
  # Save your simulated data as tab-separated text
  #----------------------------------------------------------------------------------------
    write.table(mydata, "simulated_data.txt", sep="\t") 
  # The matrix you have created, mydata, is saved as a text file called 'simulated_data' 
  #  in your working directory
  # The final part of the command specifies it is saved as tab-separated text.
  # N.B. This command is included just so you can see how to save simulated data.
      # see http://www.statmethods.net/input/exportingdata.html
  # Once you understand how this works, it is a good idea to comment out the
  # write.table line because:
  # There's actually not much point in saving the data, except if you are unconfident in R
  # and are interested in analysing simulated data in another statistical package.
  # (You can import this saved file into SPSS or Excel).
  # But in general, it makes sense to analyse your simulated data in R.
  # Note too that the data is always saved with the same name, so each time you run through the loop
  # it overwrites the previous version.
  # Also, you would seldom be interested in a single run of a simulation: 
  # Rather, you would repeat the simulation many times to look at the pattern 
  # of results - but if you overwrite the saved data each time, 
  # you just slow up the simulation and eat up memory
  #----------------------------------------------------------------------------------------
  # Now look at actual correlation matrix for this sample
  #----------------------------------------------------------------------------------------
    myresult<-rcorr(mydata) #produces correlation matrix and then corresponding p values
  
  myr <-myresult$r # correlation matrix
  myp <- myresult$P # p-values matrix
  
  colnames(myr)<-paste(rep("V",ncol(myr)),c(1:ncol(myr)),sep="") 
  #this is a clever way to assign sequential column names, V1, V2, ....etc .
  rownames(myr)<-colnames(myr)
  #----------------------------------------------------------------------------------------
  # You can look at the correlations by just typing myr at the console.
  # Are the correlations the same as you specified in myCorr?
  # If not, why not?
  #----------------------------------------------------------------------------------------
  
  myr[lower.tri(myr)] <- NA # lower triangle is mirror image of upper, so we ignore by making values NA
  
  myrf<-round(myr,digits=2) #get rid of extraneous digits
  #dev.off() #clear previous figures
  #grid.table(myrf) # Grid.table is a simple way to produce nicely formatted output - but this does not
  #highlight significant values of r - for that we need to get a bit more fancy
  
  #I had no idea how to do that, so I searched Google and found a script I could modify
  # https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
  # N.B. this won't change how myr or myrf displays on the console, but it allows you
  # to display conditionally formatted values in the Plots window
  
  # If you are new to R, this bit of script is fairly daunting: don't worry if you
  # can't follow it all
  #----------------------------------------------------------------------------------------
  # Set up visual display of grid: we'll use this just to look at the last simulation
  #----------------------------------------------------------------------------------------
    if (i==mynSims){  
    #If the number of simulations is less than i, everything between { and } is ignored
    g <- tableGrob(myrf) #creates an object specifying the details of the table; 
     # This doesn't display anything, but we can use g when manipulating the format
  
  #----------------------------------------------------------------------------------------
  # find_cell is a function defined within the program - taken from website for tableGrob
  # Functions are a bit complicated and you don't need to understand this now
  # So just include this block of code and don't try to unpack it.
  #----------------------------------------------------------------------------------------
    find_cell <- function(table, row, col){ 
    l <- table$layout
    which(l$t==row & l$l==col )
    } #end of function definition
  } #end of section defined by 'if' statement above
 
  #----------------------------------------------------------------------------------------
 #  Loop through each row and each column of matrix looking for pvalues < .05
  #----------------------------------------------------------------------------------------
  
  for (j in 1:(nVar-1)){ #looping through each row
    for (k in (j+1):nVar){ #and each column in upper triangle of matrix
      everyp<-c(everyp,myp[j,k])
      if(myp[j,k]<.05){ # looking for p values < .05
        
        if (i==mynSims){ #Again, we only do the next bit for final simulation
        ind2 <- find_cell(g, j+1, k+1) #add 1 to row and col to allow for col/row labels
        g$grobs[ind2][[1]][["gp"]] <- gpar(fill="red", col = "red", lwd=5)
        #marking this cell to be shown in red 
        } #end of 'if' statement re mynSims
        
     sigpcounter[i]<-sigpcounter[i]+1 #increment the significant p counter for this run of the simulation 
      } # end of 'if' statement re pvalues
    } # next k
  } # next j
  # We've now found all the significant p-values for this run
  if (i==mynSims){
   grid.draw(g) # show correlation matrix with significant r shown in red
    # if running many simulations, just do this for the last one
  }
} # end of loop that is iterated for each simulation. All simulations completed!
  
  # sigpcounter gives N p-values < .05 for each run 
  
  #You can then compute the proportion of 'significant' correlations in the whole set of runs
 if (mynSims>1){
   nCorrs<- sum(seq(1:(nVar-1))) #with 7 variables, N correlations is 1+2+3+4+5+6 = 21
  allCorrs <-nCorrs*mynSims # across all runs we have 21 correlations for each simulation
  overallp <- sum(sigpcounter)/allCorrs
  #hist(sigpcounter) #uncomment this to draw histogram showing N sig pvalues in all runs
  myoverview<-paste('Overall proportion of p < .05 is ',overallp)
 }
  myoverview
  # What do you expect to see for overallp if myCorr = 0?
  # What would you expect if myCorr = .3?
  # Would the sample size make any difference?
  # Would changing myM make any difference?
  # Try modifying the script by changing values of these parameters to get a feel for how
  # p-values are affected 
  
  
  #--------------------------------------------------------------------------------
  # A P-P plot will show whether distribution of p-values departs from null expectation
  #--------------------------------------------------------------------------------
  ppplot<-0 #Change this to 1 if you want to see the pp plot
  if (ppplot==1){
  everyp<-sort(everyp)
  np<-length(everyp)-1
  pexp<-seq(0,1,1/np)
  plot(pexp,everyp,cex=.5,ylab='Observed p-values',xlab='Expected p under null',main='P-P plot')
  lines(pexp,pexp) #expected distribution of p-values under null
  abline(h=.05,lty=2,col='red') #cutoff for p = .05
  pbit<-paste0(round(overallp*100,0),'% p-values < .05')
  text(.3,.7,pbit)
  text(.3,.9,paste('True r =',round(myCorr,3)))
  text(.3,.8,paste('N = ',myN))
  }
  
  #--------------------------------------------------------------------------------
  
  # This exercise should illustrate two things:
  # a) With a small sample, p-values are often nonsignificant even if the true correlation is quite large
  # Thus you can miss an important effect - make a type II error - because of low power
  # We will cover power later in the week
  
  # b) When you have several variables to look at, it is easy to find false positive 'significant' correlations.
 
  # Many people, seeing one large correlation in a matrix of null results, would
  # try to interpret it, but it is important to appreciate it might be a false positive.
  
  # One form of p-hacking is when the scientist trawls through a set of data looking for
  # 'significant' values, and then drops the non-significant findings  
  # and only reports those variables that have a significant association.
  
  # There are two ways of avoiding the errors introduced by p-hacking:
  # 1. Adjust the critical p-value to take into account the number of comparisons
  # 2. Explore data to generate a hypothesis about a correlation, but then test that hypothesis in a new sample
  
  # N.B. This program is NOT an example of good programming! It is written
  # to make it easy to understand if you run it line by line, but it is
  # very inefficient.
  # Anyone who would like a challenge could try improving it by using
  # functions to do the work inside the loops
  
  # This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/

  