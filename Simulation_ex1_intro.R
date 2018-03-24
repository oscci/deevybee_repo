#-------------------------------------------------------------------------
# Simulation introduction by DVM Bishop
# 19th March 2017; updated 24th March 2018
#-------------------------------------------------------------------------

# The hashtag at the start of this line indicates it is a comment
# Commented lines explain the script. They do not do anything when you run a script
# It is good practice to include lots of comments in your code so that you and others
# can make sense of it when returning to it after a delay
#-------------------------------------------------------------------------
# Preliminaries
#-------------------------------------------------------------------------

# Also good practice to explicitly set your working directory, at start of script
# This is the default location for finding/saving files
# You can see what your current working directory is with command getwd()
# Here is command to set working directory to Dropbox/BBSCR_STARS/Bishop on my mac

setwd('/Users/dorothybishop/Dropbox/BBSRC_STARS/Bishop')
# Note that this line has no #, and so will be treated as executable code 


# If you are working on a PC, the syntax is a bit different, e.g.
# setwd<-"C:\\Users\\dorothybishop\\Dropbox\\BBSRC_STARS\\Bishop"
# You will need to set the directory to be specific for your machine

# You can easily set your working directory by going to menu item 
# Session|Set Working Directory - this allows you to select Source File Location, which
# sets the working directory to same place as where you have saved this script

# Next step is to specify any packages that are needed later in the program
# R has hundreds of packages that contain useful functions
# You can use the library() or require() commands to tell R which ones you need, as below,
# But! these will only work if the packages are already installed on your machine
# When you try to use functions from a package you don't have, you'll get an error message.
# If that happens, you can download the package by going to Tools|Install Packages 
# and selecting the one you need. You only need to install the package once.

library(MASS) # for mvrnorm function to make multivariate normal distributed vars
library(Hmisc) # for rcorr function: gives correlation matrix and p values
library(yarrr) # for pirate plots (see below)
# (We are not using the first two in this exercise, but will need them later)

# It's also a good idea to disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r

plot.new() # open graphics window.
# ------------------------------------------------------------------------
# Simulate random numbers sampled from a normal distribution
# ------------------------------------------------------------------------
# You just specify the number (myN), mean (myM) and SD (mySD)
# Naming conventions vary: I tend to preface my variables with 'my' to avoid confusion with
# any pre-existing R functions etc
myN <- 20 #You could type myN = 20 to get the same result, but R purists prefer <- to assign a value
myM1 <- 0
mySD <- 1 #I've picked mean 0 and SD 1 to simulate z-scores

myvectorA<-rnorm(n = myN, mean = myM1, sd = mySD) #Simulate data with specified mean, standard deviation, and sample size
# rnorm generates a random sample from a normal distribution
# for any function, you can use help for more information, e.g. help(rnorm)

# To see the values you have generated, just type 'myvectorA' on the Console
myvectorA #View myvectorA on the console
mean(myvectorA) # View the mean of myvectorA
sd(myvectorA) # View the sd of myvectorA
# ------------------------------------------------------------------------
# Questions
# ------------------------------------------------------------------------

# The odds are that the mean and sd of the vector are not exactly 0 and 1
# Why is this?
# How would you modify this part of the script to make them closer to 0 and 1?

# ------------------------------------------------------------------------
# Create another sample with same N and SD but different mean
# ------------------------------------------------------------------------
myM2 <- 0.3 # The mean for this population

myvectorB<-rnorm(n = myN, mean = myM2, sd = mySD) 
myvectorB
mean(myvectorB)
sd(myvectorB)
# N.B. Unless you use a command called set.seed, you'll get different values for 
# myM1 and myM2 every time you run the script.
# For explanation of set.seed see http://rfunction.com/archives/62 
# ------------------------------------------------------------------------
# compare our two vectors with a t.test
# ------------------------------------------------------------------------
t.test(myvectorA,myvectorB) #see http://www.statmethods.net/stats/ttest.html

# ------------------------------------------------------------------------
# reformat data into a data frame
# A data frame is a kind of matrix for holding data that can take both
# numeric and non-numeric values. 
# ------------------------------------------------------------------------

# We're now going to stack vectorA and vectorB in a single column in a data frame
# with another column to designate the group

# First we'll make a dummy data frame to hold the data
mydf <- data.frame(matrix(data=NA,nrow=myN*2,ncol=2)) #NA indicates missing data
colnames(mydf)<-c('Group','Score') #c is concatenate function, puts what follows in a sequence
# colnames gives the columns' names, which can be used later on to refer to them in formulae

# An easy way to inspect mydf is to click on the term in the Environment tab

# if you look at mydf at this point, you'll see 40 rows and 2 columns, with NA values in each
# Now we are going to populate the data frame with values
range1<-1:myN #range of rows for group 1 data
range2<-(myN+1):(2*myN) #range of rows for group 1 data
range1
range2
# to see the vectors for range1 and range2

 mydf$Group[range1]<-1 # First set of rows allocated to group 1

# Square brackets used to refer to sections of a data frame
# Here we specify the column first with $Group, and then the range of rows.
# Same effect could be achieved with mydf[range1,1], indicating rows 1:myN, and column 1
 
 mydf$Group[range2]<-2 # Next block of rows allocated to group 2
 mydf$Score[range1] <- myvectorA # First block in Score column is vectorA
 mydf$Score[range2]<-myvectorB # 2nd block in Score column is vectorB
 
 #Now inspect mydf: you have data in the kind of table familiar from Excel or SPSS
#------------------------------------------------------------------------------- 
# Saving results of statistical analysis as a variable
#------------------------------------------------------------------------------- 
 myt <- t.test(myvectorA,myvectorB) #see http://www.statmethods.net/stats/ttest.html
 # Same t-test as before, but this time instead of displaying the result, we
#  save it as myt. If you look at myt, you will see it is a complex variable
#  containing a set of results on different lines.
#  You can select just part of the output using $. For instance myt$p.value gives
#  just the p-value.
 #------------------------------------------------------------------------------- 
 # Pirateplot of mydf
 #------------------------------------------------------------------------------- 
 dev.off() #Clears the plot window
 # If you don't clear it, each plot will be retained and can be viewed in the Plots window
 # But you may run out of memory eventually if you store too may plots
 
 # Turn the t-test result into a header for the graph;
 myheader=paste0('t = ', format(myt$statistic,digits=2),'; p = ',format(myt$p.value,digits=3))
 # paste0 is used to bolt together text and/or numbers into a string
 # format is used to select number of decimal places to avoid v long string of numbers
 myheader
 
pirateplot(Score~Group,data=mydf,main=myheader,   xlab="Group", ylab="Score")
# The pirateplot is a very neat way of showing all the data
# See help(pirateplot) or just Google 'pirateplot' for more information.
# You can substitute 'boxplot' for a more conventional plot
# Either type of plot is far superior to usual bar graph

#------------------------------------------------------------------------------- 
# Explore the simulation
#------------------------------------------------------------------------------- 

# Try re-running the script several times
# If N is small, you will see what Geoff Cumming has called 'the dance of the 
# p-values'. Even though you don't change the N or mean or SD of the population, 
# the values dance around from run to run.
# https://www.youtube.com/watch?v=5OL1RqHrZQ8

# Once you have experienced the p-values dancing, save the script under a new name
# You can then play around with the new version without worrying about losing the original
# Now try varying myN and/or myM1 or myM2 and re-running the script

# What happens when myN is very large?

# How often do you get a significant p-value when myvectorA and myvectorB have the same mean?

# Once you feel you understand this script, you can look at:
# Simulation_ex1_multioutput.R
# This is a pared-down version of this script which automatically runs 10 times for each of 2 sample sizes








# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/

  