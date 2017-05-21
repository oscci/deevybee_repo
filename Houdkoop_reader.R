#Analysis of Houdoop et al data from OSF https://osf.io/xm8dh/
# NB converted data to csv after removing some rows that were clearly
# erroneous: see rows (where header row treated as row zero)
#113, 431, 430, 67, 66, 112

setwd("~/Dropbox/collaborators 2016-/houdoop")
mydata = read.csv("datasharingpsychology0404.csv")  # read csv file 
mydata$year_birth[mydata$year_birth<20]<-NA

plot(mydata$year_birth,mydata$shared_data_in_past)
cor(mydata$year_birth,mydata$shared_data_in_past,use='complete.obs')

youngdata<-mydata[which(mydata$year_birth<45),]
olddata<-mydata[which(mydata$year_birth>44),]
Nyoungshare<-length(which(youngdata$shared_data_in_past>10))
Noldshare<-length(which(olddata$shared_data_in_past>10))
peryoungshare<-Nyoungshare*100/nrow(youngdata)
peroldshare<-Noldshare*100/nrow(olddata)

paste('Percent young >10% share = ',peryoungshare)
paste('Percent old >10% share = ',peroldshare)

Nyoungshare<-length(which(youngdata$shared_data_in_future>3))
Noldshare<-length(which(olddata$shared_data_in_future>3))
peryoungshare<-Nyoungshare*100/nrow(youngdata)
peroldshare<-Noldshare*100/nrow(olddata)

paste('Percent young 3 or 4 response future share = ',peryoungshare)
paste('Percent old 3 or 4 response future share = ',peroldshare)