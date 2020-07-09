library(gdata)
library(lattice)
files<-list.files("./",pattern="GS*")

dates=c(1,2,3,4,5,8,9,10,11,12,16,17)
combined_wet<-data.frame()
i=1
 for(file in files){
    temp<-read.xls(file)
    temp<-cbind(temp,rep(dates[i],nrow(temp)),seq(1:nrow(temp)))
    colnames(temp)<-c("Name","wet_weight","wet_date","wet_order")
    combined_wet<-rbind(combined_wet,temp)
    i=i+1
 }

#fix typos 
str(combined_wet)
combined_wet$Name=as.character(combined_wet$Name)
table(unlist(lapply(strsplit(combined_wet$Name,split="_"),function(x) x[1])))
combined_wet=combined_wet[-which(unlist(lapply(strsplit(combined_wet$Name,split="_"),function(x) x[1]))=="17-18"),]
table(unlist(lapply(strsplit(combined_wet$Name,split="_"),function(x) x[1])))
combined_wet=combined_wet[-which(unlist(lapply(strsplit(combined_wet$Name,split="_"),function(x) x[1]))==" "),]
table(unlist(lapply(strsplit(combined_wet$Name,split="_"),function(x) x[1])))
combined_wet$Name<-toupper(combined_wet$Name)

#check for duplicates
sum(duplicated(combined_wet$Name))
combined_wet$Name[duplicated(combined_wet$Name)]
combined_wet[combined_wet$Name %in% combined_wet$Name[duplicated(combined_wet$Name)],]
combined_wet<-combined_wet[-764,]
#look at distribution
boxplot(combined_wet$wet_weight)
xyplot(wet_weight~wet_order|as.factor(wet_date),data=combined_wet)

#write out file
write.csv(combined_wet,file="./combined_wet.csv",row.names=F)

