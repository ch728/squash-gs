library(gdata)
library(lattice)
files<-list.files("./",pattern="GS*")

dates=c(1,2,3,4,5,8,9,10,11,12,16,17)
combined_dry<-data.frame()
i=1
 for(file in files){
    temp<-read.xls(file)
    temp<-cbind(temp,rep(dates[i],nrow(temp)),seq(1:nrow(temp)))
    colnames(temp)<-c("Name","dry_weight","dry_date","dry_order")
    combined_dry<-rbind(combined_dry,temp)
    i=i+1
 }

#fix typos
str(combined_dry)
combined_dry$Name=as.character(combined_dry$Name)
table(unlist(lapply(strsplit(combined_dry$Name,split="_"),function(x) x[1])))
combined_dry=combined_dry[-which(unlist(lapply(strsplit(combined_dry$Name,split="_"),function(x) x[1]))=="17-18"),]
combined_dry$Name<-toupper(combined_dry$Name)
table(unlist(lapply(strsplit(combined_dry$Name,split="_"),function(x) x[1])))

#check for duplicates
sum(duplicated(combined_dry$Name))

#look at distribution
boxplot(combined_dry$dry_weight)
xyplot(dry_weight~dry_order|as.factor(dry_date),data=combined_dry)

#write out file
write.csv(combined_dry,file="./combined_dry.csv",row.names=F)


