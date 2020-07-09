library(tidyverse)
library(readxl)
library(stringr)
files<-list.files("./",pattern="xlsx")
all.CWD <- tibble()
for(f in files){
    tmp<-read_excel(f)[,1:7]
    names(tmp)<-c("EntryID", "Weight", "Length", "Width", "X", "Y", "Z")
    all.CWD<-rbind(all.CWD,tmp)
}
all.CWD$Length <- as.numeric(str_replace(all.CWD$Length, " cm", ""))  # strip 'cm' and make numeric
all.CWD$Width <- as.numeric(str_replace(all.CWD$Width, " cm", ""))  # strip 'cm' and make numeric
all.CWD$Weight <- as.numeric(str_replace(all.CWD$Weight, "g", ""))  # strip 'g' and make numeric
#find and fix typos
table(unlist(lapply(strsplit(all.CWD$EntryID,split="_"),function(x) x[1])))
indx <- unlist(lapply(strsplit(all.CWD$EntryID,split="_"),function(x) x[1])) == "18-3723-2"
all.CWD$EntryID[indx] <- unlist(lapply(strsplit(all.CWD$EntryID[indx], split="-"), 
              function(x) paste(x[1],"-",x[2],"_Rep",x[3],sep="")))

print(sum(duplicated(all.CWD$EntryID)))
dup <- all.CWD$EntryID[duplicated(all.CWD$EntryID)]
all.CWD <- all.CWD %>%
	filter(!(EntryID %in% dup))
write.csv(all.CWD, file="./combined_CWD.csv", row.names=F, quote=F)


