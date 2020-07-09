library(tidyverse)
library(readxl)
files <- list.files("./",pattern="*xlsx")
all.wet <- tibble()
f_count = 1
for(f in files){
    tmp <-read_excel(f)[,1:2]
    names(tmp) <- c("EntryID", "Wet")
    all.wet <- rbind(all.wet, tmp)
	f_count = f_count + 1
}
print(paste("Number of files read:", f_count))
#find and fix typos
table(unlist(lapply(strsplit(all.wet$EntryID,split="_"),function(x) x[1])))
indx <- unlist(lapply(strsplit(all.wet$EntryID,split="_"),function(x) x[1])) == "18-3742"
all.wet$EntryID[indx] <- "18-3724_Rep2_3.6"
all.wet$EntryID <- gsub("\\s+", "", all.wet$EntryID) # remove white space
indx2 <- unlist(lapply(strsplit(all.wet$EntryID, split="_"),function(x) x[1])) == "18-3723-2"
all.wet$EntryID[indx2] <- unlist(lapply(strsplit(all.wet$EntryID[indx2], split="-"), 
              function(x) paste(x[1],"-",x[2],"_Rep",x[3],sep="")))
all.wet$Wet <- as.numeric(all.wet$Wet)
dups <- all.wet$EntryID[duplicated(all.wet$EntryID)]
print(dups)
all.wet <- all.wet %>%
	filter(!(EntryID %in% dups))
#write out
write.csv(all.wet, file="./combined_wet.csv", row.names=F, quote=F)



