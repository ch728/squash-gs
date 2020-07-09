library(tidyverse)
library(readxl)
files <- list.files("./",pattern="*xlsx")
all.brix <- tibble()
f_count = 0
for(f in files){
    tmp <- read_excel(f)[,1:2]
    names(tmp) <- c("EntryID", "Brix")
    all.brix <- rbind(all.brix, tmp)
    f_count = f_count + 1
 }
print(paste("Number of files read:", f_count))
# Fix some typos
print(table(gsub("_.*", "", all.brix$EntryID)))
indx <- unlist(lapply(strsplit(all.brix$EntryID,split="-"),function(x) x[1])) == "8"
all.brix$EntryID[indx] <- "18-3723_Rep3_17.3"
print(table(gsub("_.*", "", all.brix$EntryID)))
indx2 <- unlist(lapply(strsplit(all.brix$EntryID, split="_"),function(x) x[1])) == "18-3723-2"
all.brix$EntryID[indx2] <- unlist(lapply(strsplit(all.brix$EntryID[indx2], split="-"),
              function(x) paste(x[1],"-",x[2],"_Rep",x[3],sep="")))
print(table(gsub("_.*", "", all.brix$EntryID)))
print(paste("Duplicates:", sum(duplicated(all.brix$EntryID))))
print(all.brix$EntryID[duplicated(all.brix$EntryID)])
dup <- all.brix$EntryID[duplicated(all.brix$EntryID)]
all.brix <- all.brix %>%
	filter(!(EntryID %in% dup))
print("Removed duplicates")

write.csv(all.brix, file="combined_brix.csv", quote=F, row.names=F) 
