library(tidyverse)
library(readxl)
files <- list.files("./",pattern="*xlsx")
print(files)
all.dry <- tibble()
f_count = 0
for(f in files){
    tmp <- read_excel(f)[,1:2]
    names(tmp) <- c("EntryID", "Dry")
    all.dry <- rbind(all.dry, tmp)
    f_count = f_count + 1
 }
print(f_count)
#find and fix typos
indx <- unlist(lapply(strsplit(all.dry$EntryID,split="_"),function(x) x[1])) == "18"
all.dry$EntryID[indx] <- "18-3725_Rep1_10.2"
indx2 <- unlist(lapply(strsplit(all.dry$EntryID,split="_"),function(x) x[1])) == "18-3723-2"
all.dry$EntryID[indx2] <- unlist(lapply(strsplit(all.dry$EntryID[indx2], split="-"),
              function(x) paste(x[1],"-",x[2],"_Rep",x[3],sep="")))
print(paste("Duplicates:", sum(duplicated(all.dry$EntryID))))
dup <- all.dry$EntryID[duplicated(all.dry$EntryID)]
all.dry <- all.dry %>%
	filter(!(EntryID %in% dup))
write.csv(all.dry, file="./combined_dry.csv", row.names=F, quote=F)


