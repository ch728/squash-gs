
library(tidyverse)
library(readxl)
files <- list.files("./",pattern="*GS")

all.brix <- tibble()
f_count <- 0
for(f in files){
    tmp <- read_excel(f)[,1:2]
    names(tmp) <- c("EntryID", "Brix")
    all.brix  <- rbind(all.brix, tmp)
    f_count <- f_count + 1
}
print(f_count)
# Find and fix typos
print(table(unlist(lapply(strsplit(all.brix$EntryID,split="_"),
      function(x) x[1]))))
all.brix[which(unlist(lapply(strsplit(all.brix$EntryID,split="_"),
         function(x) x[1]))=="17-182--2-1O.12"),]="17-182_2-1O.12"
print(table(unlist(lapply(strsplit(all.brix$EntryID ,split="_"), function(x) x[1]))))
all.brix$EntryID <- toupper(all.brix$EntryID)
# check for duplicates
dup <- all.brix$EntryID[duplicated(all.brix$EntryID)]
# Drop duplicated samples
all.brix <- all.brix %>%
	filter(!(EntryID %in% dup))
# Write out
write.csv(all.brix, file="./combined_brix.csv", row.names=F, quote=F)


