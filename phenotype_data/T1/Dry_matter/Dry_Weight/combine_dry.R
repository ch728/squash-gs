library(tidyverse)
library(readxl)
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.dry <- tibble()  # initiate an empty table to hold all the data
f_count = 0
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)[,1:2]
	names(tmp) <- c("FruitID", "Dry")
	all.dry <- rbind(all.dry, tmp)
	f_count = f_count + 1
}
print(paste("Number of files read:", f_count))
all.dry <- as.data.frame(all.dry)
all.dry$Dry = as.numeric(all.dry$Dry)
# Standardizes FruitIDs
print(paste("Duplicates:", sum(duplicated(all.dry$FruitID))))  # Are there duplicated entries ?
dup <- all.dry$FruitID[duplicated(all.dry$FruitID)]  # Get names of duplicated entries
all.dry <- all.dry[!(all.dry$FruitID %in% dup),]   # remove duplicate entries	
str(all.dry)
write.csv(all.dry, "combined_dry.csv", row.names=F, quote=F)
