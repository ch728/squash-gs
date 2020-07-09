library(tidyverse)
library(readxl)
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.wet <- tibble()  # initiate an empty table to hold all the data
f_count = 0
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)[,1:2]
	names(tmp) <- c("FruitID", "Wet")
	all.wet <- rbind(all.wet, tmp)
	f_count = f_count + 1
}
print(paste("Number of files read:", f_count))
all.wet <- as.data.frame(all.wet)
all.wet$Wet = as.numeric(all.wet$Wet)
# Standardizes FruitIDs
all.wet[,1] <- gsub("g", "G", all.wet[,1])  # uppercase g's
all.wet[,1] <- gsub("s", "S", all.wet[,1])  # uppercase s's
print(paste("Duplicates:", sum(duplicated(all.wet$FruitID))))  # Are there duplicated entries ?
dup <- all.wet$FruitID[duplicated(all.wet$FruitID)]  # Get names of duplicated entries
all.wet <- all.wet[!(all.wet$FruitID %in% dup),]   # remove duplicate entries	
str(all.wet)
write.csv(all.wet, "combined_wet.csv", row.names=F, quote=F)
