library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.brix <- tibble()  # initiate an empty table to hold all the data
f_count = 0
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	names(tmp) <- c("FruitID", "Brix")
	all.brix <- rbind(all.brix, tmp)
	f_count = f_count + 1
}
print(paste("Number of files read:", f_count))
all.brix <- as.data.frame(all.brix)
# Standardizes FruitIDs
all.brix[,1] <- gsub("g", "G", all.brix[,1])  # uppercase g's
all.brix[,1] <- gsub("s", "S", all.brix[,1])  # uppercase s's
all.brix <- all.brix[grepl("GS3.1", all.brix[,1]),]  # remove non-GS entries
all.brix <- all.brix[-which(all.brix$Brix > 25),] # remove outlier brix
print(paste("Duplicates:", sum(duplicated(all.brix$FruitID))))# Are there duplicated entries ?
str(all.brix)
write.csv(all.brix, "combined_brix.csv", row.names=F, quote=F)
