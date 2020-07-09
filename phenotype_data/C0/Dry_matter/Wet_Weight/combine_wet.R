library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.wet <- tibble()  # initiate an empty table to hold all the data
counter = 1
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)[,1:2]
	tmp <- cbind(tmp, rep(counter ,nrow(tmp)))
	names(tmp) <- c("FruitID", "Wet","Wet_date")
	all.wet <- rbind(all.wet, tmp)
	counter = counter + 1
}

all.wet <- as.data.frame(all.wet)
# Standardizes FruitIDs
all.wet[,1] <- gsub("g", "G", all.wet[,1])  # uppercase g's
all.wet[,1] <- gsub("s", "S", all.wet[,1])  # uppercase s's
all.wet[,1] <- gsub("-", "_", all.wet[,1])  # replace '-' with '_'
all.wet <- all.wet[grepl("GS1", all.wet[,1]),]  # remove non-GS entries
write.csv(all.wet, "combined_wet.csv", row.names=F, quote=F)


