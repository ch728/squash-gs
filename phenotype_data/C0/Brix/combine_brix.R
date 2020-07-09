library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.brix <- tibble()  # initiate an empty table to hold all the data
date_processed <- c(5,1,2,3,4,6,7,8,9,10,10,10)
counter = 1
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	tmp <- cbind(tmp, rep(date_processed[counter],nrow(tmp)))
	names(tmp) <- c("FruitID", "Brix","Brix_date")
	all.brix <- rbind(all.brix, tmp)
	counter = counter + 1
}

all.brix <- as.data.frame(all.brix)
# Standardizes FruitIDs
all.brix[,1] <- gsub("g", "G", all.brix[,1])  # uppercase g's
all.brix[,1] <- gsub("s", "S", all.brix[,1])  # uppercase s's
all.brix[,1] <- gsub("-", "_", all.brix[,1])  # replace '-' with '_'
all.brix <- all.brix[grepl("GS1", all.brix[,1]),]  # remove non-GS entries
write.csv(all.brix, "combined_brix.csv", row.names=F, quote=F)


