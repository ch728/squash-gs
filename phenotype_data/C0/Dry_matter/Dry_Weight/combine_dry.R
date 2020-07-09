library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.dry <- tibble()  # initiate an empty table to hold all the data
counter = 1
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	tmp <- cbind(tmp, rep(counter ,nrow(tmp)))
	names(tmp) <- c("FruitID", "Dry","Dry_date")
	all.dry <- rbind(all.dry, tmp)
	counter = counter + 1
}

all.dry <- as.data.frame(all.dry)
# Standardizes FruitIDs
all.dry[,1] <- gsub("g", "G", all.dry[,1])  # uppercase g's
all.dry[,1] <- gsub("s", "S", all.dry[,1])  # uppercase s's
all.dry[,1] <- gsub("-", "_", all.dry[,1])  # replace '-' with '_'
all.dry <- all.dry[grepl("GS1", all.dry[,1]),]  # remove non-GS entries
write.csv(all.dry, "combined_dry.csv", row.names=F, quote=F)


