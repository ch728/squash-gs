library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.brix <- tibble()  # initiate an empty table to hold all the data
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	names(tmp) <- c("FruitID", "Brix")
	all.brix <- rbind(all.brix, tmp)
}

all.brix <- as.data.frame(all.brix)
dups = all.brix$FruitID[ duplicated(all.brix$FruitID)]  # Find duplicated entries
all.brix <- all.brix[!(all.brix$FruitID %in% dups),]  # Drop duplicated entries
write.csv(all.brix, "combined_brix.csv", quote=F, row.names=F)
