library(tidyverse)
library(readxl)
######################################
####This script combines CWD data#####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsm")  # get list of files
all.CWD <- tibble()  # initiate an empty table to hold all the data
counter = 1
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	names(tmp) <- c("FruitID", "Weight","Length", "Width", "X", "Y", "Z")
	all.CWD <- rbind(all.CWD, tmp)
}
all.CWD <- all.CWD[str_detect(all.CWD$FruitID, "GS3.1"),]  # only keep GS entries
all.CWD$Length <- as.numeric(str_replace(all.CWD$Length, " cm", ""))  # strip 'cm' and make numeric
all.CWD$Width <- as.numeric(str_replace(all.CWD$Width, " cm", ""))  # strip 'cm' and make numeric
print(paste("Duplicates:", sum(duplicated(all.CWD[,1]))))
dups <- all.CWD$FruitID[duplicated(all.CWD$FruitID)]
all.CWD <- all.CWD[-which(all.CWD$FruitID %in% dups),]
write.csv(all.CWD, "combined_CWD.csv", quote=F, row.names=F)