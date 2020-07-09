library(tidyverse)
library(readxl)
######################################
####This script combines brix data####
######################################
# Reads all files into one dataframe (tibble)
files <- list.files(path=".", pattern="*xlsx")  # get list of files
all.CWD <- tibble()  # initiate an empty table to hold all the data
counter = 1
for(f in files){  # loop over list of files, adding to all.brix
	tmp <- read_excel(f)
	tmp <- cbind(tmp, rep(counter ,nrow(tmp)))
	names(tmp) <- c("FruitID", "Weight","Length", "Width", "X", "Y", "Z", "CWD_date")
	all.CWD <- rbind(all.CWD, tmp)
	counter = counter + 1
}

all.CWD <- as.data.frame(all.CWD)
# Standardizes FruitIDs
all.CWD[,1] <- gsub("g", "G", all.CWD[,1])  # uppercase g's
all.CWD[,1] <- gsub("s", "S", all.CWD[,1])  # uppercase s's
all.CWD[,1] <- gsub("-", "_", all.CWD[,1])  # replace '-' with '_'
all.CWD <- all.CWD[grepl("GS1", all.CWD[,1]),]  # remove non-GS entries
# strip away "CM"
all.CWD$Length <- as.numeric(gsub(" cm", "", all.CWD$Length, ignore.case=T))
all.CWD$Width <- as.numeric(gsub(" cm", "", all.CWD$Width, ignore.case=T))
# remove fruit with missing color data
ind <- c()
for(x in 1:nrow(all.CWD)){
	if(sum(is.na(all.CWD[x, 5:7])) > 0){
		ind <- c(ind, x)
	}
}
all.CWD <- all.CWD[-ind,]
# write out
write.csv(all.CWD, "combined_CWD.csv", row.names=F, quote=F)
