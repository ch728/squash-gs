library(tidyverse)
library(plyr)
source("XYZ_to_Lab.R")
Brix <- read_csv(file="../Brix/combined_brix.csv")
DM <- read_csv(file="../Dry_matter/DM.csv")
CWD <- read_csv(file="../Fruit_color_shape/combined_CWD.csv")
all <- join_all(dfs=list(CWD, Brix, DM), by="FruitID")
idx <- c()
count  = 1
for(x in 1:nrow(all)){
	if(sum(is.na(all[x,6:10])) !=0 ){
		idx <- c(idx, count)
	}
	count = count + 1
}
all <- all[-idx, ]  # Remove entries with missing fruit quality data
str(all)
lab <- t(apply(all[,6:8], 1, XYZ_to_LAB))  # Get L*a*b* values
GID <- str_replace(all$FruitID,"(GS3_[0-9]*)\\.[0-9]*", "\\1")
all.clean <- data.frame(FruitID =all$FruitID,Gid=GID, Weight=all$Weight, Length=all$Length,
                         Width=all$Width, Brix=all$Brix, DM=round(all$DM, 1),
                         L=round(lab[,1], 1),a=round(lab[,2], 1), b= round(lab[,3], 1), stringsAsFactors=F)
str(all.clean)
write.csv(all.clean, "T1_final.csv", quote=F, row.names=F)