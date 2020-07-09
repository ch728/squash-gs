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
	if(sum(is.na(all[x,5:7])) !=0 ){
		idx <- c(idx, count)
	}
	count = count + 1
}
all <- all[-idx, ]  # Remove entries with missing XYZ data
str(all)
lab <- t(apply(all[,5:7], 1, XYZ_to_LAB))  # Get L*a*b* values
GID <- str_replace(all$FruitID,"(GS3\\.1_[0-9]*)\\.[0-9]*", "\\1")
all.clean <- data.frame(FruitID =all$FruitID,Gid=GID, Weight=all$Weight, Length=all$Length,
                         Width=all$Width, Brix=all$Brix, DM=round(all$DM, 1),
                         L=round(lab[,1], 1),a=round(lab[,2], 1), b= round(lab[,3], 1), stringsAsFactors=F)
all.clean <- all.clean[-idx,]
str(all.clean)
write.csv(all.clean, "C2_final.csv", quote=F, row.names=F)