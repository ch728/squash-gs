library(tidyverse)
source("XYZ_to_Lab.R")
Brix <- read_csv(file="../Brix/combined_brix.csv")
DM <- read_csv(file="../Dry_matter/DM.csv")
CWD <- read_csv(file="../Fruit_color_shape/combined_CWD.csv")
all <- plyr::join_all(dfs=list(CWD, Brix, DM), by="EntryID")
lab <- t(apply(as.matrix(all[,5:7]) , 1, XYZ_to_LAB))
all$Pop <- gsub("(18-[0-9]*)_.*", "\\1", all$EntryID)
all$Year <- rep(2018, nrow(all))
all$Site <- rep("S2", nrow(all))
all$Block <- gsub(".*_Rep([0-9])_.*", "\\1", all$EntryID)
all$BlockRep <- rep(1, nrow(all))
all$Gid <- gsub("(.*)\\.[0-9]", "\\1", all$EntryID)
all$Fruit <- gsub(".*\\.([0-9])", "\\1", all$EntryID)
all.final <- data.frame(EntryID=all$EntryID, Pop=all$Pop, Year=all$Year,
                        Site=all$Site, Block=all$Block, BlockRep=all$BlockRep,
                        Gid=all$Gid, Fruit=all$Fruit, Weight=all$Weight,
                        Length=all$Length, Width=all$Width, Brix=all$Brix, DM=all$DM,
                        L=lab[,1], a=lab[,2],b=lab[,3])
write.csv(all.final, "Gain2018_final.csv", quote=F, row.names=F)