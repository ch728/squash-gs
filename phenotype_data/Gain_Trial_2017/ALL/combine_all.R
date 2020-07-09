library(plyr)
library(tidyverse)
source("XYZ_to_Lab.R")
CWD <- read_csv(file="../CWD/combined_CWD.csv")
Brix <- read_csv(file="../Brix/combined_brix.csv")
DM <- read_csv(file="../Dry_matter/DM.csv")
CWD <- CWD[,-2]  # Get rid of skin category
all <- plyr::join_all(dfs=list(CWD, Brix, DM), by="EntryID")
lab <- t(apply(as.matrix(all[, 5:7]) , 1, XYZ_to_LAB))
# Fix soem persistent typos
all$EntryID <- gsub(" ", "_", all$EntryID)
all$EntryID <- toupper(all$EntryID)
#Parse info from plant ID
all$Site <- ifelse(grepl("E", all$EntryID), "E",
           ifelse(grepl("B", all$EntryID), "B",
           ifelse(grepl("O", all$EntryID), "O", NA)))
all$Year <- rep(2017, nrow(all))
all$Pop <- gsub("(17-.*)_.*", "\\1", all$EntryID)
G.pops <- all$Pop %in% c("17-180", "17-181", "17-182", "17-187")
c.pops <- !(all$Pop %in% c("17-180", "17-181", "17-182", "17-187"))
all$BlockRep <- rep(0, length(c.pops))
all$BlockRep[c.pops] <- 1Q
all$BlockRep[G.pops] <- gsub(".*([0-9])[A-Z].*", "\\1", all$EntryID[G.pops])
all$Block <- rep(0, length(c.pops))
all$Block[c.pops] <- gsub(".*([0-9])[A-Z].*", "\\1", all$EntryID[c.pops])
all$Block[G.pops] <- gsub(".*_([0-9])-.*", "\\1" , all$EntryID[G.pops])
all$Gid <- rep(0, length(c.pops))
all$Gid[which(all$Site != "E")] <- all$EntryID[which(all$Site != "E")]
all$Gid[which(all$Site == "E")] <- gsub("(.*)\\..*", "\\1", all$EntryID[which(all$Site == "E")])
all$Fruit <- as.numeric(gsub(".*\\.([0-9])", "\\1", all$EntryID))
all.final <- data.frame(EntryID=all$EntryID, Pop=all$Pop, Year=all$Year,
                        Site=all$Site, Block=all$Block, BlockRep=all$BlockRep,
                        Gid=all$Gid, Fruit=all$Fruit, Weight=all$Weight,
                        Length=all$Length, Width=all$Width, Brix=as.numeric(all$Brix), DM=all$DM,
                        L=lab[,1], a=lab[,2],b=lab[,3])
write.csv(all.final, "Gain2017_final.csv", quote=F, row.names=F)
