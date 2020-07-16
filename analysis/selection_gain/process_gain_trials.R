library(tidyverse)


#### Read in gain trial data and index weights ####
gain <- read.csv("gainTrials.csv", stringsAsFactors=F) 
b <- as.matrix(read.csv("index_weights.csv", row.names=1))
#### Remove outlier dry matter value ####
gain <- filter(gain, DM < 30)
gain$Gid <- gsub("\\.[0-9][0-9]", "", gain$Gid)

# #### Add index colum using weights #####
gain$Index <- as.matrix(gain[,c("a", "Brix", "DM")]) %*% b
# 
# #### Get average over plants for plot value ####

# Get average of fruit data per plant
gain_avg <- gain %>%
					filter(!Pop %in% c("Bugle", "Amber", "Honeynut")) %>%
					group_by(Year, Site, Block, Pop, Plot, Gid) %>%
					summarize(Weight= round(mean(Weight, na.rm=T), 2),
					          Length = round(mean(Length, na.rm=T), 2),
                              Width =  round(mean(Width, na.rm=T), 2),
                              Brix = round(mean(Brix, na.rm=T), 2),
                              DM =  round(mean(DM, na.rm=T), 2),
                              L = round(mean(L, na.rm=T), 2),
                              a = round(mean(a, na.rm=T), 2),
                              b = round(mean(b, na.rm=T), 2),
                              Index = round(mean(Index, na.rm=T), 2))

# Get average over plants for plot
gain_avg <- gain_avg %>%
					filter(!Pop %in% c("Bugle", "Amber", "Honeynut")) %>%
					group_by(Year, Site, Block, Pop, Plot) %>%
					summarize(Weight= round(mean(Weight, na.rm=T), 2),
					          Length = round(mean(Length, na.rm=T), 2),
                              Width =  round(mean(Width, na.rm=T), 2),
                              Brix = round(mean(Brix, na.rm=T), 2),
                              DM =  round(mean(DM, na.rm=T), 2),
                              L = round(mean(L, na.rm=T), 2),
                              a = round(mean(a, na.rm=T), 2),
                              b = round(mean(b, na.rm=T), 2),
                              Index = round(mean(Index, na.rm=T), 2))
                             
gain_avg$Cycle <- as.numeric(gsub("C", "", gain_avg$Pop)) + 1
gain_avg$Type <- ifelse(gain_avg$Site %in% c("B", "O"), 1, 2)
                              
#### Output files ####
write.csv(gain_avg, "gain.csv", row.names=F, quote=F)

