library(tidyverse)


gain <- read.csv("../data/gainTrials.csv", stringsAsFactors=F)
b <- as.matrix(read.csv("../data/index_weights.csv", row.names=1))
# Remove outlier dry matter value
gain <- filter(gain, DM < 30)

# Add index
gain$Index <- as.matrix(gain[,c("a", "Brix", "DM")]) %*% b

# Get Gids
gain$Gid <- gsub("\\.[0-9]*", "", gain$EntryID)
gain$Gid <- gsub("_Rep[0-9]_", "_", gain$Gid)
gain$Gid <- gsub("_[0-9]-[0-9]E", "_",  gain$Gid)
######Process  locations that have plant-level data########
gain2 <- filter(gain, Site %in% c("S2", "E"))

gain2 <- gain2 %>%
					filter(!Pop %in% c("Bugle", "Amber", "Honeynut")) %>%  
					group_by(Year, Site, Block, Pop, Plot, Gid) %>% 
					summarize(Weight=mean(Weight), 
					          Length = mean(Length),
                              Width =  mean(Width), 
                              Brix = mean(Brix),
                              DM =  mean(DM), 
                              L = mean(L), 
                              a = mean(a), 
                              b = mean(b),
                              Index=mean(Index))
gain2$Cycle <- as.numeric(gsub("C", "", gain2$Pop))

########Process location with plot-level data##########
gain3 <- filter(gain, Site %in% c("B", "O"), 
                !Pop %in% c("Bugle", "Amber", "Honeynut"))
gain3$Cycle <- as.numeric(gsub("C", "", gain3$Pop))

########Combined data##########
gain4 <- gain %>%
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
gain4$Cycle <- as.numeric(gsub("C", "", gain4$Pop)) + 1
gain4$Type <- ifelse(gain4$Site %in% c("B", "O"), 1, 2)
#####Cultivar comparison with final pop#####
cult_compare <- gain%>%
					filter(Pop %in% c("Bugle", "Amber", "Honeynut", "C4")) %>%  
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
                              
##########Output files############
write.csv(gain2, "../data/gain2.csv", row.names=F, quote=F)
write.csv(gain3, "../data/gain3.csv", row.names=F, quote=F)
write.csv(gain4, "../data/gain.csv", row.names=F, quote=F)
write.csv(cult_compare, "../data/cult_compare.csv", row.names=F, quote=F)
