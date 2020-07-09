source("XYZ_to_Lab.R")
all <- read.csv("combined_all.csv", header=T, stringsAsFactors=F)
remove <- c("CWD_date", "FruitID.1", "Brix_date", "FruitID.2", "DM_date")
all <- all[,!(colnames(all) %in% remove)]
lab <- t(apply(all[,c("X","Y","Z")], 1, XYZ_to_LAB))  # get L*a*b* colorspace values
GID = gsub("\\.[0-9]*", "", all$FruitID)
GID = gsub("\\(.*\\)", "", GID)
all$FruitID = gsub("\\(.*\\)", "", all$FruitID)
all.clean <- data.frame(FruitID =all$FruitID,Gid=GID, Weight=all$Weight, Length=all$Length,
                         Width=all$Width, Brix=all$Brix, DM=round(all$DM, 1),
                         L=round(lab[,1], 1),a=round(lab[,2], 1), b= round(lab[,3], 1), stringsAsFactors=F)
str(all.clean)
write.csv(all.clean, "C0_final.csv", quote=F, row.names=F)