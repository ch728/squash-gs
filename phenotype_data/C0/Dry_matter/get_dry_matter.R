wet.dry <- read.csv("combined_wet_dry.csv", header=T, stringsAsFactors=F)
print(sum(wet.dry$FruitID != wet.dry$FruitID.1))  # check that the order is the same
dry.matter = (wet.dry$Dry/wet.dry$Wet)*100  # calculation for dry matter
out.file = data.frame(FruitID=wet.dry$FruitID, DM= dry.matter ,DM_date= wet.dry$Wet_date,
					  stringsAsFactors=F)  
print(str(out.file))  # check dataframe structure
write.csv(out.file, "DM.csv", quote=F, row.names=F)  # write out dataframe