library(tidyverse)
library(readxl)

files <- list.files("./",pattern="*GS")
all.CWD <- tibble()
f_count <-  0
for(f  in files){
	tmp <- read_excel(f)
	names(tmp) <- c("EntryID", "Skin", "Weight", "Length", "Width", "X", "Y","Z")
	all.CWD <- rbind(all.CWD, tmp) 
	f_count <- f_count + 1	
}
print(f_count)
# Strip out 'cm' from weight and length
all.CWD$Length <- as.numeric(str_replace(all.CWD$Length, " cm", ""))
all.CWD$Width <- as.numeric(str_replace(all.CWD$Width, " cm", ""))
write.csv(all.CWD, file="combined_CWD.csv", row.names=F)  # Write out data
