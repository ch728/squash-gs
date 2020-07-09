library(tidyverse)
yield <- read_csv(file="GS_3.1_yield_weight.csv")
yield <- yield[!str_detect(yield$Name,"[a-z]"), ]  # Only retain GS entries
yield$Name <- paste("GS3.1", yield$Name, sep="_")
colnames(yield) <- c("Gid", "TotalFrtCt", "TotalWtKg")
write.csv(yield, "C2_yield.csv", quote=F, row.names=F)