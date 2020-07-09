library(tidyverse)

Dry <- read_csv("./Dry_Weight/combined_dry.csv")
Wet <- read_csv("./Wet_Weight/combined_wet.csv")
Wet.Dry <- plyr::join(Dry, Wet, by="Name")
DM <- (Wet.Dry$dry_weight/Wet.Dry$wet_weight) * 100
write.csv(tibble(EntryID = Wet.Dry$Name, DM = DM), "DM.csv", quote =F, row.names=F)
