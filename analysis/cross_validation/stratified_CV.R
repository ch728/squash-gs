library(DBI)
library(RSQLite)
library(tidyverse)
library(asreml)  
library(doMC)
source("CV_functions.R")


# Set up a parallel backend
registerDoMC(12)

# Read in  phenotypic data from project database
con <- dbConnect(RSQLite::SQLite(),"../../pheno/pheno.db")  # Connect to pheno database
res  <-  dbSendQuery(con, "SELECT Gid, TotalFrtCt, TotalWtKg FROM trainingYield")
YLD <- dbFetch(res)
dbClearResult(res)
res <- dbSendQuery(con, "SELECT Gid, Pop,  avg(Brix) AS Brix, avg(DM) AS DM,
                   avg(a) AS a, avg(b) AS b, avg(L) as L,
                   avg(Weight) AS Weight, avg(Length) AS Length,
                   avg(Width) AS Width, avg(Length/Width) AS Shape FROM trainingData GROUP BY Gid")
pheno.avg <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con) 
pheno.avg <- plyr::join(pheno.avg, YLD, by="Gid", type="inner")
pheno.avg <- mutate(pheno.avg, Y3=TotalWtKg * (DM/100))
traits <- colnames(pheno.avg)[3:14]

# Run stratified cross validation
seeds <- sample.int(10000000, 300)  # Get random seeds
all.geno <- as.matrix(read.table("../../geno/C0_T1_C2_kinship.txt", skip=3, row.names=1)) # Read in kinship matrix
diag(all.geno) <- diag(all.geno) + 0.00001
all.ginv <- solve(all.geno)  # Get inverse of kinship matrix
rownames(all.ginv) <- rownames(all.geno)  # Add rownames to inverse matrix
all.pheno <- pheno.avg %>% filter(Gid %in% rownames(all.ginv))
pop.sizes <- c(48, 99, 150, 201, 249, 300, 348, 402)
pop.res <- foreach(i=1:length(traits), .combine=rbind) %dopar%{
	full <- tibble()
    for(s in pop.sizes){  # Diffrent training pop sizes
        tmp.res <- asremlStrat(sets=c("C0", "C2", "T1"), pheno=all.pheno, trait=traits[i], ginv=all.ginv,
                               weights=NULL, size.train=s, size.test=75, nrep=50, seeds=seeds)
        full <- rbind(full, tmp.res)
    }
    return(full)
}
write.csv(pop.res, "strat_predictions.csv", quote=F, row.names=F)
