library(DBI)
library(tidyverse)
library(asreml)
library(doMC)
source("CV_functions.R")


# Set up parallel backend
registerDoMC(12)

# Read in  phenotypic data from project database
con <- dbConnect(RSQLite::SQLite(),"../../pheno/pheno.db")  # Connect to pheno database
res <- dbSendQuery(con, "SELECT * FROM trainingData")
pheno <- dbFetch(res)
dbClearResult(res)
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

# Start running differnt cross validation test sets
# C0  within
seeds <- sample.int(10000000,50)
C0.geno <- as.matrix(read.table("../../geno/filtered_C0_kinship.txt", skip=3, row.names=1)) # Read in kinship matrix
diag(C0.geno) <- diag(C0.geno) + 0.000001
C0.ginv <- solve(C0.geno)  # Get inverse of kinship matrix
rownames(C0.ginv) <- rownames(C0.geno)  # Add rownames to inverse matrix
C0.pheno <- pheno.avg %>% filter(Gid %in% rownames(C0.ginv))
C0.res <- foreach(i=1:length(traits), .combine=rbind) %dopar%
				 asremlWithinUni(pheno=C0.pheno, trait=traits[i], set="C0_uni", ginv=C0.ginv,
                                weights=NULL, train.prop=0.8, nrep=50, seeds=seeds)
# C2 within
seeds <- sample.int(10000000,50)
C2.geno <- as.matrix(read.table("../../geno/filtered_C2_kinship.txt", skip=3, row.names=1)) # Read in kinship matrix
diag(C2.geno) <- diag(C2.geno) + 0.000001
C2.ginv <- solve(C2.geno)  # Get inverse of kinship matrix
rownames(C2.ginv) <- rownames(C2.geno)  # Add rownames to inverse matrix
C2.pheno <- pheno.avg %>% filter(Gid %in% rownames(C2.ginv))
C2.res <- foreach(i=1:length(traits), .combine=rbind) %dopar%
				 asremlWithinUni(pheno=C2.pheno, trait=traits[i], set="C2_uni", ginv=C2.ginv,
                                 weights=NULL, train.prop=0.8, nrep=50, seeds=seeds)
# #T1 within
seeds <- sample.int(10000000,50)
T1.geno <- as.matrix(read.table("../../geno/filtered_T1_kinship.txt", skip=3, row.names=1)) # Read in kinship matrix
diag(T1.geno) <- diag(T1.geno) + 0.00001
T1.ginv <- solve(T1.geno)  # Get inverse of kinship matrix
rownames(T1.ginv) <- rownames(T1.geno)  # Add rownames to inverse matrix
T1.pheno <- pheno.avg %>% filter(Gid %in% rownames(T1.ginv))
T1.res <- foreach(i=1:length(traits), .combine=rbind) %dopar%
				  asremlWithinUni(pheno=T1.pheno, trait=traits[i], set="T1_uni", ginv=T1.ginv,
                                 weights=NULL, train.prop=0.8, nrep=50, seeds=seeds)
within.res <- rbind(C0.res, C2.res, T1.res)#
write.csv(within.res, "within_predictions.csv", quote=F, row.names=F)
