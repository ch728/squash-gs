library(DBI)
library(RSQLite)
library(tidyverse)
library(asreml)
library(doMC)
source("CV_functions.R")


# Get phenotypic data
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
traits <- colnames(pheno.avg)[3:14]# Start cross validation test sets

# Prog1 
seeds <- sample.int(10000000,50)
prog1.geno <- as.matrix(read.table("../../geno/C0_C2_kinship.txt", skip=3, row.names=1))  # Read in kinship matrix
diag(prog1.geno) <-diag(prog1.geno) + 0.00001
prog1.ginv <- solve(prog1.geno)  # Get inverse of kinship matrix
rownames(prog1.ginv) <- rownames(prog1.geno)  # Add rownames to inverse matrix
prog1.pheno <- pheno.avg %>% filter(Gid %in% rownames(prog1.ginv))
prog1.train <- prog1.pheno %>% filter(Pop=="C0")  # Subset training set
prog1.pred <-  prog1.pheno %>% filter(Pop=="C2")  # Subset prediction set

prog1 <- foreach(i=1:length(traits), .combine=rbind) %dopar%
                asremlTestUni(pheno.pred=prog1.pred, pheno.train=prog1.train, weights=NULL ,
                              trait= traits[i],set="Prog1", ginv=prog1.ginv,
                              mix.prop=0.0001, nrep=1, seeds=seeds)

# Test1
seeds <- sample.int(10000000,50)
test1.geno <- as.matrix(read.table("../../geno/C0_T1_kinship.txt", skip=3, row.names=1))  # Read in kinship matrix
diag(test1.geno) <- diag(test1.geno)+0.00001
test1.ginv <- solve(test1.geno)  # Get inverse of kinship matrix
rownames(test1.ginv) <- rownames(test1.geno)  # Add rownames to inverse matrix
test1.pheno <- pheno.avg %>% filter(Gid %in% rownames(test1.geno))
test1.train <- test1.pheno %>% filter(Pop == "C0")  # Subset training set
test1.pred <- test1.pheno %>% filter(Pop == "T1")  # Subset validation set
test1 <- foreach(i=1:length(traits), .combine=rbind) %dopar%
                 asremlTestUni(pheno.pred=test1.pred, pheno.train=test1.train, weights=NULL,
                           trait=traits[i], set="Test1", ginv=test1.ginv,
                           mix.prop=0.0001, nrep=1, seeds=seeds)

test.res <- rbind(prog1, test1)
write.csv(test.res,"test_predictions.csv", quote=F, row.names=F)

