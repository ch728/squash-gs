# This script tests out different weights (c values)
# Author: Chris Hernandez
library(DBI)
library(RSQLite)
library(tidyverse)
library(asreml)
library(doMC)
source("CV_functions.R")
# Set up parallel backend
registerDoMC(15)
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
                   avg(Width) AS Width, count(Brix) AS Count FROM trainingData GROUP BY Gid")
pheno.avg <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con) 
pheno.avg <- plyr::join(pheno.avg, YLD, by="Gid", type="inner")
# Get trait weights for three different values of c
t <-c(0.41, 0.25, 0.31, 0.47, 0.80, 0.36, 0.65, 0.90)  # Vector of trait repeatabilite
h <- c(0.21, 0.18, 0.23, 0.4, 0.56, 0.36, 0.41, 0.32 )  # Vector of trait heritabilites
traits <- c("Brix", "DM", "a", "b", "L",
            "Weight", "Length", "Width", 
            "TotalFrtCt", "TotalWtKg")  # Vector of trait names
            
pheno.wt1 <- data.frame(row.names=1:nrow(pheno.avg))  # Weights with c=0.1
for(i in 1:length(t)){
    w <- get_weights(pheno.avg, "Count", h[i], t[i], c=0.1)
    pheno.wt1 <- cbind(pheno.wt1, w)
}
pheno.wt1 <- cbind(pheno.wt1, rep(1, nrow(pheno.avg)), rep(1, nrow(pheno.avg)))
rownames(pheno.wt1) <- pheno.avg$Gid
colnames(pheno.wt1) <- traits
pheno.wt2 <- data.frame(row.names=1:nrow(pheno.avg))  # Weights with c=0.5
for(i in 1:length(t))
{
    w <- get_weights(pheno.avg, "Count", h[i], t[i], c=0.5)
    pheno.wt2 <- cbind(pheno.wt2, w)
}
rownames(pheno.wt2) <- pheno.avg$Gid
pheno.wt2 <- cbind(pheno.wt2, rep(1, nrow(pheno.avg)), rep(1, nrow(pheno.avg)))
colnames(pheno.wt2) <- traits
pheno.wt3 <- data.frame(row.names=1:nrow(pheno.avg))  # Weights with c=0.9
for(i in 1:length(t)){
    w <- get_weights(pheno.avg, "Count", h[i], t[i], c=0.9)
    pheno.wt3 <- cbind(pheno.wt3, w)
}
rownames(pheno.wt3) <- pheno.avg$Gid
pheno.wt3 <- cbind(pheno.wt3, rep(1, nrow(pheno.avg)), rep(1, nrow(pheno.avg)))
colnames(pheno.wt3) <-traits
# Test different weights with cross validation
test.res <- tibble()
seeds <- sample.int(10000000,50)
prog1.geno <- read.table("../../geno/C0_C2_kinship.txt", skip=3, row.names=1)  # Read in kinship matrix
prog1.ginv <- solve(prog1.geno)  # Get inverse of kinship matrix
rownames(prog1.ginv) <- rownames(prog1.geno)  # Add rownames to inverse matrix
prog1.pheno <- pheno.avg %>% filter(Gid %in% rownames(prog1.ginv))
prog1.train <- prog1.pheno %>% filter(Pop=="C0")  # Subset training set
prog1.pred <-  prog1.pheno %>% filter(Pop=="C2")  # Subset prediction set
res1 <- foreach(i = 1:length(traits), .combine=rbind) %dopar%
        asremlTestUni(pheno.pred=prog1.pred, pheno.train=prog1.train, weights=NULL,
                         trait=traits[i], set="Prog1_no", ginv=prog1.ginv, mix.prop=0.10,
                         nrep=50, seeds=seeds)
res2 <- foreach(i = 1:length(traits), .combine=rbind) %dopar%
        asremlTestUni(pheno.pred=prog1.pred, pheno.train=prog1.train, weights=pheno.wt1,
                         trait=traits[i], set="Prog1_0.1", ginv=prog1.ginv, mix.prop=0.10,
                         nrep=50, seeds=seeds)
res3 <- foreach(i = 1:length(traits), .combine=rbind) %dopar%
        asremlTestUni(pheno.pred=prog1.pred, pheno.train=prog1.train, weights=pheno.wt2,
                         trait=traits[i], set="Prog1_0.5", ginv=prog1.ginv, mix.prop=0.10,
                         nrep=50, seeds=seeds)
res4 <- foreach(i = 1:length(traits), .combine=rbind) %dopar%
        asremlTestUni(pheno.pred=prog1.pred, pheno.train=prog1.train, weights=pheno.wt3,
                         trait=traits[i], set="Prog1_0.9", ginv=prog1.ginv, mix.prop=0.10,
                         nrep=50, seeds=seeds)
test.res <- rbind(res1, res2, res3, res4)
write.csv(test.res, "test_weights_predictions.csv", quote=F, row.names=F)
