library(DBI)
library(RSQLite)
library(tidyverse)
library(asreml)
library(doMC)
library(stringr)

### Set up parallel backend ###
registerDoMC(20)

### Get phenotypic data ###
con = dbConnect(RSQLite::SQLite(), "../../../pheno/pheno.db")
pheno <- dbReadTable(con, "trainingData")  # Get trainingData table
pheno <- mutate(pheno, Shape=Length/Width)
res  <- dbSendQuery(con, "SELECT Gid, Pop, avg(Brix) AS Brix,
                          avg(DM) AS DM, avg(a) AS a,
                          avg(L) AS L,  avg(b) AS b,
                          avg(Length) AS Len, avg(Width) AS Width,
                          avg(Weight)  AS WT, avg(Length/Width) AS Shape
                          FROM trainingData GROUP BY Gid")  # Get means for traits in trainingData
pheno.avg <- dbFetch(res)
dbClearResult(res)
res <- dbSendQuery(con, "SELECT Gid, TotalFrtCt, TotalWtKg  FROM trainingYield")  # Get yield data
YLD <-dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
pheno.avg <- plyr::join(pheno.avg, YLD, by="Gid", type="inner")  # merge yield data with fruit average data
pheno.avg <- mutate(pheno.avg, Y3=TotalWtKg * (DM/100))

### Get genotypic data ###
geno <- as.matrix(read.table("../../../geno/C0_T1_C2_kinship.txt", row.names=1,  skip=3))
pheno <-pheno[pheno$Gid %in% rownames(geno),]  # Subset pheno to only include genotyped individuals
diag(geno) <- diag(geno) + 0.00001 # Add a small amount to diagnal to help converge
ginv <- solve(geno)
rownames(ginv) <- rownames(geno)

### Fit univariate repeatability  model for calculating  heritability and repeatability ###
traits <- colnames(pheno)[5:ncol(pheno)]
param1 <- foreach(i=1:length(traits), .combine=rbind ) %dopar% {
    t <- traits[i]
    fit <-asreml(fixed= eval(parse(text=t)) ~ Pop,
                 random= ~ide(Gid) + ped(Gid),
                 ginverse=list(Gid=ginv),
                 maxiter=500,
                 data=pheno)
    saveRDS(fit, file=paste(t,".rds", sep=""))  # Save model object
    vars <- summary(fit)$varcomp  # Get variance components
    h2 <- signif(vars[2,2]/sum(vars[,2]), 2)  # Calculate heritability
    r <- signif(sum(vars[1:2,2])/sum(vars[,2]), 2)  # Calculate repeatability
    return(data.frame(trait=traits[i],h2=h2,r=r))
 }
### Fit univariate model for estimating yield trait heritability ###
pheno.avg <- pheno.avg[pheno.avg$Gid %in% rownames(geno),]
traits <- colnames(pheno.avg)[12:14]
param2 <- foreach(i=1:length(traits), .combine=rbind ) %dopar% {
    t <- traits[i]
    fit <-asreml(fixed= eval(parse(text=t)) ~ Pop,
                 random= ~ped(Gid),
                 ginverse=list(Gid=ginv),
                 maxiter=250,
                 data=pheno.avg)
    saveRDS(fit, file=paste(t,".rds", sep=""))  # Save model object
    vars <- summary(fit)$varcomp  # Get variance components
    h2 <- signif(vars[1,2]/sum(vars[,2]), 2)  # Calculate heritability
    r <- NA  # Calculate repeatability
    return(data.frame(trait=traits[i],h2=h2,r=r))
 }
param <- rbind(param1, param2)
param$PA <- sqrt(param[,2])
write.csv(param, "parameters.csv", quote=F, row.names=F )  # Output

### Fit bivariate model for calculating pair-wise genetic correlations (full model) ###
pheno.avg <- pheno.avg[pheno.avg$Gid %in% rownames(geno),]
for(trait1 in 3:14){
    for(trait2 in 3:14){
        if(trait2 > trait1){
            t2 <- colnames(pheno.avg)[trait2]
            t1 <- colnames(pheno.avg)[trait1]
            m= paste("cbind(", t1, ",", t2, ")", "~trait + trait:Pop",sep="")
            fit <- asreml(eval(parse(text=m)),
                          random =~us(trait):ped(Gid),
                          ginverse=list(Gid=ginv),
                          rcov =~units:us(trait),
                          maxiter=250,
                          data=pheno.avg)
            saveRDS(fit, file = paste("bi_", t1, "_", t2, ".rds",sep=""))
         }
    }
}

### Fit bivariate model without genetic covariance (reduced model) ###
pheno.avg <- pheno.avg[pheno.avg$Gid %in% rownames(geno),]
for(trait1 in 3:14){
    for(trait2 in 3:14){
        if(trait2 > trait1){
            t2 <- colnames(pheno.avg)[trait2]
            t1 <- colnames(pheno.avg)[trait1]
            m= paste("cbind(", t1, ",", t2, ")", "~trait + trait:Pop",sep="")
            fit <- asreml(eval(parse(text=m)),
                          random =~diag(trait):ped(Gid),  # Specify diagonal instead of unstructured
                          ginverse=list(Gid=ginv),
                          rcov =~units:us(trait),
                          maxiter=250,
                          data=pheno.avg)
            saveRDS(fit, file = paste("biNull_", t1, "_", t2, ".rds",sep=""))
         }
    }
}

### Read in model objects and check model conversion ###
in_files <- list.files(".", "bi_")
converge <- c()
for(i in 1:length(in_files)){
    tmp <- readRDS(in_files[i])
    converge <- c(converge, tmp$converge)
}

in_filesNull <- list.files(".", "biNull_")
Nullconverge <- c()
for(i in 1:length(in_filesNull)){
    tmp <- readRDS(in_filesNull[i])
    Nullconverge <- c(Nullconverge, tmp$converge)
}

### Read in model objects and calculate genetic correlations ###
cor.g <- foreach(i=1:length(in_files), .combine=rbind) %dopar% {
   m <- in_files[i]
   vars<- summary(readRDS(m))$varcomp  #  Extract variance components from full model
   loglikfull <- summary(readRDS(m))$loglik  # Extract logliklihood from full model
   n <- gsub("\\.rds", "", m)
   r_g <- signif(vars[2,2]/(sqrt(vars[1,2]) * sqrt(vars[3,2])), 2)  # Calculate genetic correlation
   null <- paste("biNull_", str_split(m, "bi_", simplify=T)[,2], sep="")
   logliknull <- summary(readRDS(null))$loglik
   LRT = -2 * (logliknull - loglikfull)  # Calculate loglikelihood test statistic
   p = 1-pchisq(LRT, df=1)  # Get p-value
   return(data.frame(n, r_g, p))
}
p.adj <- p.adjust(cor.g$p, method="bonferroni")  # Multiple test correction
cor.g <- cbind(cor.g, p.adj=p.adj)
cor.g <- cbind(cor.g , data.frame(Trait1=str_split(cor.g[,1], "_", simplify=T)[,2],
                                  Trait2=str_split(cor.g[,1], "_", simplify=T)[,3]))
write.csv(cor.g, "genetic_correlations.csv",  row.names=F, quote=F)  # Output genetic correlations

### Calculate index heritabilty and export G (genetic covariance matrix) ###
bx_dm <- signif(summary(readRDS("bi_Brix_DM.rds"))$varcomp[2,2], 3)
bx_a <- signif(summary(readRDS("bi_Brix_a.rds"))$varcomp[2,2], 3)
dm_a <- signif(summary(readRDS("bi_DM_a.rds"))$varcomp[2,2], 3)
bx_bx <- signif(summary(readRDS("bi_Brix_DM.rds"))$varcomp[1,2], 3)
dm_dm <- signif(summary(readRDS("bi_Brix_DM.rds"))$varcomp[3,2], 3)
a_a <- signif(summary(readRDS("bi_Brix_a.rds"))$varcomp[3,2], 3)
G <- matrix(rep(1,9), ncol=3)
G[1,2] <- bx_a
G[1,3] <- dm_a
G[2,3] <- bx_dm
G[2,1] <- bx_a
G[3,1] <- dm_a
G[3,2] <- bx_dm
G[1,1] <- a_a
G[2,2] <- bx_bx
G[3,3] <- dm_dm
colnames(G) <- c("a", "Brix", "DM")
rownames(G) <- c("a", "Brix", "DM")
write.csv(G, "G.csv", quote=F)
P <- as.matrix(read.csv("../phenotypic/P.csv", header=T, row.names=1))
b <- solve(P) %*% G %*% c(1,1,1)  # Calculate Smith-Hazel index weights
colnames(b) <- "weight"
h_i <- (t(b) %*% G %*% b) / (t(b) %*% P %*% b)  # Calculate index heritability
write.csv(b, "index_weights.csv", quote=F)  # Write out index weights 