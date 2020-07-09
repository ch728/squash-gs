library(DBI)
library(RSQLite)
library(tidyverse)
con <- dbConnect(RSQLite::SQLite(),"../../../pheno/pheno.db") 
# Get phenotypic data 
res  <- dbSendQuery(con, "SELECT Gid, Pop, avg(Brix) AS Brix, 
                          avg(DM) AS DM, avg(a) AS a,
                          avg(L) AS L,  avg(b) AS b,
                          avg(Length) AS Length, avg(Width) AS Width,
                          avg(Weight)  AS Weight, avg(Length/Width) AS Shape
                          FROM trainingData GROUP BY Gid")  # Get means for traits in trainingData
pheno.avg <- dbFetch(res)
dbClearResult(res)
res <- dbSendQuery(con, "SELECT Gid, TotalFrtCt, TotalWtKg  FROM trainingYield")  # Get yield data
YLD <-dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
pheno.avg <- plyr::join(pheno.avg, YLD, by="Gid", type="inner")  # merge yield data with fruit average data
pheno.avg <- mutate(pheno.avg, Y3=TotalWtKg * (DM/100)) 
# Get all pairwise correlations
res <- data.frame()
traits <- colnames(pheno.avg)[3:14]
for(i in 1:length(traits)){
	for(j in 1:length(traits)){
		if(i > j){
			pop <- pheno.avg[,2]
			t1 <- pheno.avg[ ,i + 2]
			t1 <- resid(lm(t1 ~ pop))  # remove pop effect
			t2 <- pheno.avg[ ,j + 2]
			t2 <- resid(lm(t2 ~ pop))  # remove pop effect
			r_p <-signif(cor(t1, t2), 2)
			p <- summary(lm(t1 ~ t2))$coefficients[2,4]
			res <- rbind(res, data.frame(r_p, p, Trait1=traits[i],
			                             Trait2=traits[j]))
		}
	}
}

res$p.adj <- p.adjust(res$p, method="bonferroni")
write.csv(res, "phenotypic_correlations.csv", quote=F, row.names=F)
# Calculate P
a <- resid(lm(a ~ Pop, pheno.avg))
bx <- resid(lm(Brix ~ Pop, pheno.avg))
dm <- resid(lm(DM ~ Pop, pheno.avg))
P <- var(data.frame(a=a, Brix=bx, DM=dm))
write.csv(P, "P.csv", quote=F)
