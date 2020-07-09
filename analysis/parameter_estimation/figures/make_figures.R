library(xtable)
library(tidyverse)
library(hash)

params <- read.csv("../genotypic/parameters.csv", stringsAsFactors=F)
g.cor <- read.csv("../genotypic/genetic_correlations.csv", stringsAsFactors=F)
p.cor <- read.csv("../phenotypic/phenotypic_correlations.csv", stringsAsFactors=F)
h <- hash(keys=c("a", "b", "L", "Brix", "DM",
                 "TotalFrtCt", "TotalWtKg", "Y3",
                 "Length", "Width", "Weight", "Shape",
                 "C0_uni", "C2_uni", "T1_uni", "Prog1", "Test1", "WT", "Len"),
          values=c("a*", "b*", "L*","°Bx", "%DM",
                    "FrtCt", "TotalWt", "TotalDM",
                    "Len", "Wd", "Wt", "Shp",
                    "C0", "C2", "T1", "Prog", "Test", "Wt", "Len"))
params$trait <- sapply(params$trait, function(x) h[[x]], USE.NAMES=F)
print(xtable(params), type= "latex", include.rownames=F, file="genetic_params.tex")


g.cor$Trait1 <- sapply(g.cor$Trait1, function(x) h[[x]], USE.NAMES=F)
g.cor$Trait2 <- sapply(g.cor$Trait2, function(x) h[[x]], USE.NAMES=F)
p.cor$Trait1 <- sapply(p.cor$Trait1, function(x) h[[x]], USE.NAMES=F)
p.cor$Trait2 <- sapply(p.cor$Trait2, function(x) h[[x]], USE.NAMES=F)
traits <- c("a*", "b*", "L*","°Bx", "%DM",
            "Len", "Wd", "Wt", "Shp",
            "FrtCt", "TotalWt", "TotalDM")
m  <- as.data.frame(matrix(rep(NA, 12*12), ncol=12))
colnames(m) <- traits
rownames(m) <- traits
i <- 1
j <- 1
for(t in 1:nrow(m)){  # add 1 to diagonal
	m[i,j] <- "1"
	i <- i + 1
	j <- j + 1
}

for(i in 1:nrow(g.cor)){  # add genetic correlations above diagonal with sig. indication
	if(g.cor$p.adj[i] < 0.05){
		val <- paste(g.cor$r_g[i], "**", sep="")
	}else if(g.cor$p[i] < 0.05){
		val <- paste(g.cor$r_g[i], "*", sep="")
	} else{
		val <- paste(g.cor$r_g[i], sep="")
	}	
	r <- g.cor$Trait1[i]
	c <- g.cor$Trait2[i]
	if(which(colnames(m) ==c) > which(rownames(m) ==r)){
		m[r,c] <- val
	} else{
		m[c,r] <- val
	}
}

for(i in 1:nrow(g.cor)){  # add phenotypic  correlations below diagonal with sig. indication
	if(p.cor$p.adj[i] < 0.05){
		val <- paste(p.cor$r_p[i], "**", sep="")
	}else if(p.cor$p[i] < 0.05){
		val <- paste(p.cor$r_p[i], "*", sep="")
	} else{
		val <- paste(p.cor$r_p[i], sep="")
	}	
	r <- p.cor$Trait1[i]
	c <- p.cor$Trait2[i]
	if(which(colnames(m) ==c) <  which(rownames(m) ==r)){
		m[r,c] <- val
	} else{
		m[c,r] <- val
	}
}

print(xtable(m), type="latex", include.rownames=T, file="corr_table.tex")
