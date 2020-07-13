library(asreml)
library(sommer)
library(reshape2)
source("~/scripts/atcg1234.R")

#read in  GS3.1 pheno
pheno<-read.csv("../../../../GS3.1/phenotypes/ALL/GS3.1_phenotypes.csv")
str(pheno)

#read in GS.3.1 + GS4 genotype table
geno<-read.table("../../../RE_TASSEL/Genotype_tables/GS4_GS3.1_RD2_MAF0.05_0.8_0.6.txt",colClasses="character",header=T,row.names=1)
geno2<-t(apply(geno,1,function(x) gsub("0","N",x)))
geno2<-t(apply(geno2,1,function(x) gsub("-","N",x)))
str(geno)
table(c(as.matrix(geno)))

#get dosage
dosage<-atcg1234_2(geno,maf=0.05,imp=F)
dosage2<-atcg1234_2(geno2,maf=0.05,imp=F)
#get Amat
Amat1<-A.mat(dosage,impute.method="EM")
  
#get inverse
A<-Amat1
diag(A)<-diag(A)+0.00000001
Ginv<-solve(A)

#subset pheno so that pheno contains only individuals in Amat1
phenoSubset<-pheno[pheno$gid %in% rownames(Amat1),]
phenoSubset$gid<-factor(phenoSubset$gid)

#fit models
asremlFit<-asreml(scale(cbind(Brix,Dry_matter,a))~trait
                    ,random=~us(trait):ped(gid)
                    ,rcov= ~ units:us(trait)
                    ,data=phenoSubset
                    ,ginverse=list(gid=as.matrix(Ginv))
                    ,maxiter=250, control=list(workspace=1.6e+9))

asremlFit2<-asreml(scale(cbind(Brix,Dry_matter,a))~trait+Date_Processed:trait
                  ,random=~us(trait):ped(gid)
                  ,rcov= ~ units:us(trait)
                  ,data=phenoSubset
                  ,ginverse=list(gid=as.matrix(Ginv))
                  ,maxiter=250, control=list(workspace=1.6e+9))

#extract predictions
asremlPred<-as.data.frame(asremlFit$coefficients$random)
colnames(asremlPred)[1]<-"GEBV"
asremlPred$gid<-gsub("trait_.*ped\\(gid\\)_","",rownames(asremlPred))
asremlPred$trait<-gsub("trait_(.*):ped\\(gid\\)_.*","\\1",rownames(asremlPred))
asremlPred<-melt(asremlPred)
asremlPred<-acast(asremlPred,gid~trait)
asremlIndx<-as.data.frame(rowSums(asremlPred))
asremlIndx<-asremlIndx[grep("GS4_",rownames(asremlIndx)),,drop=F]
asremlIndxSorted<-asremlIndx[order(-asremlIndx$`rowSums(asremlPred)`),,drop=F]


asremlPred2<-as.data.frame(asremlFit2$coefficients$random)
colnames(asremlPred2)[1]<-"GEBV"
asremlPred2$gid<-gsub("trait_.*ped\\(gid\\)_","",rownames(asremlPred2))
asremlPred2$trait<-gsub("trait_(.*):ped\\(gid\\)_.*","\\1",rownames(asremlPred2))
asremlPred2<-melt(asremlPred2)
asremlPred2<-acast(asremlPred2,gid~trait)
asremlIndx2<-as.data.frame(rowSums(asremlPred2))
asremlIndx2<-asremlIndx2[grep("GS4_",rownames(asremlIndx2)),,drop=F]
asremlIndxSorted2<-asremlIndx2[order(-asremlIndx2$`rowSums(asremlPred2)`),,drop=F]

#compare both model top selections
sum(rownames(asremlIndxSorted2)[1:20]%in%rownames(asremlIndxSorted)[1:20])

#compare predicted with actual phenotypes for GS3.1


#output selections
write.csv(asremlIndxSorted[1:30,,F],"./GS4_selections_without_day_effect.csv")
write.csv(asremlIndxSorted2[1:30,,F],"./GS4_selections_with_day_effect.csv")


}
