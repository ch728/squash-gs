library(rrBLUP)
library(EMMREML)
library(asreml)

source("~/scripts/EMMREML_validate.R")
source("~/scripts/ecmFirstStep.R")
source("~/scripts/rrBLUP_validate.R")

#read in genotype table
geno<-read.table("dosage.txt",row.names=1)
str(geno)
#get additive relationship matrix using rrBLUP A.mat() using the EM method for imputing missing values
Amat<-A.mat(geno,impute.method="EM")

#read in the phenotype data
pheno<-read.csv("Final_training_only_fruit_data_with_time_duplicates_removed.csv")
str(pheno)

#check phenotype data
cor(pheno[,c("Brix","Dry_matter","a")])
length(unique(pheno$gid))

#get BLUPS using EMMREML for CV 
Z=as.matrix(t(model.matrix(~gid-1,data=pheno)))
rownames(Z)<-gsub("gid","",row.names(Z))

X<-as.matrix(t(model.matrix(~1,data=pheno)))

K=as.matrix(t(diag(186)))

rownames(K)=rownames(Z)
colnames(K)=rownames(K)

Y<-as.matrix(t(scale(pheno[,3:5])))
colnames(Y)<-pheno$gid

BLUPS<-ecmFirstStep(Y,X,Z,K)$Gpred
rownames(BLUPS)<-c("Brix","Dry_matter","a")
BLUPS<-as.data.frame(t(BLUPS))
#check correlation of BLUPS (estimate of genetic correlation)
cor(BLUPS)

#run CV to to check GS1 data

GS1_mat<-Amat[grep("GS1_*",rownames(Amat)),grep("GS1_*",rownames(Amat))]

common<-intersect(rownames(BLUPS), rownames(GS1_mat))

BLUPS<-BLUPS[common,]
cor(BLUPS)

BLUPS$gid=rownames(BLUPS)
colnames(BLUPS)<-c("Brix","Dry_matter","a","gid")

ans<-EMMREML_cross_validate(BLUPS,pheno=c("Brix","Dry_matter","a"),Amat=GS1_mat,nfold=5,nrep=10)
colMeans(ans)

#subset phenotype file
common<-pheno$gid %in% rownames(Amat)[grep("GS1_*",rownames(Amat))]
pheno<-pheno[common,]
pheno$gid<-factor(pheno$gid)

#get predictions using ASREML
Ginv<-solve((Amat+diag(x= 0.000000001,nrow(Amat),ncol(Amat))))
asreml_out<-asreml(scale(cbind(Brix,Dry_matter,a))~trait #1
       ,random=~us(trait):ped(gid) 
       ,rcov= ~ units:us(trait) 
       ,data=pheno
       ,ginverse=list(gid=as.matrix(Ginv))
       ,maxiter=250, control=list(workspace=1.6e+9))

a<-asreml_out$coefficients$random[grep("trait_a:ped(gid)*",names(asreml_out$coefficients$random))]
names(a)<-gsub("trait_a:ped\\(gid\\)_","",names(a))
a<-as.data.frame(a)
a$gid<-rownames(a)

brix<-asreml_out$coefficients$random[grep("trait_Brix:ped(gid)*",names(asreml_out$coefficients$random))]
names(brix)<-gsub("trait_Brix:ped\\(gid\\)_","",names(brix))  
brix<-as.data.frame(brix)
brix$gid<-rownames(brix)

dry<-asreml_out$coefficients$random[grep("trait_Dry_matter:ped(gid)*",names(asreml_out$coefficients$random))]
names(dry)<-gsub("trait_Dry_matter:ped\\(gid\\)_","",names(dry))  
dry<-as.data.frame(dry)
dry$gid<-rownames(dry)


asreml_predictions<-merge(brix,dry,by="gid")
asreml_predictions<-merge(asreml_predictions,a,by="gid")
rownames(asreml_predictions)<-asreml_predictions$gid

GS3.1_asreml_predictions<-asreml_predictions[grep("GS3.1*",rownames(asreml_predictions)),]

GS3.1_asreml_index<-as.data.frame(rowSums(GS3.1_asreml_predictions[,2:4]))
GS3.1_asreml_index$gid=rownames(GS3.1_asreml_index)

GS3.1_asreml_index_sorted<-GS3.1_asreml_index[order(-GS3.1_asreml_index$`rowSums(GS3.1_asreml_predictions[, 2:4])`),]


write.table(GS3.1_asreml_index_sorted[1:20,],file="GS3.1_top_20_selection.txt",sep="\t",row.names=F)

#get predictions using EMMREML

K=as.matrix(Amat)

dim(K)

pheno$gid<-factor(pheno$gid,levels=colnames(K))

Z=as.matrix(t(model.matrix(~gid-1,data=pheno)))
rownames(Z)<-gsub("gid","",rownames(Z))

dim(Z)

X<-t(model.matrix(~1,pheno))

dim(X)

Y<-t(pheno[,c("Brix","Dry_matter","a")])

dim(Y)

EMMREML_Predictions<-emmremlMultivariate(Y,X,Z,K)$Gpred

EMMREML_Predictions<-t(EMMREML_Predictions)

colnames(EMMREML_Predictions)<-c("Brix","Dry_matter","a")


cor(EMMREML_Predictions)

GS3.1_EMMREML_Predictions<-EMMREML_Predictions[grep("GS3.1*",rownames(EMMREML_Predictions)),]

cor(GS3.1_EMMREML_Predictions)

GS3.1_EMMREML_index<-as.data.frame(rowSums(GS3.1_EMMREML_Predictions))
GS3.1_EMMREML_index$gid<-rownames(GS3.1_EMMREML_index)

GS3.1_EMMREML_index_sorted<-GS3.1_EMMREML_index[order(-GS3.1_EMMREML_index$`rowSums(GS3.1_EMMREML_Predictions)`),]


#compare predictions from asreml with EMMREML
sum( GS3.1_EMMREML_index_sorted$gid[1:24]%in% GS3.1_asreml_index_sorted$gid[1:24])

#cross-check results from ASREML and EMMREML with rrBLUP by comparing GS3.1 predictions 
rrBLUP_Brix<-as.data.frame(kin.blup(data=pheno,geno="gid",pheno="Brix",K=Amat)$g)

rrBLUP_Dry_matter<-as.data.frame(kin.blup(data=pheno,geno="gid",pheno="Dry_matter",K=Amat)$g)

rrBLUP_a<-as.data.frame(kin.blup(data=pheno,geno="gid",pheno="a",K=Amat)$g)

cor(rrBLUP_Brix[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"brix"])
plot(rrBLUP_Brix[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"brix"])

cor(rrBLUP_Dry_matter[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"dry"])
plot(rrBLUP_Dry_matter[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"dry"])

cor(rrBLUP_a[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"a"])
plot(rrBLUP_a[rownames(asreml_predictions),1], asreml_predictions[rownames(asreml_predictions),"a"])

#cross-check results from ASREML with BLUPS phenotypes alone

GS1_asreml_brix<-asreml_predictions[grep("GS1_*",rownames(asreml_predictions)),c("brix","gid")]
cor(rrBLUP_Brix[rownames(GS1_asreml_brix),],GS1_asreml_brix[,1])

GS1_asreml_dry<-asreml_predictions[grep("GS1_*",rownames(asreml_predictions)),c("dry","gid")]
cor(rrBLUP_Dry_matter[rownames(GS1_asreml_dry),],GS1_asreml_dry[,1])

GS1_asreml_a<-asreml_predictions[grep("GS1_*",rownames(asreml_predictions)),c("a","gid")]
cor(rrBLUP_a[rownames(GS1_asreml_a),],GS1_asreml_a[,1])


