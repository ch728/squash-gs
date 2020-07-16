library(asreml)

### Read in genotype table ###
geno<-read.table("dosage.txt",row.names=1)

### Get additive relationship matrix using rrBLUP A.mat() using the EM method for imputing missing values  ###
Amat<-A.mat(geno,impute.method="EM")

### Read in the phenotype data ###
pheno<-read.csv("pheno.csv")

### Subset phenotype file ###
common<-pheno$gid %in% rownames(Amat)[grep("GS1_*",rownames(Amat))]
pheno<-pheno[common,]
pheno$gid<-factor(pheno$gid)

### Get predictions using ASReml ###
Ginv<-solve((Amat+diag(x= 0.000000001,nrow(Amat),ncol(Amat))))
asreml_out<-asreml(scale(cbind(Brix,Dry_matter,a))~trait 
       ,random=~us(trait):ped(gid) 
       ,rcov= ~ units:us(trait) 
       ,data=pheno
       ,ginverse=list(gid=as.matrix(Ginv))
       ,maxiter=250, control=list(workspace=1.6e+9))

### Extract and format ASReml output ###
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

### Calculate index and output selections ###
GS3.1_asreml_index<-as.data.frame(rowSums(GS3.1_asreml_predictions[,2:4]))
GS3.1_asreml_index$gid=rownames(GS3.1_asreml_index)
GS3.1_asreml_index_sorted<-GS3.1_asreml_index[order(-GS3.1_asreml_index$`rowSums(GS3.1_asreml_predictions[, 2:4])`),]
write.table(GS3.1_asreml_index_sorted[1:20,],file="selection.txt",sep="\t",row.names=F)
