library(asreml)

### Read in genotypes ###
K.mat<-read.table("kinship.txt")

### Read in phenotypes ###
pheno<-read.csv("pheno.csv")
cor(pheno[,3:5])

### Subset pheno to only include individuals in kinship matrix ###
pheno2<-pheno[pheno$gid %in%colnames(K.mat)[grep("GS1",colnames(K.mat))],]
pheno2$gid<-droplevels(pheno2$gid)

### Get predictions with ASREML ###
K<-K.mat+diag(x=0.000000001,nrow=353,ncol=353)  # Bend matrix slightly
Ginv<-solve(K)
pheno2$gid<-as.character(pheno2$gid)
pheno2$gid<-factor(pheno2$gid,levels=rownames(Ginv))
fit<-asreml(scale(cbind(Brix,Dry_matter,a))~trait
             
             ,random=~us(trait):ped(gid) 
             
             ,rcov= ~ units:us(trait) 
             
             ,data=pheno2 
             
             ,ginverse=list(gid=as.matrix(Ginv)))

### Extract and organize preditions for each trait ###
pred<-fit$coefficients$random
brix<-data.frame(pred[grep("trait_Brix",names(pred))])
rownames(brix)<-gsub("trait_Brix:ped\\(gid\\)_","",rownames(brix))

dry<-data.frame(pred[grep("Dry_matter",names(pred))])
rownames(dry)<-gsub("trait_Dry_matter:ped\\(gid\\)_","",rownames(dry))

a<-data.frame(pred[grep("trait_a",names(pred))])
rownames(a)<-gsub("trait_a:ped\\(gid\\)_","",rownames(a))

pred<-as.matrix(cbind(brix,dry[rownames(brix),],a[rownames(brix),]))
colnames(pred)<-c("Brix","Dry_matter","a")
indx<-as.data.frame(.33*rowSums(pred[grep("GS2.1", rownames(pred)),])) # Get index value for selection
indx$gid<-rownames(indx)
indx<-indx[order(-indx[,1]),]
head(indx,n=24)
write.csv(indx[1:24,],"selections.csv")  # Write out selections
