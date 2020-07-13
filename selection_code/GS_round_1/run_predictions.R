library(asreml)
library(rrBLUP)
source("EMMREML_validate.R")
source("ecmFirstStep.R")

###read in genotypes
K.mat<-read.table("GS2.1_VCF_tools_kinship.txt")
K.mat2<-read.table("GS2.1_KNNimp_kinship.txt",row.names=1)
rownames(K.mat2)<-gsub(":[0-9]*","",rownames(K.mat2))
colnames(K.mat2)<-rownames(K.mat2)
hist(as.vector(as.matrix(K.mat)-as.matrix(K.mat2)))
###read in phenotypes
pheno<-read.csv("Final_training_only_fruit_data_with_time_duplicates_removed.csv")
cor(pheno[,3:5])

###subset pheno to only include individuals in kinship matrix
pheno2<-pheno[pheno$gid %in%colnames(K.mat)[grep("GS1",colnames(K.mat))],]
pheno2$gid<-droplevels(pheno2$gid)
cor(pheno2[,3:5])

###get BLUPS and do cross validation to check genotype order
X<-t(model.matrix(~1,data=pheno2))
Z<-t(model.matrix(~gid-1,data=pheno2))
rownames(Z)<-gsub("gid","",rownames(Z))
K<-diag(nrow(Z))
rownames(K)<-rownames(Z)
colnames(K)<-rownames(K)
Y<-t(scale(pheno2[,3:5]))
BLUPS<-t(ecmFirstStep(Y,X,Z,K)$Gpred)
colnames(BLUPS)<-c("Brix","Dry_matter","a")
BLUPS<-as.data.frame(BLUPS)
BLUPS$gid<-rownames(BLUPS)
K.train1<-K.mat[row.names(BLUPS),row.names(BLUPS)]
k.train2<-K.mat2[row.names(BLUPS),row.names(BLUPS)]

ans<-EMMREML_cross_validate(data=BLUPS,pheno=c("Brix","Dry_matter","a"),Amat=as.matrix(K.train1),nfold=5,nrep=25)
ans2<-EMMREML_cross_validate(datame=BLUPS,pheno=c("Brix","Dry_matter","a"),Amat=as.matrix(k.train2),nfold=5,nrep=10)
#Generate boxplot of accuracies
results<-matrix(nrow=nrow(ans)/5,ncol=3)
x=1
for(i in seq(1,nrow(ans),5)){
  results[x,]<-colMeans(ans[seq(i,i+4,1),])
  x=x+1
}
boxplot(results,ylim=c(0,0.7),names=c("Brix","Dry Matter","a*"),main="5-fold CV",lwd=2,cex.axis=2.5,cex.lab=2.5,cex.main=2.5,font=2,font.axis=2)
  
####get predictions with ASREML with K.mat
K<-K.mat+diag(x=0.000000001,nrow=353,ncol=353)
Ginv<-solve(K)
pheno2$gid<-as.character(pheno2$gid)
pheno2$gid<-factor(pheno2$gid,levels=rownames(Ginv))
fit<-asreml(scale(cbind(Brix,Dry_matter,a))~trait #1
             
             ,random=~us(trait):ped(gid) #2
             
             ,rcov= ~ units:us(trait) #3
             
             ,data=pheno2 #4
             
             ,ginverse=list(gid=as.matrix(Ginv)))#5

pred<-fit$coefficients$random
brix<-data.frame(pred[grep("trait_Brix",names(pred))])
rownames(brix)<-gsub("trait_Brix:ped\\(gid\\)_","",rownames(brix))

dry<-data.frame(pred[grep("Dry_matter",names(pred))])
rownames(dry)<-gsub("trait_Dry_matter:ped\\(gid\\)_","",rownames(dry))

a<-data.frame(pred[grep("trait_a",names(pred))])
rownames(a)<-gsub("trait_a:ped\\(gid\\)_","",rownames(a))

pred<-as.matrix(cbind(brix,dry[rownames(brix),],a[rownames(brix),]))
colnames(pred)<-c("Brix","Dry_matter","a")
cor(pred)
indx<-as.data.frame(.33*rowSums(pred[grep("GS2.1",rownames(pred)),]))
#indx<-as.data.frame(.33*rowSums(pred))
write.csv(indx,"selections.csv")
indx$gid<-rownames(indx)
indx<-indx[order(-indx[,1]),]
head(indx,n=24)
write.csv(indx[1:24,],"selections.csv")
hist(indx)
####get predictions with ASREML with K.mat2  
Ginv<-solve(K.mat2)
fit2<-asreml(scale(cbind(Brix,Dry_matter,a))~trait #1
            
            ,random=~us(trait):ped(gid) #2
            
            ,rcov= ~ units:us(trait) #3
            
            ,data=pheno2 #4

            ,ginverse=list(gid=as.matrix(Ginv)))#5

pred2<-fit2$coefficients$random

brix<-data.frame(pred2[grep("trait_Brix",names(pred2))])
rownames(brix)<-gsub("trait_Brix:ped\\(gid\\)_","",rownames(brix))

dry<-data.frame(pred2[grep("Dry_matter",names(pred2))])
rownames(dry)<-gsub("trait_Dry_matter:ped\\(gid\\)_","",rownames(dry))

a<-data.frame(pred2[grep("trait_a",names(pred2))])
rownames(a)<-gsub("trait_a:ped\\(gid\\)_","",rownames(a))

pred2<-as.matrix(cbind(brix,dry,a))
colnames(pred2)<-c("Brix","Dry_matter","a")
indx2<-as.data.frame(rowSums(pred2[grep("GS2.1",rownames(pred2)),]))
indx2$gid<-rownames(indx2)
indx2<-indx2[order(-indx2[,1]),]

###get predictions with EMMREML K.mat

K=as.matrix(K.mat)

Y=t(scale(pheno2[,c("Brix","Dry_matter","a")]))
X=t(model.matrix(~1,data=pheno2))
pheno2$gid=as.character(pheno2$gid)
pheno2$gid=factor(pheno2$gid,levels=colnames(K))

Z = t(model.matrix(~gid-1, data=pheno2))
colnames(Z)=gsub("gid","",colnames(Z))
rownames(Z)=gsub("gid","",rownames(Z))

fit3<-ecmFirstStep(Y,X,Z,K)

pred3<-t(fit3$Gpred[,grep("GS2.1",colnames(fit3$Gpred))])
indx3<-as.data.frame(rowSums(pred3))
indx3$gid<-rownames(indx3)
indx3<-indx3[order(-indx3[,1]),]
head(indx3,n=24)


###get predictions with EMMREML K.mat2 
X=t(model.matrix(~1,data=pheno2))

K=as.matrix(K.mat2)

Y=t(scale(pheno2[,c("Brix","Dry_matter","a")]))

pheno2$gid=as.character(pheno2$gid)
pheno2$gid=factor(pheno2$gid,levels=colnames(K))

Z = t(model.matrix(~gid-1, data=pheno2))
colnames(Z)=gsub("gid","",colnames(Z))
rownames(Z)=gsub("gid","",rownames(Z))

fit4<-ecmFirstStep(Y,X,Z,K)

pred4<-t(fit4$Gpred[,grep("GS2.1",colnames(fit4$Gpred))])
indx4<-as.data.frame(rowSums(pred4))
indx4$gid<-rownames(indx4)
indx4<-indx4[order(-indx4[,1]),]
head(indx4,n=24)

###check to see how different top selections are
sum(rownames(indx[1:20,])%in%rownames(indx2[1:20,]))
sum(rownames(indx[1:24,])%in%rownames(indx3[1:24,]))
sum(rownames(indx3[1:20,])%in%rownames(indx2[1:20,]))
sum(rownames(indx4[1:24,])%in%rownames(indx[1:24,]))
###cross check GEBVs with univariate rrBLUP
brix<-kin.blup(data=pheno2,geno="gid",pheno="Brix",K=as.matrix(K.mat))$pred
cor(pred[,1],brix[rownames(pred)])

dry<-kin.blup(data=pheno2,geno="gid",pheno="Dry_matter",K=as.matrix(K.mat))$pred
cor(pred[,2],dry[rownames(pred)])

a<-kin.blup(data=pheno2,geno="gid",pheno="a",K=as.matrix(K.mat))$pred
cor(pred[,3],a[rownames(pred)])

###check predictions with BLUPS
common<-rownames(BLUPS)%in%rownames(pred)
temp<-BLUPS[common,]
temp2<-pred[rownames(BLUPS),]

cor(temp[,1],temp2[,1])
cor(temp[,2],temp2[,2])
cor(temp[,3],temp2[,3])






