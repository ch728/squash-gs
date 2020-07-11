# These functions are used for cross validation procedures
# Dependencies: ASReml-R, plyr, and tidyverse
# Author Chris Hernandez

get_weights <- function(pheno.avg, count, h, t, c=0.1){
	# Calculates weights to use for mean of repeated measures
	n <- pheno.avg[,count]
	h2 <- rep(h, length(n))
	t <- rep(t, length(n))
	c <- rep(c, length(n))
	w <- (1-h2)/(c*h2+(1+(n-1)*t)/n-h2)
	return(w)	
}

asremlTestUni <- function(pheno.pred, pheno.train, weights=NULL,  trait, set,  ginv, mix.prop, nrep, seeds){
    # Function for univariate model validation across test sets
    n.val <- nrow(pheno.pred)  
    n.sample <- ceiling(nrow(pheno.pred) * mix.prop)
    results <- data.frame()
    pheno$Gid <- as.character(pheno$Gid)  # Make sure this is a character for matching
    for(r in 1:nrep){
        set.seed(seeds[r])
        sub <- sample.int(n.val, n.sample)
        pheno.composite <- rbind(pheno.train, pheno.pred[sub,])
        Gid.pred <- as.character(pheno.pred[-sub,"Gid"])
        if(is.null(weights)){
            weights_sub <- rep(1,nrow(pheno.composite))
            pheno.composite <- cbind(pheno.composite, weights_sub)
        } else{
            weights_sub <- weights[pheno.composite$Gid,trait]
            pheno.composite <-cbind(pheno.composite, weights_sub)
        }
        assign("trait", trait, envir=.GlobalEnv)
        assign("pheno.composite", pheno.composite, envir=.GlobalEnv)
        assign("ginv", ginv, envir=.GlobalEnv)
        fit <- asreml(fixed=eval(parse(text=trait))~1, random=~ped(Gid), ginverse=list(Gid=ginv),
                      weights="weights_sub", data=pheno.composite, maxiter=250, trace=F)
        rm(list=c("trait", "pheno.composite", "ginv"), envir=.GlobalEnv) 
        pred <- coef(fit)$random
        rownames(pred) <- gsub("ped\\(Gid\\)_", "", rownames(pred))
        pred <- pred[Gid.pred,]
        val <- pheno.pred[-sub, trait]
        results <- rbind(results, data.frame(Gid=Gid.pred, 
                        Rep=rep(r,length(Gid.pred)),
                        Set=rep(set,length(Gid.pred)),
            		    Trait=rep(trait, length(Gid.pred)),
                        Pred=pred, Val=val)) 
    }
    return(results)
}

asremlWithinUni <- function(pheno, weights=NULL,  trait, set,  ginv, train.prop, nrep, seeds){
        # Univariate cross validation within set 
        n <- nrow(pheno) 
        n.sample <- ceiling(n * train.prop)
        results <- data.frame()
        pheno$Gid <- as.character(pheno$Gid)  # Make sure this is a character for matching
        for(r in 1:nrep){
            set.seed(seeds[r])
            sub <- sample.int(n, n.sample)
            pheno.train <- pheno[sub,]
            Gid.pred <- as.character(pheno[-sub,"Gid"])
            if(is.null(weights)){
                weights_sub <- rep(1,nrow(pheno.train))
                pheno.train <- cbind(pheno.train, weights_sub)
            } else{
                weights_sub <- weights[as.charater(pheno.train$Gid),trait]
                pheno.train <-cbind(pheno.train, weights_sub)
            }
            assign("trait", trait, envir=.GlobalEnv)
            assign("pheno.train", pheno.train, envir=.GlobalEnv)
            assign("ginv", ginv, envir=.GlobalEnv)
            fit <- asreml(fixed=eval(parse(text=trait))~1, random=~ped(Gid), ginverse=list(Gid=ginv),
                          weights="weights_sub", data=pheno.train, maxiter=250, trace=F)
            rm(list=c("trait", "pheno.train", "ginv"), envir=.GlobalEnv) 
            pred <- coef(fit)$random
            rownames(pred) <- gsub("ped\\(Gid\\)_", "", rownames(pred))
            pred <- pred[Gid.pred,]
            val <- pheno[-sub, trait]
            results <- rbind(results, data.frame(Gid=Gid.pred, 
                            Rep=rep(r,length(Gid.pred)),
                            Set=rep(set,length(Gid.pred)),
                		    Trait=rep(trait, length(Gid.pred)),
                            Pred=pred, Val=val)) 
        }
        return(results)
}

asremlStrat <- function(sets, pheno, trait,  ginv, weights=NULL, size.train, size.test,  nrep, seeds){
   # Function for stratified cross-validation to test effect of trainng population size
    results <- data.frame()
    c <- 1
    n.train <- ceiling(size.train/length(sets))
    n.test <- ceiling(size.test/length(sets))
    pheno$Pop <- as.character(pheno$Pop)  # Make sure this is a character for matching
    pheno$Gid <- as.character(pheno$Gid)  # Make sure this is a character for matching
    for(r in 1:nrep){
        train <- c()
        test <- c()
        for(s in sets){
           idx <- which(pheno$Pop == s)
           set.seed(seeds[c])
           c <- c +1
           train <-c(train, sample(idx, n.train))
           set.seed(seeds[c])
           c <- c +1
           test <- c(test, sample(idx[!(idx %in% train)], n.test))
        }
        pheno.train <- pheno[train,]
        val <- pheno[test, trait]
        if(is.null(weights)){
            weights_sub <- rep(1,nrow(pheno.train))
            pheno.train <- cbind(pheno.train, weights_sub)
        } else{
            weights_sub <- weights[pheno.train$Gid,trait]
            pheno.train <-cbind(pheno.train, weights_sub)
         }
        assign("trait", trait, envir=.GlobalEnv)
        assign("pheno.train", pheno.train, envir=.GlobalEnv)
        assign("ginv", ginv, envir=.GlobalEnv)
        fit <- asreml(fixed=eval(parse(text=trait))~1, random=~ped(Gid), ginverse=list(Gid=ginv),
                      weights="weights_sub", data=pheno.train, maxiter=100, trace=F)
        rm(list=c("trait", "pheno.train", "ginv"), envir=.GlobalEnv)
        pred <- coef(fit)$random
        rownames(pred) <- gsub("ped\\(Gid\\)_", "", rownames(pred))
        pred <- pred[pheno$Gid[test],]
        n <- length(pred)
        results <- rbind(results, data.frame(Gid=names(pred),
                         Rep=rep(r,n),
                    	 Trait=rep(trait, n),
                    	 Set=rep(size.train, n),
                         Pred=pred, Val=val))      
  }
  return(results)
}
