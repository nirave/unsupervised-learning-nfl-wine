#Copied from rcabrera from SLack
require(dplyr)

randomProjection <- function(A, k = 10, method = 'norm') {
    
    if (!is.matrix(A)) {
        tmp <- as.matrix(dplyr::select(A, -quality))
    } else {
        tmp <- dplyr::select(A, -quality)
    }
    p <- ncol(tmp)
    #set.seed(as.numeric(format(Sys.time(), '%S')))
    if (method == 'norm') {
        R <<- matrix(data = rnorm(k*p),
                     nrow = k,
                     ncol = p)
    } else if (method == 'zeroOne') {
        R <<- matrix(data = round(runif(k*p), 0),
                     nrow = k,
                     ncol = p)
    } else if (method == 'negOne') {
        nums <- round(runif(k*p), 0)
        R <<- matrix(data = replace(nums, nums==0, -1),
                     nrow = k,
                     ncol = p)
    } else {
        stop('method must be norm, zeroOne, or negOne')
    }
    
    
    tmp <- apply(tmp, 2, function(x) (x - mean(x)) / sd(x))
    as.data.frame(t(R %*% t(tmp))) %>%
        mutate(quality = A$quality)
}

R <- NULL


library(dplyr)

ctgtemp <- wine

bestrmse <- Inf
dataf <- data.frame()
variancedf <- data.frame()

for (j in 1:11) {
    sumrmse<-0
    bestrmse<-Inf
    variances<-c()
    variances <- c(variance, 1:50)
    for (i in 1:50) {
        R<-0
        ctgRandom <- randomProjection(ctgtemp,k=j)
        Reconstructed <-  as.matrix(ctgRandom[,-(j+1)]) %*% ginv(t(R))
        sumrmse <- sum(RMSE(Reconstructed,wine[,1:12]))
        variances[i]<-sumrmse
        
        if (bestrmse>sumrmse) {
            bestRandComp <- ctgRandom 
            bestR <- R
            bestrmse <- sumrmse
        } 
        
        #        dataf <- rbind(dataf,c(sumrmse,j))
    }
    variancedf<-rbind(variancedf, c(j, var(variances)))
    dataf <- rbind(dataf,c(j,sumrmse/50))
}

print(bestrmse)
plot(dataf, type="l", main="Reconstruction error wine", ylab="RMSE", xlab="Number of Components")
plot(variancedf, type="l", main="Variance of Random Projection", ylab="Variance", xlab="Number of Components")


randomProjectionNFL <- function(A, k = 10, method = 'norm') {
    require(dplyr)
    
    if (!is.matrix(A)) {
        tmp <- as.matrix(dplyr::select(A, -WinOrLose))
    } else {
        tmp <- dplyr::select(A, -WinOrLose)
    }
    
    p <- ncol(tmp)
    #set.seed(as.numeric(format(Sys.time(), '%S')))
    if (method == 'norm') {
        R <<- matrix(data = rnorm(k*p),
                     nrow = k,
                     ncol = p)
    } else if (method == 'zeroOne') {
        R <<- matrix(data = round(runif(k*p), 0),
                     nrow = k,
                     ncol = p)
    } else if (method == 'negOne') {
        nums <- round(runif(k*p), 0)
        R <<- matrix(data = replace(nums, nums==0, -1),
                     nrow = k,
                     ncol = p)
    } else {
        stop('method must be norm, zeroOne, or negOne')
    }
    
    
    tmp <- apply(tmp, 2, function(x) (x - mean(x)) / sd(x))
    as.data.frame(t(R %*% t(tmp))) %>%
        mutate(WinOrLose = A$'WineOrLose')
}

nfl_temp <- nfl_stats_total_2013
nfl_temp$Site<- as.numeric(nfl_temp$Site)
nfl_temp$WinOrLose<- as.numeric(nfl_temp$WinOrLose)
nfl_temp <- dplyr::select(nfl_temp, -Fun)
nfl_temp[is.na(nfl_temp)] <- 1

ctgtemp <- nfl_temp

bestrmse <- Inf
dataf <- data.frame()
variancedf <- data.frame()

for (j in 1:30) {
    sumrmse<-0
    bestrmse<-Inf
    variances<-c()
    variances <- c(variance, 1:50)
    for (i in 1:50) {
        R<-0
        ctgRandom <- randomProjectionNFL(ctgtemp,k=j)
        Reconstructed <-  as.matrix(ctgRandom[,-(j+1)]) %*% ginv(t(R))
        sumrmse <- sum(RMSE(Reconstructed,n_train_scaled))
        variances[i]<-sumrmse
        
        if (bestrmse>sumrmse) {
            bestRandComp <- ctgRandom 
            bestR <- R
            bestrmse <- sumrmse
        } 
        
        #        dataf <- rbind(dataf,c(sumrmse,j))
    }
    variancedf<-rbind(variancedf, c(j, var(variances)))
    dataf <- rbind(dataf,c(j,sumrmse/50))
    print(sumrmse)
}

print(bestrmse)
plot(dataf, type="l", main="Reconstruction error wine", ylab="RMSE", xlab="Number of Components")
plot(variancedf, type="l", main="Variance of Random Projection", ylab="Variance", xlab="Number of Components")
