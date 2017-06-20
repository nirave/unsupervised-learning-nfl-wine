source("nfl.R")
source("wine.R")


#########################################################
#1.	Run the clustering algorithms on the data sets and 
#   describe what you see.
#########################################################

#Normalize the data
reds_inputs <-reds[,1:11]
reds_temp<-scale(reds_inputs)
reds[,1:11]<-reds_temp

algo <- "Forgy"

wss<-c()
times<-c()
for (i in 2:15) {
    wss[i] <- 0
    times[i] <- 0
    for (j in 1:10) {
        ptm <- proc.time()
        wss[i] <- wss[i] + sum(kmeans(reds, i, nstart = 10, algorith = algo)$withinss)
        predictTime<-(proc.time() - ptm)[1]
        times[i] <- predictTime + times[i]
    }
    wss[i] = wss[i] / 10
    times[i] = times[i] / 10
}


plot(wss, main="Forgy Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")
plot(times, main="Wine Forgy Method Cluster Timing", xlab="Number of Clusters", ylab="Time")


algo <- "Lloyd"


wss<-c()
for (i in 2:15) {
    wss[i] <- 0
    for (j in 1:10) {
        wss[i] <- wss[i] + sum(kmeans(reds, i, nstart = 10, algorith = algo)$withinss)
    }
    wss[i] = wss[i] / 10
}


plot(wss, main="Lloyd Method Cluster Evaluation", xlab="Number of Clusters", ylab="Within groups sum of squared")


#We see the kink at 6

algo <- "Forgy"

ptm <- proc.time()
wineCluster <- kmeans(reds, 6, nstart = 20, algorith = algo)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

#The plot:
library(fpc)
plotcluster(wine, wineCluster$cluster)

library(MASS)
parcoord(wine, wineCluster$cluster)


library(flexclust)
crosstab <- table(wine$quality, wineCluster$cluster)
agreement <- randIndex(crosstab)
print(agreement)

#Which is better than 0, but not by much.  


n_train_scaled<-subset(nfl_stats_total_2013, select = -c(Fun))
n_train_scaled$WinOrLose <- 1.0 * as.numeric(n_train_scaled$WinOrLose)
n_train_scaled$Site <- 1.0 * as.numeric(n_train_scaled$Site) 
n_train_scaled[is.na(n_train_scaled)] <- 0

n_train_scaled<-scale(n_train_scaled)

algo <- "Forgy"

wss<-c()
times<-c()
for (i in 2:30) {
    times[i] <- 0
    wss[i] <- 0
    for (j in 1:10) {
        ptm <- proc.time()
        wss[i] <- wss[i] + sum(kmeans(n_train_scaled, i, nstart = 10, algorith = algo)$withinss)
        predictTime<-(proc.time() - ptm)[1]
        times[i] <- predictTime + times[i]
    }
    wss[i] = wss[i] / 10
    times[i] = times[i] / 10
}

plot(times, main="NFL Forgy Method Cluster Timing", xlab="Number of Clusters", ylab="Time")

plot(wss, main="Forgy Method Cluster Evaluation - NFL", xlab="Number of Clusters", ylab="Within groups sum of squared")

algo <- "Lloyd"

wss<-c()
for (i in 2:15) {
    wss[i] <- 0
    for (j in 1:10) {
        wss[i] <- wss[i] + sum(kmeans(n_train_scaled, i, nstart = 10, algorith = algo)$withinss)
    }
    wss[i] = wss[i] / 10
}


plot(wss, main="Lloyd Method Cluster Evaluation - NFL", xlab="Number of Clusters", ylab="Within groups sum of squared")

ptm <- proc.time()
nflCluster<-kmeans(n_train_scaled, 10, nstart=20)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

plotcluster(n_train_scaled, nflCluster$cluster)
nflCluster$centers

parcoord(n_train_scaled, nflCluster$cluster)

library(flexclust)
crosstab <- table(nfl_stats_total_2013$WinOrLose, nflCluster$cluster)
agreement <- randIndex(crosstab)
print(agreement)


#EM 

library(mclust)

wineBIC<-mclustBIC(wine)
plot(wineBIC)

#Plot the times
times<-c()
for (i in 2:11) {
    ptm <- proc.time()
    print("before run")
    emWineCluster<-Mclust(wine, i)
    times[i] <-(proc.time() - ptm)[1]
    print(times[i])
}

plot(times, main="Wine EM Cluster Timing", xlab="Number of Clusters", ylab="Time")

ptm <- proc.time()
emWineCluster<-Mclust(wine, 7)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

crosstab <- table(wine$quality, emWineCluster$classification)
table(wine$quality, emWineCluster$classification)
agreement <- randIndex(crosstab)
print(agreement)

plot(emWineCluster)

plotcluster(wine, emWineCluster$classification)
parcoord(wine, emWineCluster$classification)

nflBIC<-mclustBIC(n_train_scaled)
plot(nflBIC)

ptm <- proc.time()
emNFLCluster<-Mclust(n_train_scaled, 3)
predictTime<-(proc.time() - ptm)[1]
print(predictTime)

times<-c()
for (i in 2:30) {
    ptm <- proc.time()
    print("before run")
    Mclust(n_train_scaled, i)
    times[i] <-(proc.time() - ptm)[1]
    print(times[i])
}

plot(times, main="NFL EM Cluster Timing", xlab="Number of Clusters", ylab="Time")

crosstab <- table(n_train_scaled$WinOrLose, emNFLCluster$classification)
table(n_train_scaled$quality, emNFLCluster$classification)
agreement <- randIndex(crosstab)
print(agreement)

plot(emNFLCluster)

plotcluster(n_train_scaled, emNFLCluster$classification)
parcoord(n_train_scaled, emNFLCluster$classification)

#########################################################
#2.	Apply the dimensionality reduction algorithms to the two 
#   datasets and describe what you see.
#########################################################

#PCA
#FROM https://github.com/zygmuntz/wine-quality/blob/master/pca_red.r
data_file = "winequality-red.csv"

wine <- read.csv( data_file, sep=';', header = TRUE )
# number of elements
numel = length( as.matrix( wine )) / length( wine )

# pca analysis
ptm <- proc.time()
pcx <- prcomp( wine, scale = TRUE )
predictTime<-(proc.time() - ptm)
print(predictTime)
biplot( pcx, xlabs = rep( '.', numel ))

# another window
dev.new()

# principal components
bar_colors = c( 'red', 'red', rep( 'gray', 10 ))
plot( pcx, col = bar_colors )

#END OF COPY

library(scatterplot3d)
scatterplot3d(pcxPCA3d[, c(1,2,3)], pch=16, color=as.numeric((wine[,12])))

#Eigenvalues
ev <- pcx$sdev^2
plot(ev)

variance_plot<-function(dims_red_data, title) {
    #std_dev <- dims_red_data$sdev
    std_dev <- apply(dims_red_data, 2, sd)
    pr_var <- std_dev^2
    pr_var
    prop_varex <- pr_var/sum(pr_var)
    prop_varex
    plot(prop_varex, xlab = "Component",      
         ylab = "Proportion of Variance Explained",
         type = "b", main=title)
    print(prop_varex)
}

variance_plot(pcx$x, "PCA Wine Variance Explained")

#NFL PCA:
numel_nfl = length( as.matrix( n_train_scaled )) / length( n_train_scaled )

# pca analysis
ptm <- proc.time()
pcx_nfl <- prcomp( n_train_scaled, scale = TRUE )
predictTime<-(proc.time() - ptm)
print(predictTime)
biplot( pcx_nfl, xlabs = rep( '.', numel_nfl ))

# another window
dev.new()

# principal components
bar_colors = c( 'red', 'red', rep( 'gray', 10 ))
plot( pcx_nfl, col = bar_colors )

#Eigenvalue
ev_nfl <- pcx_nfl$sdev^2
plot(ev_nfl, type="l")

#Variance explained
std_dev <- pcx_nfl$sdev
pr_var <- std_dev^2
pr_var
prop_varex <- pr_var/sum(pr_var)
prop_varex
plot(prop_varex, xlab = "Principal Component",      
     ylab = "Proportion of Variance Explained",
     type = "b")


variance_plot(pcx_nfl$x, "PCA NFL Variance Explained")

library(nFactors)
ev <- eigen(cor(pcx$x)) # get eigenvalues
ap <- parallel(subject=nrow(pcx$x),var=ncol(pcx$x),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


library(nFactors)
ev <- eigen(cor(pcx_nfl$x)) # get eigenvalues
ap <- parallel(subject=nrow(pcx_nfl$x),var=ncol(pcx_nfl$x),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

###ICA
library(fastICA)
library(e1071)
library(ggplot2)

ptm <- proc.time()
ica <- fastICA(wine[1:11], 11)
predictTime<-(proc.time() - ptm)
print(predictTime)

icaS <- data.frame(ica$S)

icaSkurt <- data.frame(apply(ica$S,2,kurtosis))
icaSkurt <- cbind(icaSkurt,seq(1:11))

#remember, you want as far as 3 (in absolute value) as possible
icaSkurt[,1] <- abs(icaSkurt[,1] - 3)

names(icaSkurt) = c("kurtosis","component")
ggplot(icaSkurt,aes(x = component, y = kurtosis))+geom_bar(stat="identity") 

icsSclasses <- cbind(aS,wine[,12])

bestICA <- order(icsSkurt$kurtosis)[c(8,9,10,11)]

pcxICA3d <- icsSclasses[,bestICA]

scatterplot3d(pcxICA3d[, c(1,2,3)], pch=16, color=as.numeric((wine[,12])))

#NFL ICA
ptm <- proc.time()
ica_nfl <- fastICA(n_train_scaled, 32)
predictTime<-(proc.time() - ptm)
print(predictTime)

icaS_nfl <- data.frame(ica_nfl$S)

icaSkurt_nfl <- data.frame(apply(ica_nfl$S,2,kurtosis))
icaSkurt_nfl <- cbind(icaSkurt_nfl,seq(1:32))

#remember, you want as far as 3 (in absolute value) as possible
icaSkurt_nfl[,1] <- abs(icaSkurt_nfl[,1] - 3)

names(icaSkurt_nfl) = c("kurtosis","component")
ggplot(icaSkurt_nfl,aes(x = component, y = kurtosis))+geom_bar(stat="identity") 

#icsSclasses_nfl <- cbind(aS,wine[,12])
icsSclasses_nfl <- icaS_nfl

bestICA_nfl <- order(icaSkurt_nfl$kurtosis)[c(29,30,31,32)]

pcxICA3d_nfl <- icsSclasses_nfl[,bestICA_nfl]

###Random Projection

kurtosis_graph<-function(data, last) {
    
    ##Copied from rcabrera
    datafica<- data.frame()
    kurtosis_total <- data.frame()
    
    for (i in seq(2:last)) {
        
        acum_kurt <- 0
        
        for (j in seq(1:20)) {
            
            ICA_DataFrame_loop <- fastICA(data[,1:last],i)
            a_loop<- ICA_DataFrame_loop
            aS_loop <- data.frame(a_loop$S)
            aSkurt_loop <- data.frame(apply(a_loop$S,2,kurtosis))
            aSkurt_loop <- abs(aSkurt_loop-3)
            
            datafica <- rbind(datafica,c(i,sum(aSkurt_loop)))
            
            acum_kurt <- acum_kurt + sum(aSkurt_loop/i)
            
        }
        
        cat("Components:",i,"avg norm kurt:",acum_kurt/(i*20),"\n")
        kurtosis_total <- rbind(kurtosis_total, c(i,acum_kurt))
        
    }
    
    names(datafica) = c("Components","AvgExcessKurtosis")
    plot(kurtosis_total, type="l", main="Average Excess Kurtosis", xlab="Components", ylab="Avg Excess Kurtosis")
    #end of copy from rcabrera
}

kurtosis_graph(wine, 11)
kurtosis_graph(n_train_scaled, 30)

source("random_projection_reconstruction.R")
#Completely copied from https://github.com/chappers/CS7641-Machine-Learning/blob/master/Unsupervised%20Learning/R/random_projection_gauss.R


### Random Projections

source("random_projection_gauss.R")
library(dplyr)
library(reshape)

# Wine
wq_nl = wine[,1:11]
wq_rca <- Map(function(x) {
    gaussian_random_projection(wine[,1:11], 6)
}, 1:100)


# get the ones which immitate the result best.

wqrcadiff <- Map(function(x) {
    sum((wq_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, wq_rca) %>% melt


bestrca <- wqrcadiff %>% arrange(value) %>% head(1)
names(bestrca) <- c("value", "k")
wqrca <- cbind(as.data.frame(wq_rca[[bestrca$k]]$RP), quality=wine$quality)

#Reconstruction matrix
k = 6
p = ncol(wine)
R <<- matrix(data = rnorm(k*p),
             nrow = k,
             ncol = p)

#end of copy from https://github.com/chappers/CS7641-Machine-Learning/blob/master/Unsupervised%20Learning/R/random_projection_gauss.R

scatterplot3d(wqrca[, c(1,2,3)], pch=16, color=as.numeric((wine[,12])))

wq_nl_nfl = n_train_scaled
wq_rca_nfl <- Map(function(x) {
    gaussian_random_projection(n_train_scaled, 6)
}, 1:100)


# get the ones which immitate the result best.

wqrcadiff_nfl <- Map(function(x) {
    sum((wq_nl_nfl - (x$RP %*% MASS::ginv(x$R)))^2)
}, wq_rca_nfl) %>% melt


bestrca_nfl <- wqrcadiff_nfl %>% arrange(value) %>% head(1)
names(bestrca_nfl) <- c("value", "k")
#wqrca_nfl <- cbind(as.data.frame(wq_rca_nfl[[bestrca_nfl$k]]$RP), quality=nfl_game_data_frame$WinOrLose)
wqrca_nfl <- as.data.frame(wq_rca_nfl[[bestrca_nfl$k]]$RP)

### Factor Analysis

factan <-factanal(wine[1:12], 4)
a<-rowSums(factan$loadings[,1] * wine)
b<-rowSums(factan$loadings[,2] * wine)
c<-rowSums(factan$loadings[,3] * wine)
d<-rowSums(factan$loadings[,4] * wine)

factan_dataset <- cbind(a, b, c, d)

nfl_factan <-factanal(n_train_scaled, 14)
a<-rowSums(nfl_factan$loadings[,1] * n_train_scaled)
b<-rowSums(nfl_factan$loadings[,2] * n_train_scaled)
c<-rowSums(nfl_factan$loadings[,3] * n_train_scaled)
d<-rowSums(nfl_factan$loadings[,4] * n_train_scaled)
e<-rowSums(nfl_factan$loadings[,5] * n_train_scaled)
f<-rowSums(nfl_factan$loadings[,6] * n_train_scaled)
g<-rowSums(nfl_factan$loadings[,7] * n_train_scaled)
h<-rowSums(nfl_factan$loadings[,8] * n_train_scaled)
i<-rowSums(nfl_factan$loadings[,9] * n_train_scaled)
j<-rowSums(nfl_factan$loadings[,10] * n_train_scaled)
k<-rowSums(nfl_factan$loadings[,11] * n_train_scaled)
l<-rowSums(nfl_factan$loadings[,12] * n_train_scaled)
m<-rowSums(nfl_factan$loadings[,13] * n_train_scaled)
n<-rowSums(nfl_factan$loadings[,14] * n_train_scaled)

nfl_factan_dataset <- cbind(a, b, c, d, e, f, g, h, i)
plot(nfl_factan_dataset, type="n")

library(nFactors)
library(MASS)
ev <- eigen(cor(wine)) # get eigenvalues
ap <- parallel(subject=nrow(wine),var=ncol(wine), rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

ev <- eigen(cor(n_train_scaled)) # get eigenvalues
ap <- parallel(subject=nrow(n_train_scaled),var=ncol(n_train_scaled), rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 


#########################################################
#3.	Reproduce your clustering experiments, but on the 
#   data after you've run dimensionality reduction on it.
#########################################################

runCluster<-function(data, dataset, results, numClusters, title) {
    algo <- "Forgy"
    
    wss<-c()
    for (i in 2:15) {
        wss[i] <- 0
        for (j in 1:10) {
            wss[i] <- wss[i] + sum(kmeans(data, i, nstart = 10, algorith = algo)$withinss)
        }
        wss[i] = wss[i] / 10
    }
    
    
    plot(wss)
    
    
    ptm <- proc.time()
    wineCluster2 <- kmeans(data, numClusters, nstart = 20)
    predictTime<-(proc.time() - ptm)
    print(predictTime)
    
    crosstab <- table(results, wineCluster2$cluster)
    agreement <- randIndex(crosstab)
    print(agreement)
    
    plotcluster(dataset, wineCluster2$cluster)
    
    #EM after reduction
    
    ptm <- proc.time()
    emWineCluster2<-Mclust(data, numClusters)
    predictTime<-(proc.time() - ptm)[1]
    print(predictTime)
    
    #plot(emWineCluster2)
    
    crosstab <- table(results, emWineCluster2$classification)
    agreement <- randIndex(crosstab)
    print(agreement)
    
    plotcluster(dataset, emWineCluster2$classification)
    parcoord(dataset, emWineCluster2$classification)
    
    
}

pcxPCA4d <- cbind(pcx$x[,c(1,2,3,4)],wine[,12])
pcxPCA3d_nfl <- cbind(pcx_nfl$x[,c(1,2,3)])

runCluster(pcxPCA4d, wine, wine$quality, 6, "Wine PCA")
runCluster(pcxPCA3d_nfl, n_train_scaled, nfl_stats_total_2013$WinOrLose, 3, "NFL PCA")

runCluster(pcxICA3d, wine, wine$quality, 6, "Wine PCA")
runCluster(pcxICA3d_nfl, n_train_scaled, nfl_stats_total_2013$WinOrLose, 3, "NFL PCA")

runCluster(wqrca, wine, wine$quality, 6, "Wine Random Projection")
runCluster(wqrca_nfl, n_train_scaled, nfl_stats_total_2013$WinOrLose, 3, "NFL Random Projection")

runCluster(factan_dataset, wine, wine$quality, 6, "Wine Factor Analysis")
runCluster(nfl_factan_dataset,  n_train_scaled, nfl_stats_total_2013$WinOrLose, 3, "NFL Factor Analysis")


#########################################################
#4.	Apply the dimensionality reduction algorithms to 
#   one of your datasets from assignment #1 (if you've 
#   reused the datasets from assignment #1 to do experiments
#   1-3 above then you've already done this) and rerun your
#   neural network learner on the newly projected data.
################
#########################################

library(nnet)

cat="quality"
ideal <-class.ind(training[[cat]])
nntraining<-subset(training, select = -c(quality))

#Normal!
ptm <- proc.time()
nueral_net<-nnet(nntraining, ideal, size=30, softmax=TRUE)
predictions <- as.factor(predict(nueral_net, validation, type="class"))
predictTime<-(proc.time() - ptm)
print(predictTime)
accuracy <- confusionMatrix(predictions, validation[[cat]])$overall['Accuracy']
print(accuracy)

#PCA 3 dimensions - norm
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
winenormal<-range01(wine)

pcx_norm <- prcomp(winenormal)
pcx_norm_4d <- cbind(pcx_norm$x[,c(1,2,3,4)],wine[,12])

wine_nueralnetwork<-function(data, ideal, validation, last) {
    cat = "quality"
    new_nntraining<-data[ inTrain,]
    new_nntraining<-new_nntraining[,1:last]
    ptm <- proc.time()
    nueral_net<-nnet(new_nntraining, ideal, size=30, softmax=TRUE)
    predictTime<-(proc.time() - ptm)
    print(predictTime)
    predictions <- as.factor(predict(nueral_net, validation, type="class"))
    accuracy <- confusionMatrix(predictions, validation[[cat]])$overall['Accuracy']
    print(accuracy)
}

wine_nueralnetwork(pcx_norm_4d, ideal, validation, 4)

#ICA Neural Network - need to normalize to get it to work
ptm <- proc.time()
ica <- fastICA(winenormal[1:11], 11)
predictTime<-(proc.time() - ptm)
print(predictTime)

icaS <- data.frame(ica$S)

icaSkurt <- data.frame(apply(ica$S,2,kurtosis))
icaSkurt <- cbind(icaSkurt,seq(1:11))

#remember, you want as far as 3 (in absolute value) as possible
icaSkurt[,1] <- abs(icaSkurt[,1] - 3)

icsSclasses <- cbind(aS,wine[,12])

bestICA <- order(icsSkurt$kurtosis)[c(8,9,10,11)]

pcxICA3d <- icsSclasses[,bestICA]

wine_nueralnetwork(pcxICA3d, ideal, validation, 3)

#Random Projection (no normalization needed)
wine_nueralnetwork(wqrca, ideal, validation, 6)


#Factor Analysis (FA) - Neural Network (normalization is needed)
factan <-factanal(winenormal[1:12], 4)
a<-rowSums(factan$loadings[,1] * wine)
b<-rowSums(factan$loadings[,2] * wine)
c<-rowSums(factan$loadings[,3] * wine)
d<-rowSums(factan$loadings[,4] * wine)

factan_dataset <- cbind(a, b, c, d)
wine_nueralnetwork(factan_dataset, ideal, validation, 4)


#########################################################
#5.	Apply the clustering algorithms to the same dataset
#   to which you just applied the dimensionality reduction
#   algorithms (you've probably already done this), treating
#   the clusters as if they were new features. In other
#   words, treat the clustering algorithms as if they were
#   dimensionality reduction algorithms. Again, rerun your
#   neural network learner on the newly projected data.
#########################################################

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
winenormal<-range01(wine)

pcx_norm <- prcomp(winenormal[ inTrain, ])
pcx_norm_4d <- cbind(pcx_norm$x[,c(1,2,3,4)],winenormal[ inTrain ,12])

data<-pcx_norm_4d
numClusters<-6
wineClusterNN <- kmeans(data, numClusters, nstart = 20)

new_reds <- cbind(reds[ inTrain, ], wineClusterNN$cluster)

new_training <- new_reds
new_validation <- reds[-inTrain,]
new_nntraining<-subset(new_training, select = -c(quality))
ptm <- proc.time()
nueral_net<-nnet(new_nntraining, ideal, size=30, softmax=TRUE)
predictTime<-(proc.time() - ptm)
print(predictTime)
predictions <- as.factor(predict(nueral_net, validation, type="class"))
accuracy <- confusionMatrix(predictions, validation[['quality']])$overall['Accuracy']
print(accuracy)

#EM

emWineClusterNN<-Mclust(data, 7)

new_reds <- cbind(reds[ inTrain, ], emWineClusterNN$cluster)

new_training <- new_reds
new_validation <- reds[-inTrain,]
new_nntraining<-subset(new_training, select = -c(quality))
ptm <- proc.time()
nueral_net<-nnet(new_nntraining, ideal, size=30, softmax=TRUE)
predictTime<-(proc.time() - ptm)
print(predictTime)
predictions <- as.factor(predict(nueral_net, validation, type="class"))
accuracy <- confusionMatrix(predictions, validation[['quality']])$overall['Accuracy']
print(accuracy)
