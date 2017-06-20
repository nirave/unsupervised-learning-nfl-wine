reds<-read.csv(file="winequality-red.csv",  sep=';', header = TRUE)

reds$quality <- factor(reds$quality)

library(caret)
set.seed(62433)

#Separate out the training into a training and a validation section
inTrain <- createDataPartition(reds$quality, p = 0.80)[[1]]
training <- reds[ inTrain,]
validation <- reds[-inTrain,]
