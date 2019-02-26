load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1]<- factor(id[,1])

set.seed(123)
id_shuffle <- id[sample(nrow(id)),]

id_pca <- prcomp(id_shuffle[,-1], center = TRUE, scale = TRUE)

totalvar = sum(id_pca$sdev)

#2.1.1
plot(id_pca$sdev[1:20], main= "standard deviation (from eigenvalues)", ylab="std. dev.", xlab="Principal component")
plot(id_pca$sdev[1:20]/totalvar, main = "Proportion of variance", xlab="Principal component", ylab= "proportion of total variance explained")
plot(cumsum(id_pca$sdev/totalvar), main="Cumulative sum of proportions of variance", xlab="Principal Component", ylab="cumulative sum")


#2.1.2
sum(id_pca$sdev[1:72]/totalvar)



data <- id_pca$x[,1:72]
  
train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

library(class)
library(gmodels)
library(caret)

"Sample size: Train 10000, Test = 30000, k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"Sample size: Train 10000, Test = 30000, k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"Sample size: Train 10000, Test = 30000, k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc