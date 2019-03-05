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
library(class)
library(gmodels)
library(caret)
cumsum(id_pca$sdev/totalvar)


indexFor80pct <- 72
indexFor90pct <- 115
indexFor95pct <- 157
indexFor99pct <- 232

data <- id_pca$x[,1:indexFor80pct]
  
train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

" 80 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"80 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"80 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc



data <- id_pca$x[,1:indexFor90pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

" 90 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"90 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"90 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc


data <- id_pca$x[,1:indexFor95pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

" 95 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"95 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"95 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc



data <- id_pca$x[,1:indexFor99pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

"99 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"99 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"99 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc


# disjunct

id_pca_disj <- prcomp(id[,-1], center = TRUE, scale = TRUE)

totalvar_disj = sum(id_pca_disj$sdev)

#2.1.1
plot(id_pca_disj$sdev[1:20], main= "standard deviation (from eigenvalues)", ylab="std. dev.", xlab="Principal component")
plot(id_pca_disj$sdev[1:20]/totalvar_disj, main = "Proportion of variance", xlab="Principal component", ylab= "proportion of total variance explained")
plot(cumsum(id_pca_disj$sdev/totalvar), main="Cumulative sum of proportions of variance", xlab="Principal Component", ylab="cumulative sum")


indexFor80pct <- 72
indexFor90pct <- 115
indexFor95pct <- 157
indexFor99pct <- 232


data <- id_pca_disj$x[,1:2]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id[0:10000,1]
test_labels <- id[10001:40000,1]

" 80 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc


"80 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"80 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

data <- id_pca_disj$x[,1:indexFor90pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id[0:10000,1]
test_labels <- id[10001:40000,1]



" 90 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"90 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"90 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

data <- id_pca_disj$x[,1:indexFor95pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id[0:10000,1]
test_labels <- id[10001:40000,1]



" 95 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"95 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"95 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

data <- id_pca_disj$x[,1:indexFor99pct]

train <- as.data.frame(data[0:10000,])
test <- as.data.frame(data[10001:40000,])
train_labels <- id[0:10000,1]
test_labels <- id[10001:40000,1]

"99 pct k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"99 pct k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"99 pct k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

# 2.2

normalize <- function(x){ 
  return ((x-min(x))/(max(x)-min(x)))
  
}

#No normalization

id_pca <- prcomp(id_shuffle[,-1], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[,1:indexFor80pct]


folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_shuffle[-folds[[i]],1] 
  test_labels <- id_shuffle[folds[[i]],1] 
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#No normalization
summary(results)
sd(results)

#BEFORE PCA normalization
id_shuffle_norm <- lapply(id_shuffle[,-1],normalize)
id_shuffle_norm <- as.data.frame(id_shuffle_norm)
id_pca <- prcomp(id_shuffle_norm[], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[,1:indexFor80pct]


folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_shuffle[-folds[[i]],1] 
  test_labels <- id_shuffle[folds[[i]],1] 
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#BEFORE PCA normalization
summary(results)
sd(results)

#AFTER PCA normalization
id_pca <- prcomp(id_shuffle[,-1], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[,1:indexFor80pct]
data <- as.data.frame(data)
data <- lapply(data,normalize)
data <- as.data.frame(data)

folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_shuffle[-folds[[i]],1] 
  test_labels <- id_shuffle[folds[[i]],1] 
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#AFTER PCA normalization
summary(results)
sd(results)



