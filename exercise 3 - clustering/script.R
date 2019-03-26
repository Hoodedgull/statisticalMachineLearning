library(class)
library(gmodels)
library(caret)
load("idList-cornered-100.Rdata")
id <- idList[15:16]
trainDataSet <- as.data.frame(id[1])


train_set <- trainDataSet[1 : (nrow(trainDataSet)),2:ncol(trainDataSet)]
train_labels <- trainDataSet[1 : (nrow(trainDataSet)),1]

testDataSet <- as.data.frame(id[2])
test_set <- testDataSet[ 1 : nrow(testDataSet) ,2:ncol(testDataSet)]
test_labels <- testDataSet[  1 : nrow(testDataSet) ,1]

# id_mat <- data.matrix(testDataSet, rownames.force = NA)

# rotate <- function(x)
#   t(apply(x, 2, rev))
# imageSize <- sqrt(ncol(id_mat) - 1)
# # Show first 10 images
# for (i in 1:10)
# {
#   rotated <- c(id_mat[-200 + i * 200 + 1, 2:ncol(id_mat)])
#   rotated <-
#     ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
# 
#   image <-
#     matrix(rotated,
#            nrow = imageSize,
#            ncol = imageSize,
#            byrow = FALSE)
#   image <- rotate(image)
#   image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
# 
# }

set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()

clusters <- 50
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# 
# id_mat <- data.matrix(train_dat, rownames.force = NA)
# 
# rotate <- function(x)
#   t(apply(x, 2, rev))
# imageSize <- sqrt(ncol(id_mat) - 1)
# # Show first 10 images
# for (i in 1:10)
# {
#   rotated <- c(id_mat[-200 + i * 200 + 1, 2:ncol(id_mat)])
#   rotated <-
#     ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
# 
#   image <-
#     matrix(rotated,
#            nrow = imageSize,
#            ncol = imageSize,
#            byrow = FALSE)
#   image <- rotate(image)
#   image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
# 
# }

# KNN with no clustering 
beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))


# KNN with 50 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))




cipher_cluster <- c()
label_cluster <- c()

clusters <- 75
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 75 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))



cipher_cluster <- c()
label_cluster <- c()

clusters <- 100
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 100 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))


cipher_cluster <- c()
label_cluster <- c()

clusters <- 125
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 125 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))


cipher_cluster <- c()
label_cluster <- c()

clusters <- 150
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 150 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))


cipher_cluster <- c()
label_cluster <- c()

clusters <- 175
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 175 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))



cipher_cluster <- c()
label_cluster <- c()

clusters <- 200
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 200 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
afterTime-beforeTime
sum(diag(table$prop.tbl))

#########################################################
#####################3.1.2###############################
#########################################################
dataset <- do.call(rbind,idList[15:16])
dataset <- as.data.frame(dataset)
dataset$V1 <- factor(dataset$V1)
folds <- createFolds(dataset$V1, k = 10)
clusters <- 100
results = c()
resultsClusters = c()
for(i in 1:10){
  train <- dataset[-folds[[i]],-1]
  test <- dataset[folds[[i]],-1]
  
  train_labels <- dataset[-folds[[i]],1] 
  test_labels <- dataset[folds[[i]],1] 
  
  #Clustering
  cipher_cluster <- c()
  label_cluster <- c()
  for( j in 0:9) {
    single_cipher_data <- train[ train_labels == j ,]
    clusterData <- kmeans(single_cipher_data, clusters)
    cipher_cluster[[j + 1]] <- clusterData$centers
    label_cluster[[j + 1]] <- c(1:clusters)*0 + j
  }
  
  train_lab <- factor(unlist(label_cluster))
  train_dat <- do.call(rbind, cipher_cluster)
  
  #KNN No Clusters
  beforeTime <- Sys.time()
  result <- knn(train = train, test = test, cl = train_labels, k = 3)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
  afterTime <- Sys.time()
  NoClusterTime <- afterTime-beforeTime
  #KNN Clusters
  beforeTime <- Sys.time()
  resultCluster <- knn(train = train_dat, test = test, cl = train_lab, k = 3)
  cfcMtxCluster <- confusionMatrix(data = resultCluster, reference = test_labels)
  acc <- sum(diag(cfcMtxCluster$table))/sum(cfcMtxCluster$table)
  resultsClusters <- c(resultsClusters, acc)
  afterTime <- Sys.time()
  ClusterTime <- afterTime - beforeTime

  print("No Cluster Time")
  print(NoClusterTime)
  print("Cluster Time")
  print(ClusterTime)
  }
summary(results)
sd(results)
summary(resultsClusters)
sd(resultsClusters)

#3.1.3
id <- do.call(rbind,idList[1:30])
id <- as.data.frame(id)
trainDataSet <- as.data.frame(id[1:30000,])


train_set <- trainDataSet[1 : (nrow(trainDataSet)),2:ncol(trainDataSet)]
train_labels <- trainDataSet[1 : (nrow(trainDataSet)),1]

testDataSet <- as.data.frame(id[30001:60000,])
test_set <- testDataSet[ 1 : nrow(testDataSet) ,2:ncol(testDataSet)]
test_labels <- testDataSet[  1 : nrow(testDataSet) ,1]


cipher_cluster <- c()
label_cluster <- c()

clusters <- 2000
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# KNN with 2000 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

# KNN with no clustering 
beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))




# 3.2.1
id <- idList[15:16]
trainDataSet <- as.data.frame(id[1])


train_set <- trainDataSet[1 : (nrow(trainDataSet)),2:ncol(trainDataSet)]
train_labels <- trainDataSet[1 : (nrow(trainDataSet)),1]

testDataSet <- as.data.frame(id[2])
test_set <- testDataSet[ 1 : nrow(testDataSet) ,2:ncol(testDataSet)]
test_labels <- testDataSet[  1 : nrow(testDataSet) ,1]

?hclust
data_to_cluster <- c()
for( i in 0:9){
  tmp <- train_set[(i*200+1):(i*200+5),]
  data_to_cluster <- rbind(data_to_cluster,tmp)
}
  
dendr <- hclust(dist(data_to_cluster))
plot(dendr)


#3.2.2

set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()

clusters <- 5
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

dendr <- hclust(dist(train_dat))
plot(dendr,labels=train_lab)


#3.3
id <- do.call(rbind, idList[15:17])
id <- as.data.frame(id)
id$V1 <- factor(id$V1)

#ALLLLL Persons in!
id_shuffle <- id[sample(nrow(id)),]
train <- id_shuffle[0:3000,-1]
test <- id_shuffle[3001:6000,-1]
train_labels <- id_shuffle[0:3000,1]
test_labels <- id_shuffle[3001:6000,1]

# https://stackoverflow.com/a/36843900
f1_score <- function(predicted, expected) {
  # Hacky hack to ensure that we always have at least one NA
  # So we get a square confusion matrix
  predicted[3000] <- NA
  expected[3000] <- NA
  
  predicted <- factor(predicted, exclude=NULL)
  expected  <- factor(expected, exclude=NULL)
  addNA(expected)
  addNA(predicted)
  
  cm <- as.matrix(table(expected, predicted, exclude=NULL))
  print(cm)
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  # We get a precision, recall and f1-score for each factor level
  # So i guess we just avg them? Seems fine to me.
  return(c(mean(precision),mean(recall),mean(f1)))
  
}



accuracydata <- c()
precisondata <- c()
recalldata <- c()
kdata <- c()
ldata <- c()
f1score <- c()
for(i in 0:6){
  k <- i*2+1 
  for(j in 1:k){
    predictions <- knn(train = train, test = test, cl = train_labels, k =k, l = j)
    summary(predictions)
    summary(test_labels)
    #calc prec and recall
    scores <- f1_score(predictions, test_labels)
    #store data in data arrays
    precisondata <- c(precisondata, scores[1])
    recalldata <- c(recalldata, scores[2])
    f1score <- c(f1score, scores[3])
    kdata <- c(kdata,k)
    ldata <- c(ldata,j)
    print(c(k,j))
  }
}

# plot deliciuous data
plot(  recalldata)
plot(x=kdata,  y=recalldata)
plot(  precisondata)
plot(x=recalldata,  y=precisondata)
?plot
plot(0,0,xlim = c(0,1),ylim = c(0.8,1),type = "n",xlab = "recall", ylab = "precision")

cl <- rainbow(6)
prev<-0
for(i in 1:6){
 
  k <- i*2+1
  start <- prev+1
  end <- prev+k
  recData <- recalldata[start:end]
  precData <- precisondata[start:end]
  lines(x=recData,  y=precData,col = cl[i],type = 'b')
  prev <- prev + k
}
legend("topleft", legend = c(3,5,7,9,11,13), col=cl, pch=1)

