library(class)
library(gmodels)
library(caret)

#This loads on mac
load("~/Desktop/TradingMarketGit/statisticalMachineLearning/Excercise 1 - KNN/id100.Rda")

# 1.4.1 
set.seed(123)
dataset <- id
dataset_shuffle <- dataset[sample(nrow(dataset)),]


train_set <- dataset_shuffle[1 : (nrow(dataset_shuffle)/2),2:ncol(dataset_shuffle)]
train_labels <- dataset_shuffle[1 : (nrow(dataset_shuffle)/2),1]
test_set <- dataset_shuffle[ (nrow(dataset_shuffle)/2 + 1) : nrow(dataset_shuffle) ,2:ncol(dataset_shuffle)]
test_labels <- dataset_shuffle[ (nrow(dataset_shuffle)/2 + 1) : nrow(dataset_shuffle) ,1]

beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 5)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

# 1.4.2
beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 1)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 5)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 21)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 133)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))

#1.4.3

folds <- createFolds(dataset$X1, k = 10)

results = c()
for(i in 1:10){
  train <- dataset[-folds[[i]],-1]
  test <- dataset[folds[[i]],-1]
  
  train_labels <- dataset[-folds[[i]],1] 
  test_labels <- dataset[folds[[i]],1] 
  
  result <- knn(train = train, test = test, cl = train_labels, k = 21)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
summary(results)
sd(results)

# 1.4.4
#idList <- load(file = "idList-co-100.Rda")
load("~/Desktop/TradingMarketGit/statisticalMachineLearning/Excercise 1 - KNN/idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id$V1 <- factor(id$V1)

#ALLLLL Persons in!
id_shuffle <- id[sample(nrow(id)),]
train <- id_shuffle[0:10000,-1]
test <- id_shuffle[10001:40000,-1]
train_labels <- id_shuffle[0:10000,1]
test_labels <- id_shuffle[10001:40000,1]

prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
cfcMtx1 <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
cfcMtx1


# Disjunct 

train <- id[0:10000,-1]
test <- id[10001:40000,-1]
train_labels <- id[0:10000,1]
test_labels <- id[10001:40000,1]

prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
cfcMtx2 <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
cfcMtx2

# 1.4.5


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

train <- id_shuffle[0:1000,-1]
test <- id_shuffle[1001:4000,-1]
train_labels <- id_shuffle[0:1000,1]
test_labels <- id_shuffle[1001:4000,1]

"Sample size: Train 1000, Test = 3000, k = 1"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 1)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"Sample size: Train 1000, Test = 3000, k = 5"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 5)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc

"Sample size: Train 1000, Test = 3000, k = 21"
beforeTime <- Sys.time()
prediction <- knn(train = train, test = test, cl = train_labels, k= 21)
afterTime <- Sys.time()
afterTime-beforeTime
cfcMtx <- confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
acc
