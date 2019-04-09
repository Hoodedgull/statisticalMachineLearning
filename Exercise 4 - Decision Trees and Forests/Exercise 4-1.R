source('knn_methods.r')
library(class)
library(gmodels)
library(caret)
library(C50)
load("idList-co-100.Rdata")


prepareData <- function(dataset,startRow, endRow){
  id <- do.call(rbind, dataset[startRow:endRow])
  id <- as.data.frame(id)
  id[,1] <- factor(id[,1])
  return(id)
}




#4.1.2 With basic digit data as basis???

dataset <- prepareData(dataset = idList, startRow = 1, endRow = 2) 


dataset_shuffle <- splitDatasetInHalf(dataset)


m <- C5.0(dataset_shuffle$train_set, factor(dataset_shuffle$train_set_labels))
plot(m)


#4.1.2 With PCA as basis????

dataset <- prepareData(dataset = idList, startRow = 1, endRow = 2) 

dataset_shuffle <- splitDatasetInHalf(dataset)

# Run PCA on the training set
id_pca <- prcomp(dataset_shuffle$train_set, center = TRUE, scale = TRUE)


#Select the first five PCAs
Attributes <- id_pca$x[,1:5]

train_data <- dataset_shuffle$train_set
test_data <- dataset_shuffle$test_set

train_label <- dataset_shuffle$train_set_labels
test_label <- dataset_shuffle$test_set_labels

m <- C5.0(train_data, factor(train_label))

plot(m)



#4.1.3 Ish

dataset <- prepareData(dataset = idList, startRow = 1, endRow = 2) 

set.seed(2345)

folds <- createFolds(dataset$V1, k = 10)

resultsList <- c()
TreeRuntimeList <- c()
PredictionRuntimeList <- c()


for (i in 1:10) {
  
  #Without headers
  data_test <- dataset[folds[[i]],-1]
  data_train <- dataset[-folds[[i]],-1]
  
  #Get the labels
  data_test_labels <- dataset[folds[[i]], 1] #Dataset is set in both AllPersonIn and Disjunct
  
  #Get the labels
  data_train_labels <- dataset[-folds[[i]], 1]
  
  ####Compute ############################
  startTime <- proc.time()
  
  m <- C5.0(data_train, factor(data_train_labels))
  
  computationTime <<- proc.time() - startTime
  
  TreeRuntimeList[i] <- computationTime
  
  #######################################
  
  ####Compute ############################
  startTime <- proc.time()
  
  data_test_pred <- predict(m, data_test)
  
  computationTime <<- proc.time() - startTime
  
  PredictionRuntimeList[i] <- computationTime
  
  #######################################
  
  # Matrix
  cf <- confusionMatrix(data_test_labels, data_test_pred)
  
  print(c("Precision: ", i , sum(diag(
    cf$table / sum(cf$table)
  ))))
  
  resultsList[i] <- sum(diag(cf$table / sum(cf$table)))
}



#Mean
print(c("The mean ", mean(resultsList)))

#Standard Deviation
print(c("The Standard Deviation ", sd(resultsList)))

#K-Means Mean
print(c("The Decision Tree Training runtime ", mean(TreeRuntimeList)))

#K-NN Mean
print(c("The Prediction runtime ", mean(PredictionRuntimeList)))

