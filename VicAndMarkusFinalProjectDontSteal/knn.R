library(class) #For knn
library(tictoc) #For timing

load("idList-cornered-100.Rdata")
data <- as.data.frame(idList[1])
for(i in 2:10){
  aPerson <- as.data.frame(idList[i])
  data <- rbind(data,aPerson)
  
}

measure_acc <- function(true, predicted){
  count = 0
  for( i in 1:length(true)){
    if(true[i] == predicted[i]){
      count <- count +1
    }
  }
  
  return(count/length(true))
  
}
#shuffle
split_index <- (5*2000)
data_shuffled <- data[sample(nrow(data)),]
train_data <- data_shuffled[1:split_index,-1]
train_labels <- data_shuffled[1:split_index,1]
test_data <- data_shuffled[(split_index+1):(split_index*2),-1]
test_labels<- data_shuffled[(split_index+1):(split_index*2),1]

#Find best k
acc_list <- c()
time_list <- c()
klist <- c(1, 3, 5, 7, 11, 15, 21, 31, 51, 75, 101, 151, 201)
for( k in c(1, 3, 5, 7, 11, 15, 21, 31, 51, 75, 101, 151, 201)){
  
print("looping")
tic("knn")
predicted_labels <- knn(train=train_data, test = test_data, cl = train_labels, k = k)
time <- toc(quiet=TRUE)
acc <- measure_acc(test_labels,predicted_labels)
acc_list <- c(acc_list,acc)
time_list <- c(time_list, time$toc-time$tic)
}

plot(klist,acc_list, type = "o")

tic("pca")
pca_result <- prcomp(data_shuffled[,-1], center = TRUE, scale = TRUE)
toc()

#plot(pca_result$sdev[1:100])
plot(cumsum(pca_result$sdev)/sum(pca_result$sdev))

find_index_for_a_pct_of_variance <- function(pca, variance_pct){
  sum <- cumsum(pca$sdev)/sum(pca$sdev)
  for( i in 1:length(pca$sdev)){
    if(sum[i] >= variance_pct/100){
      return(i)
    }
  }
  
}



#Find best pca
acc_listPCA <- c()
time_listPCA <- c()
for( var in c(10,20,30,40,50,60,70,80,90,99)){
  index <- find_index_for_a_pct_of_variance(pca_result, var)
  print(index)
  train_data_pca <- pca_result$x[1:(split_index),1:index]
  test_data_pca <- pca_result$x[(split_index+1):(split_index*2),1:index]
  print(var)
  tic("knn")
  predicted_labels <- knn(train=train_data_pca, test = test_data_pca, cl = train_labels, k = 3)
  time <- toc(quiet=TRUE)
  acc <- measure_acc(test_labels,predicted_labels)
  acc_listPCA <- c(acc_listPCA,acc)
  time_listPCA <- c(time_listPCA, time$toc-time$tic)
}

plot(c(10,20,30,40,50,60,70,80,90,99), acc_listPCA, type = "o" )
plot(c(10,20,30,40,50,60,70,80,90,99), time_listPCA, type = "o" )

#### Cross Validation
library(class)
library(gmodels)
library(caret)
time_listFolds <- c()
folds <- createFolds(data_shuffled$X1, k = 10)
results = c()
for(i in 1:10){
  print(i)
  train <- data_shuffled[-folds[[i]],-1]
  test <- data_shuffled[folds[[i]],-1]
  
  train_labels <- data_shuffled[-folds[[i]],1] 
  test_labels <- data_shuffled[folds[[i]],1] 
  tic("knnFold")
  result <- knn(train = train, test = test, cl = train_labels, k = 5)
  time <- toc()
  time_listFolds <- c(time_listFolds, time$toc-time$tic)
  cfcMtx <- confusionMatrix(factor(result, levels = 0:9), factor(test_labels, levels = 0:9))
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
summary(results)
sd(results)