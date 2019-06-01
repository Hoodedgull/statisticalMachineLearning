library(class) #For knn
library(tictoc) #For timing
library(caret) #For k-folds

load("idList-cornered-100.Rdata")
data <- as.data.frame(idList[1])
for(i in 2:79){
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

train_data <- data[(0*2000+1):(40*2000),-1]
train_labels <- data[(0*2000+1):(40*2000),1]
test_data <- data[(40*2000+1):(79*2000),-1]
test_labels<- data[(40*2000+1):(79*2000),1]


tic("pca")
pca_result <- prcomp(data[,-1], center = TRUE, scale = TRUE)
toc()

find_index_for_a_pct_of_variance <- function(pca, variance_pct){
  sum <- cumsum(pca$sdev)/sum(pca$sdev)
  for( i in 1:length(pca$sdev)){
    if(sum[i] >= variance_pct/100){
      return(i)
    }
  }
  
}

index <- find_index_for_a_pct_of_variance(pca_result, 70)

train_data_pca <- pca_result$x[(0*2000+1):(40*2000),1:index]
test_data_pca <- pca_result$x[((40*2000)+1):(79*2000),1:index]


tic("knn")
predicted_labels <- knn(train=train_data_pca, test = test_data_pca, cl = train_labels, k = 3)
time <- toc(quiet=TRUE)

acc <- measure_acc(test_labels,predicted_labels)
print(acc)
print(time$toc-time$tic)