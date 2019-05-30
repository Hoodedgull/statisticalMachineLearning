library(class) #For knn
library(tictoc) #For timing


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

train_data <- data[1:(5*2000),-1]
train_labels <- data[1:(5*2000),1]
test_data <- data[(5*2000+1):(10*2000),-1]
test_labels<- data[(5*2000+1):(10*2000),1]

#Find best k
acc_list <- c()
time_list <- c()
for( k in c(1, 2, 3, 5, 7, 10, 15, 20, 30, 50, 75, 100, 150, 200)){
  

tic("knn")
predicted_labels <- knn(train=train_data, test = test_data, cl = train_labels, k = 3)
time <- toc(quiet=TRUE)
acc <- measure_acc(test_labels,predicted_labels)
acc_list <- c(acc_list,acc)
time_list <- c(time_list, time$toc-time$tic)
}

tic("pca")
pca_result <- prcomp(data[,-1], center = TRUE, scale = TRUE)
toc()

#plot(pca_result$sdev[1:100])
#plot(cumsum(pca_result$sdev)/sum(pca_result$sdev))

train_data_pca <- pca_result$x[1:(5*2000),1:65]
test_data_pca <- pca_result$x[((5*2000)+1):(10*2000),1:65]

tic("knn")
predicted_labels <- knn(train=train_data_pca, test = test_data_pca, cl = train_labels, k = 3)
toc()
measure_acc(test_labels,predicted_labels)
