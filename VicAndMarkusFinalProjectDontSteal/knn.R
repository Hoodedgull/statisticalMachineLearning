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

train_data <- data[(10*2000+1):(15*2000),-1]
train_labels <- data[(10*2000+1):(15*2000),1]
test_data <- data[(15*2000+1):(20*2000),-1]
test_labels<- data[(15*2000+1):(20*2000),1]

#Find best k
acc_list <- c()
time_list <- c()
for( k in c(1,2,3,5,7,11,21,31,51,75, 101, 151, 201,503)){
  

tic("knn")
predicted_labels <- knn(train=train_data, test = test_data, cl = train_labels, k = k)
time <- toc(quiet=TRUE)
acc <- measure_acc(test_labels,predicted_labels)
acc_list <- c(acc_list,acc)
time_list <- c(time_list, time$toc-time$tic)
}

plot(x=c(1,2,3,5,7,11,21,31,51,75, 101, 151, 201,503), y=c(acc_list), type = "o")
plot(x=c(1, 2, 3, 5, 7, 10, 15, 20, 30, 50, 75, 100, 150, 200), y=c(time_list))
lapply(acc_list,print)
lapply(time_list,print)
tic("pca")
pca_result <- prcomp(data[,-1], center = TRUE, scale = TRUE)
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
acc_list <- c()
time_list <- c()
for( var in c(10,20,30,40,50,60,70,80,90,99,100)){
  index <- find_index_for_a_pct_of_variance(pca_result, var)
  train_data_pca <- pca_result$x[(10*2000+1):(15*2000),1:index]
  test_data_pca <- pca_result$x[((15*2000)+1):(20*2000),1:index]
  
  tic("knn")
  predicted_labels <- knn(train=train_data_pca, test = test_data_pca, cl = train_labels, k = 3)
  time <- toc(quiet=TRUE)
  acc <- measure_acc(test_labels,predicted_labels)
  acc_list <- c(acc_list,acc)
  time_list <- c(time_list, time$toc-time$tic)
}


plot(x=c(10,20,30,40,50,60,70,80,90,99,100), y=c(acc_list), type = "o")
