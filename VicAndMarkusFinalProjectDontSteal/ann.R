library(tictoc) #For timing

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

train_data <- data[1:(5*2000),-1]
train_labels <- data[1:(5*2000),1]
test_data <- data[(5*2000+1):(10*2000),-1]
test_labels<- data[(5*2000+1):(10*2000),1]
