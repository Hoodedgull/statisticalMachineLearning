library(tictoc) #For timing
library(RSNNS) #For nn

load("idList-cornered-100.Rdata")
data <- as.data.frame(idList[1])
for (i in 2:79) {
  aPerson <- as.data.frame(idList[i])
  data <- rbind(data, aPerson)
  
}

measure_acc <- function(true, predicted) {
  count = 0
  for (i in 1:length(true)) {
    if (true[i] == predicted[i]) {
      count <- count + 1
    }
  }
  
  return(count / length(true))
  
}

nnacc_from_william <- function(predictions, test_labels){
  
  ################The bottom part of williams stuff
  responselist <-
    matrix(nrow = length(predictions[, 1]),
           ncol = 1,
           data = "Na")
  
  for (i in 1:nrow(predictions)) {
    responselist[i, ] <-
      toString(which(predictions[i, ] == max(predictions[i, ])) - 1)
  }
  responselist <- data.frame(responselist)
  responselist[, 1] <- as.factor(responselist[, 1])
  
  # Calculating the accuracy
  agreement_rbf <- responselist[, 1] == test_labels
  table(agreement_rbf)
  acc <- prop.table(table(agreement_rbf))[2]
  return(acc)
}

train_data <- data[(10 * 2000 + 1):(15 * 2000), -1]
train_labels <- data[(10 * 2000 + 1):(15 * 2000), 1]
test_data <- data[(15 * 2000 + 1):(20 * 2000), -1]
test_labels <- data[(15 * 2000 + 1):(20 * 2000), 1]


################## The top part of williams stuff
lev <- levels(as.factor(data$X1)) # Number of classes?levels

nnTrainingClass <-
  matrix(nrow = length(data$X1),
         ncol = 10,
         data = 0) # Create a list probabilities, for all labels

for (i in 1:length(data$X1)) {
  # Set probabilities to one for matching class
  matchList <- match(lev, toString(data$X1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i, ] <- matchList
}

trainingClass <- as.data.frame(nnTrainingClass)
summary(trainingClass[1, ])
######################### Real stuff

###################################################################Find best num of neurons
acc_list <- c()
tacc_list <- c()
ttime_list <- c()
ptime_list <- c()
for (neurons in c(1, 5, 10, 25, 50, 100)) {
 tic("train")
   nn <-
    mlp(
      x = train_data,
      y = trainingClass[(15 * 2000 + 1):(20 * 2000), ],
      size = c(neurons), 
      maxit = 100,
      initFunc = "Randomize_Weights",
      initFuncParams = c(-0.3, 0.3),
      learnFunc = "Std_Backpropagation",
      learnFuncParams = c(0.01, 0),
      updateFunc = "Topological_Order",
      updateFuncParams = c(0),
      hiddenActFunc = "Act_Logistic",
      shufflePatterns = TRUE,
      linOut = FALSE,
      inputsTest = NULL,
      targetsTest = NULL,
      pruneFunc = NULL,
      pruneFuncParams = NULL
    )
  ttime <-  toc()
  ttime_list <- c(ttime_list,ttime)
  tic("predict")
  predictions <- predict(nn, test_data)
  ptime <- toc()
  ptime_list <- c(ptime_list,ptime)
  
  acc <- nnacc_from_william(predictions, test_labels)
  acc_list <- c(acc_list,acc)
  
  tpredictions <- predict(nn, train_data)
  tacc <- nnacc_from_william(tpredictions, train_labels)
  tacc_list <- c(tacc_list,tacc)
}

###################################################################Find best num of epochs

acc_list2 <- c()
tacc_list2 <- c()
ttime_list2 <- c()
ptime_list2 <- c()
for (epochs in c(1, 5, 10, 25, 50, 100, 200, 500, 1000)) {
  tic("train")
  nn <-
    mlp(
      x = train_data,
      y = trainingClass[(15 * 2000 + 1):(20 * 2000), ],
      size = c(50), 
      maxit = epochs,
      initFunc = "Randomize_Weights",
      initFuncParams = c(-0.3, 0.3),
      learnFunc = "Std_Backpropagation",
      learnFuncParams = c(0.01, 0),
      updateFunc = "Topological_Order",
      updateFuncParams = c(0),
      hiddenActFunc = "Act_Logistic",
      shufflePatterns = TRUE,
      linOut = FALSE,
      inputsTest = NULL,
      targetsTest = NULL,
      pruneFunc = NULL,
      pruneFuncParams = NULL
    )
  ttime <-  toc()
  ttime_list2 <- c(ttime_list,ttime)
  tic("predict")
  predictions <- predict(nn, test_data)
  ptime <- toc()
  ptime_list2 <- c(ptime_list,ptime)
  
  acc <- nnacc_from_william(predictions, test_labels)
  acc_list2 <- c(acc_list,acc)
  
  tpredictions <- predict(nn, train_data)
  tacc <- nnacc_from_william(tpredictions, train_labels)
  tacc_list2 <- c(tacc_list,tacc)
}


###################################################################Find best lr

acc_list3 <- c()
tacc_list3 <- c()
ttime_list3 <- c()
ptime_list3 <- c()
for (lr in c(0.0001, 0.001, 0.01,0,05, 0.1,0.25, 0.5, 0.75, 1,2)) {
  tic("train")
  nn <-
    mlp(
      x = train_data,
      y = trainingClass[(15 * 2000 + 1):(20 * 2000), ],
      size = c(50), 
      maxit = epochs,
      initFunc = "Randomize_Weights",
      initFuncParams = c(-0.3, 0.3),
      learnFunc = "Std_Backpropagation",
      learnFuncParams = c(0.01, 0),
      updateFunc = "Topological_Order",
      updateFuncParams = c(0),
      hiddenActFunc = "Act_Logistic",
      shufflePatterns = TRUE,
      linOut = FALSE,
      inputsTest = NULL,
      targetsTest = NULL,
      pruneFunc = NULL,
      pruneFuncParams = NULL
    )
  ttime <-  toc()
  ttime_list3 <- c(ttime_list,ttime)
  tic("predict")
  predictions <- predict(nn, test_data)
  ptime <- toc()
  ptime_list3 <- c(ptime_list,ptime)
  
  acc <- nnacc_from_william(predictions, test_labels)
  acc_list3 <- c(acc_list,acc)
  
  tpredictions <- predict(nn, train_data)
  tacc <- nnacc_from_william(tpredictions, train_labels)
  tacc_list3 <- c(tacc_list,tacc)
}