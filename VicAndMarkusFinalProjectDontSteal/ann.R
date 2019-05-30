library(tictoc) #For timing
library(RSNNS) #For nn

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

?rsnns.learnFuncParams
?rsnnsObjectFactory


################## The top part of williams stuff
lev <- levels(as.factor(data$X1)) # Number of classes?levels

nnTrainingClass <- matrix(nrow = length(data$X1), ncol = 10, data = 0) # Create a list probabilities, for all labels

for(i in 1:length(data$X1)) { # Set probabilities to one for matching class
  matchList <- match(lev,toString(data$X1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}

trainingClass <- as.data.frame(nnTrainingClass)
summary(trainingClass[1,])
######################### Real stuff
nn <- mlp(x=train_data, y=trainingClass[1:(5*2000),], size = c(5), maxit = 500,
          initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
          learnFunc = "Std_Backpropagation", learnFuncParams = c(0.5, 0),
          updateFunc = "Topological_Order", updateFuncParams = c(0),
          hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE,
          inputsTest = NULL, targetsTest = NULL, pruneFunc = NULL,
          pruneFuncParams = NULL)
predictions <- predict(nn, test_data)
summary(predictions)

################The bottom part of williams stuff
responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")

for(i in 1:nrow(predictions)) {
  responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1])

# Calculating the accuracy
agreement_rbf <- responselist[,1] == test_labels
table(agreement_rbf)
prop.table(table(agreement_rbf))

