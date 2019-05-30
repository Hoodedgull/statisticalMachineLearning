source('knn_methods.r')
library(class)
library(gmodels)
library(caret)
library(C50)
library("kernlab")


#load("idList-corner-100.Rdata")

load("idList-co-100.Rda")


prepareData <- function(dataset,startRow, endRow){
  id <- do.call(rbind, dataset[startRow:endRow])
  id <- as.data.frame(id)
  id[,1] <- factor(id[,1])
  return(id)
}


###########5.2.1#############################

result <- c()
TreeRuntime <- 0
PredictionRuntime <- 0
dataset <- prepareData(dataset = idList, startRow = 1, endRow = 10)

dataset_shuffled <- shuffleAndSplitData(dataset)

data_Train <- dataset_shuffled$train_set

data_Test <- dataset_shuffled$test_set

form <- as.formula(dataset_shuffled$train_set_labels ~ .)

startTime <- proc.time()

SVM_model <- ksvm(form, data_Train, kernel = "rbfdot", C = 1)

computationTime <<- proc.time() - startTime

TreeRuntime <- computationTime

startTime <- proc.time()

data_Test_prediction <- predict(SVM_model,data_Test, type = "response")

computationTime <<- proc.time() - startTime

PredictionRuntime <- computationTime

cf <- confusionMatrix(dataset_shuffled$test_set_labels, data_Test_prediction)

cf$table

print(c("Precision: " , sum(diag(
  cf$table / sum(cf$table)
))))

###########################################


###########5.2.2#############################

dataset <- prepareData(dataset = idList,
                       startRow = 1,
                       endRow = 2)

kernelModes <- c("rbfdot", "vanilladot", "polydot")

set.seed(2345)

folds <- createFolds(dataset$V1, k = 5)


for (kernelMode in kernelModes) {
  
  for (index in 1:10) {
    cValue <- index * 100
    
    resultsList <- c()
    TreeRuntimeList <- c()
    PredictionRuntimeList <- c()
    
    for (i in 1:10) {
      #Without headers
      data_test <- dataset[folds[[i]],-1]
      data_train <- dataset[-folds[[i]],-1]
      
      #Get the labels
      data_test_labels <-
        dataset[folds[[i]], 1] #Dataset is set in both AllPersonIn and Disjunct
      
      #Get the labels
      data_train_labels <- dataset[-folds[[i]], 1]
      
      ####Compute Decisiont Tree Runtime ############################
      form <- as.formula(data_train_labels ~ .)
      
      startTime <- proc.time()
      
      SVM_model <-
        ksvm(form, data_train, kernel = kernelMode, C = cValue)
      
      computationTime <<- proc.time() - startTime
      
      TreeRuntimeList[i] <- computationTime
      
      #######################################
      
      ####Compute Prediction runtime ############################
      startTime <- proc.time()
      
      data_test_pred <- predict(SVM_model, data_test)
      
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
    
    print(c("The C value: ", cValue))
    
    print(c("The kernel mode: ", kernelMode))
    
    
    #Mean
    print(c("The mean ", mean(resultsList)))
    
    print(c("The Standard Deviation ", sd(resultsList)))
    
    
    print(c(
      "The Support Vector Model Training runtime ",
      mean(TreeRuntimeList)
    ))
    
    
    print(c("The Prediction runtime ", mean(PredictionRuntimeList)))
    
    print("############################################")
    
  }
}

degree <- c(1, 2, 3, 4, 5, 10)
scales <- c(1, 2, 3, 4, 5, 6)
offset <- c(0, 1, 2, 3, 4, 5)
for (x in 1:6) {
  for (index in 1:10) {
    cValue <- index * 0.5
    
    resultsList <- c()
    TreeRuntimeList <- c()
    PredictionRuntimeList <- c()
    
    for (i in 1:5) {
      #Without headers
      data_test <- dataset[folds[[i]],-1]
      data_train <- dataset[-folds[[i]],-1]
      
      #Get the labels
      data_test_labels <-
        dataset[folds[[i]], 1] #Dataset is set in both AllPersonIn and Disjunct
      
      #Get the labels
      data_train_labels <- dataset[-folds[[i]], 1]
      
      ####Compute Decisiont Tree Runtime ############################
      form <- as.formula(data_train_labels ~ .)
      
      startTime <- proc.time()
      arguments <- list(scales[x])
      SVM_model <-
        ksvm(form, data_train, kernel = 'polydot', kpar = arguments, C = cValue)
      
      computationTime <<- proc.time() - startTime
      
      TreeRuntimeList[i] <- computationTime
      
      #######################################
      
      ####Compute Prediction runtime ############################
      startTime <- proc.time()
      
      data_test_pred <- predict(SVM_model, data_test)
      
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
    
    print(c("The C value: ", cValue))
    
    print(c("The kernel paramters: ", arguments))
    print(c("Numbers: ", x))
    
    
    #Mean
    print(c("The mean ", mean(resultsList)))
    
    print(c("The Standard Deviation ", sd(resultsList)))
    
    
    print(c(
      "The Support Vector Model Training runtime ",
      mean(TreeRuntimeList)
    ))
    
    
    print(c("The Prediction runtime ", mean(PredictionRuntimeList)))
    
    print("############################################")
    
  }
}
