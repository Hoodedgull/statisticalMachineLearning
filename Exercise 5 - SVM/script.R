source('knn_methods.r')
library(class)
library(gmodels)
library(caret)
library(C50)
library("kernlab")


#load("idList-corner-100.Rdata")

load("idList-co-100.Rdata")


prepareData <- function(dataset,startRow, endRow){
  id <- do.call(rbind, dataset[startRow:endRow])
  id <- as.data.frame(id)
  id[,1] <- factor(id[,1])
  return(id)
}


###########5.2.1#############################

result <- c()

dataset <- prepareData(dataset = idList, startRow = 1, endRow = 2)

dataset_shuffled <- shuffleAndSplitData(dataset)

data_Train <- dataset_shuffled$train_set

data_Test <- dataset_shuffled$test_set

form <- as.formula(dataset_shuffled$train_set_labels ~ .)

SVM_model <- ksvm(form, data_Train, kernel = "rbfdot", c = 1)


data_Test_prediction <- predict(SVM_model,data_Test, type = "response")


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

folds <- createFolds(dataset$V1, k = 10)


for (kernelMode in kernelModes) {
  
  for (index in 1:10) {
    cValue <- index * 2
    
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