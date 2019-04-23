library(class)
library(gmodels)
library(caret)
library(C50)
load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
dataset <- as.data.frame(id)
set.seed(123)
dataset_shuffle <- dataset[sample(nrow(dataset)),]

calcEntropy <- function(inputdata, inputlabels) {
  entropy <- 0
  if (nrow(inputdata) == 0) {
    return(0)
  }
  for (i in 0:9) {
    cipherdata <- inputdata[(inputlabels == i), ]
    p <- nrow(cipherdata) / nrow(inputdata)
    if (p == 0) {
      entropy <- entropy
    } else{
      entropy <- entropy + ((-p) * log2(p))
    }
  }
  return(entropy)
}

calcEntropyTwoDatasets <-
  function(inputdata1,
           inputlabels1,
           inputdata2,
           inputlabels2) {
    entropy1 <- calcEntropy(inputdata1, inputlabels1)
    entropy2 <- calcEntropy(inputdata2, inputlabels2)
    proportion1 <-
      nrow(inputdata1) / (nrow(inputdata1) + nrow(inputdata2))
    proportion2 <-
      nrow(inputdata2) / (nrow(inputdata1) + nrow(inputdata2))
    
    entSplit <- entropy1 * proportion1 + entropy2 * proportion2
    return(entSplit)
  }

calcInformationGain <-
  function(inputdata1,
           inputlabels1,
           inputdata2,
           inputlabels2,
           originData,
           originLabels) {
    TotalEntropy <- calcEntropy(originData, originLabels)
    SplitEntropy <-
      calcEntropyTwoDatasets(inputdata1, inputlabels1, inputdata2, inputlabels2)
    return(TotalEntropy - SplitEntropy)
  }

#4.1.1 Dunno what to do
id_pca <- prcomp(dataset_shuffle[,-1], center = TRUE, scale = TRUE)

attributes <- id_pca$x[, 1:5]
attributes <- as.data.frame(attributes)
summary(attributes)
attribute_labels <- factor(dataset_shuffle[, 1])

plot(x=0,y=0, xlim= c(-10,10),ylim=c(0,0.5), type='n')
colors = rainbow(5)
for (pca in 1:5) {
  ent <- calcEntropy(attributes[, ], attribute_labels[])
  IG <- c()
  DP <- c()
  for (res in-100:100) {
    step <- res / 10
    DP <- c(DP, step)
    inputdata1 <- attributes[attributes[pca] < step, ]
    inputlabels1 <- attribute_labels[attributes[pca] < step]
    
    inputdata2 <- attributes[attributes[pca] >= step, ]
    inputlabels2 <- attribute_labels[attributes[pca] >= step]
    
    IG <-
      c(
        IG,
        calcInformationGain(
          inputdata1,
          inputlabels1,
          inputdata2,
          inputlabels2,
          attributes,
          attribute_labels
        )
      )
  }
  
  summary(IG)
  lines(DP, IG,col=colors[pca])
  
}
legend(x = -10,y=0.5,legend = c("pc1","pc2","pc3","pc4","pc5"),col = colors,lty = 1)





#4.1.2 With PCA data as basis
dataset <- as.data.frame(id)
set.seed(123)
dataset_shuffle <- dataset[sample(nrow(dataset)),]

id_pca <- prcomp(dataset_shuffle[,-1], center = TRUE, scale = TRUE)

attributes <- id_pca$x[, 1:5]
attributes <- as.data.frame(attributes)
summary(attributes)
attribute_labels <- factor(dataset_shuffle[, 1])

train_data <- attributes[1:3600, ]
test_data <- attributes[3601:4000, ]

train_label <- attribute_labels[1:3600]
test_label <- attribute_labels[3601:4000]

trainDataWithLabel <- cbind(train_label,train_data)


# Plotting Classification Trees with the plot.rpart and rattle pckages

library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. 


# Make big tree
form <- as.formula(train_label ~ .)
#-------------------------------------------------------------------
tree.2 <- rpart(form,trainDataWithLabel)			# A more reasonable tree
prp(tree.2,extra=1, tweak=1.5, fallen.leaves = FALSE, under = TRUE)                                     # A fast plot													
?fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
#-------------------------------------------------------------------





#4.1.3
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
  data_test_labels <- factor(dataset[folds[[i]], 1])
  
  #Get the labels
  data_train_labels <- factor(dataset[-folds[[i]], 1])
  
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

print(c("The Decision Tree Training runtime ", mean(TreeRuntimeList)))

print(c("The Prediction runtime ", mean(PredictionRuntimeList)))



##################################################################
##################################################################
##################################################################
##################################################################
#4.2.1
library(randomForest)
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
  data_test_labels <- factor(dataset[folds[[i]], 1])
  
  #Get the labels
  data_train_labels <- factor(dataset[-folds[[i]], 1])
  
  ####Compute ############################
  startTime <- proc.time()
  
  ###CREATE RANDOM FOREST###
  m <- randomForest(data_train, data_train_labels, ntree = 3)
  
  computationTime <<- proc.time() - startTime
  
  TreeRuntimeList[i] <- computationTime
  
  #######################################
  
  ####Compute ############################
  startTime <- proc.time()
  
  ###PREDICT WITH RANDOM FOREST###
  data_test_pred <- predict(m, data_test, type = "response")
  
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

print(c("The Random Forest Generation runtime ", mean(TreeRuntimeList)))

print(c("The Prediction runtime ", mean(PredictionRuntimeList)))

####################################
####################################
####################################
  library(randomForest)
  set.seed(2345)
  
  
  folds <- createFolds(dataset$V1, k = 10)
  
  resultsList <- c()
  TreeRuntimeList <- c()
  PredictionRuntimeList <- c()
  
  maxnodes <- 10000
  for (i in 1:10) {
    
    #Without headers
    data_test <- dataset[folds[[i]],-1]
    data_train <- dataset[-folds[[i]],-1]
    
    #Get the labels
    data_test_labels <- factor(dataset[folds[[i]], 1])
    
    #Get the labels
    data_train_labels <- factor(dataset[-folds[[i]], 1])
    
    ####Compute ############################
    startTime <- proc.time()
    
    ###CREATE RANDOM FOREST###
    m <- randomForest(data_train, data_train_labels, ntree = 10)
    
    computationTime <<- proc.time() - startTime
    
    TreeRuntimeList[i] <- computationTime
    
    #######################################
    
    ####Compute ############################
    startTime <- proc.time()
    
    ###PREDICT WITH RANDOM FOREST###
    data_test_pred <- predict(m, data_test, type = "response")
    
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
  
  print(c("The Random Forest Generation runtime ", mean(TreeRuntimeList)))
  
  print(c("The Prediction runtime ", mean(PredictionRuntimeList)))


