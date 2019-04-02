library(class)
library(gmodels)
library(caret)
library(C50)
load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
dataset <- as.data.frame(id)
set.seed(123)
dataset_shuffle <- dataset[sample(nrow(dataset)), ]

calcEntropy <- function(inputdata, inputlabels){
  entropy <- 0
  if (nrow(inputdata) == 0){
    return(0)
  }
  for (i in 0:9) {
    cipherdata <- inputdata[(inputlabels == i),]
    p <- nrow(cipherdata)/nrow(inputdata)  
    if (p== 0){
      entropy <- entropy
    }else{
      
    entropy <- entropy + ((-p)*log2(p))
    }
  }
  return(entropy)
}

calcEntropyTwoDatasets <- function(inputdata1, inputlabels1, inputdata2, inputlabels2){
  entropy1 <- calcEntropy(inputdata1, inputlabels1)
  entropy2 <- calcEntropy(inputdata2, inputlabels2)
  proportion1 <- nrow(inputdata1)/(nrow(inputdata1) + nrow(inputdata2))
  proportion2 <- nrow(inputdata2)/(nrow(inputdata1) + nrow(inputdata2))
  
  entSplit <- entropy1 * proportion1 + entropy2 * proportion2
  return(entSplit)
}

calcInformationGain <- function(inputdata1, inputlabels1, inputdata2, inputlabels2, originData, originLabels){
  TotalEntropy <- calcEntropy(originData, originLabels)
  SplitEntropy <- calcEntropyTwoDatasets(inputdata1, inputlabels1, inputdata2, inputlabels2)
  return(TotalEntropy - SplitEntropy)
}

#4.1.1 Dunno what to do
id_pca <- prcomp(dataset_shuffle[, -1], center = TRUE, scale = TRUE)

attributes <- id_pca$x[,1:5]
attributes <- as.data.frame(attributes)
summary(attributes)
attribute_labels <- factor(dataset_shuffle[,1])

ent <- calcEntropy(attributes[,],attribute_labels[])
IG <- c()
DP <- c()
for(res in -100:100){
  step <- res/10
  DP <- c(DP,step)
  inputdata1 <- attributes[attributes$PC2 < step,]
  inputlabels1 <- attribute_labels[attributes$PC2 < step]

  inputdata2 <- attributes[attributes$PC2 >= step,]
  inputlabels2 <- attribute_labels[attributes$PC2 >= step]

  IG <- c(IG,calcInformationGain(inputdata1,inputlabels1,inputdata2, inputlabels2, attributes,attribute_labels))
}

summary(IG)
plot(DP,IG)







#4.1.2 With basic digit data as basis???
train_data <- dataset_shuffle[1:3600,-1]
test_data <- dataset_shuffle[3601:4000,-1]

train_label <- dataset_shuffle[1:3600,1]
test_label <- dataset_shuffle[3601:4000,1]


m <- C5.0(train_data, factor(train_label))
plot(m)

#4.1.2 With PCA as basis????
id_pca <- prcomp(dataset_shuffle[, -1], center = TRUE, scale = TRUE)

Attributes <- id_pca$x[,1:5]

train_data <- Attributes[1:3600,]
test_data <- Attributes[3601:4000,]

train_label <- dataset_shuffle[1:3600,1]
test_label <- dataset_shuffle[3601:4000,1]

m <- C5.0(train_data, factor(train_label))
plot(m)


#4.1.3 Ish
c5prediction <- predict(m,test_data)
crosstable <- CrossTable(test_label,c5prediction,prop.chisq = FALSE)
crosstable$prop.tbl
sum(diag(crosstable$prop.tbl))
