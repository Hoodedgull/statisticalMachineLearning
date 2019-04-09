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





#4.1.2 With basic digit data as basis???
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
# Plot a tree built with RevoScaleR
# Construct a model formula
sdNames <- names(segmentationData)
X <- as.vector(sdNames[-c(1,2,3)])
form <- as.formula(paste("Class","~", paste(X,collapse="+")))
# Run the model
rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
                   minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
# Plot the tree						
prp(rxAddInheritance(rx.tree))
fancyRpartPlot(rxAddInheritance(rx.tree))



#4.1.2 With PCA as basis????
id_pca <- prcomp(dataset_shuffle[,-1], center = TRUE, scale = TRUE)

Attributes <- id_pca$x[, 1:5]

train_data <- Attributes[1:3600, ]
test_data <- Attributes[3601:4000, ]

train_label <- dataset_shuffle[1:3600, 1]
test_label <- dataset_shuffle[3601:4000, 1]

?C5.0Control
m <- C5.0(train_data, factor(train_label), control=C5.0Control(noGlobalPruning = FALSE))
summary(m)
plot(m)


#4.1.3 Ish
c5prediction <- predict(m, test_data)
crosstable <- CrossTable(test_label, c5prediction, prop.chisq = FALSE)
crosstable$prop.tbl
sum(diag(crosstable$prop.tbl))
