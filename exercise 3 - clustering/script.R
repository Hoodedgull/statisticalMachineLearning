library(class)
library(gmodels)
library(caret)
load("idList-corner-300.Rdata")
id <- idList[15:16]
trainDataSet <- as.data.frame(id[1])


train_set <- trainDataSet[1 : (nrow(trainDataSet)),2:ncol(trainDataSet)]
train_labels <- trainDataSet[1 : (nrow(trainDataSet)),1]

testDataSet <- as.data.frame(id[2])
test_set <- testDataSet[ 1 : nrow(testDataSet) ,2:ncol(testDataSet)]
test_labels <- dataset[  1 : nrow(testDataSet) ,1]

# id_mat <- data.matrix(df, rownames.force = NA)
# 
# rotate <- function(x)
#   t(apply(x, 2, rev))
# imageSize <- sqrt(ncol(id_mat) - 1)
# # Show first 10 images
# for (i in 1:10)
# {
#   rotated <- c(id_mat[-200 + i * 200 + 1, 2:ncol(id_mat)])
#   rotated <-
#     ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
#   
#   image <-
#     matrix(rotated,
#            nrow = imageSize,
#            ncol = imageSize,
#            byrow = FALSE)
#   image <- rotate(image)
#   image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
#  
# }

set.seed(2345)

cipher_cluster <- c()
label_cluster <- c()

clusters <- 199
for( i in 0:9) {
  single_cipher_data <- train_set[ train_labels == i ,]
  clusterData <- kmeans(single_cipher_data, clusters)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:clusters)*0 + i
}

train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)

# 
# id_mat <- data.matrix(train_dat, rownames.force = NA)
# 
# rotate <- function(x)
#   t(apply(x, 2, rev))
# imageSize <- sqrt(ncol(id_mat) - 1)
# # Show first 10 images
# for (i in 1:10)
# {
#   rotated <- c(id_mat[-200 + i * 200 + 1, 2:ncol(id_mat)])
#   rotated <-
#     ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
# 
#   image <-
#     matrix(rotated,
#            nrow = imageSize,
#            ncol = imageSize,
#            byrow = FALSE)
#   image <- rotate(image)
#   image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
# 
# }

# KNN with no clustering 
beforeTime <- Sys.time()
res <- knn(train = train_set,test = test_set,cl = train_labels,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))


# KNN with 50 clusters
beforeTime <- Sys.time()
res <- knn(train = train_dat,test = test_set,cl = train_lab,k = 3)
afterTime <- Sys.time()
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
afterTime-beforeTime
sum(diag(table$prop.tbl))