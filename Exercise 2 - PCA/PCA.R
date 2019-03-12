load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[, 1] <- factor(id[, 1])

set.seed(123)
id_shuffle <- id[sample(nrow(id)), ]

id_pca <- prcomp(id_shuffle[, -1], center = TRUE, scale = TRUE)

totalvar = sum(id_pca$sdev)

#2.1.1
plot(id_pca$sdev[1:20],
     main = "standard deviation (from eigenvalues)",
     ylab = "std. dev.",
     xlab = "Principal component")
plot(
  id_pca$sdev[1:20] / totalvar,
  main = "Proportion of variance",
  xlab = "Principal component",
  ylab = "proportion of total variance explained"
)
plot(
  cumsum(id_pca$sdev / totalvar),
  main = "Cumulative sum of proportions of variance",
  xlab = "Principal Component",
  ylab = "cumulative sum"
)


#2.1.2
library(class)
library(gmodels)
library(caret)
cumsum(id_pca$sdev / totalvar)


indexFor80pct <- 72
indexFor90pct <- 115
indexFor95pct <- 157
indexFor99pct <- 232

data <- id_pca$x[, 1:indexFor80pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id_shuffle[0:10000, 1]
test_labels <- id_shuffle[10001:40000, 1]

" 80 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"80 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"80 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc



data <- id_pca$x[, 1:indexFor90pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id_shuffle[0:10000, 1]
test_labels <- id_shuffle[10001:40000, 1]

" 90 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"90 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"90 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc


data <- id_pca$x[, 1:indexFor95pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id_shuffle[0:10000, 1]
test_labels <- id_shuffle[10001:40000, 1]

" 95 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"95 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"95 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc



data <- id_pca$x[, 1:indexFor99pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id_shuffle[0:10000, 1]
test_labels <- id_shuffle[10001:40000, 1]

"99 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"99 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"99 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc


# disjunct

id_pca_disj <- prcomp(id[, -1], center = TRUE, scale = TRUE)

totalvar_disj = sum(id_pca_disj$sdev)

#2.1.1
plot(id_pca_disj$sdev[1:20],
     main = "standard deviation (from eigenvalues)",
     ylab = "std. dev.",
     xlab = "Principal component")
plot(
  id_pca_disj$sdev[1:20] / totalvar_disj,
  main = "Proportion of variance",
  xlab = "Principal component",
  ylab = "proportion of total variance explained"
)
plot(
  cumsum(id_pca_disj$sdev / totalvar),
  main = "Cumulative sum of proportions of variance",
  xlab = "Principal Component",
  ylab = "cumulative sum"
)


indexFor80pct <- 72
indexFor90pct <- 115
indexFor95pct <- 157
indexFor99pct <- 232


data <- id_pca_disj$x[, 1:indexFor80pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id[0:10000, 1]
test_labels <- id[10001:40000, 1]

" 80 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc


"80 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"80 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

data <- id_pca_disj$x[, 1:indexFor90pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id[0:10000, 1]
test_labels <- id[10001:40000, 1]



" 90 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"90 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"90 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

data <- id_pca_disj$x[, 1:indexFor95pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id[0:10000, 1]
test_labels <- id[10001:40000, 1]



" 95 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"95 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"95 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

data <- id_pca_disj$x[, 1:indexFor99pct]

train <- as.data.frame(data[0:10000, ])
test <- as.data.frame(data[10001:40000, ])
train_labels <- id[0:10000, 1]
test_labels <- id[10001:40000, 1]

"99 pct k = 1"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 1
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"99 pct k = 5"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 5
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

"99 pct k = 21"
beforeTime <- Sys.time()
prediction <-
  knn(
    train = train,
    test = test,
    cl = train_labels,
    k = 21
  )
afterTime <- Sys.time()
afterTime - beforeTime
cfcMtx <-
  confusionMatrix(data = prediction, reference = test_labels)
acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
acc

# 2.2

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
  
}

#No normalization

id_pca <-
  prcomp(id_shuffle[, -1], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[, 1:indexFor80pct]


folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for (i in 1:10) {
  train <- data[-folds[[i]], -1]
  test <- data[folds[[i]], -1]
  
  train_labels <- id_shuffle[-folds[[i]], 1]
  test_labels <- id_shuffle[folds[[i]], 1]
  
  result <-
    knn(
      train = train,
      test = test,
      cl = train_labels,
      k = 1
    )
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
  results <- c(results, acc)
}
#No normalization
summary(results)
sd(results)

#BEFORE PCA normalization
id_shuffle_norm <- lapply(id_shuffle[, -1], normalize)
id_shuffle_norm <- as.data.frame(id_shuffle_norm)
id_pca <-
  prcomp(id_shuffle_norm[], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[, 1:indexFor80pct]


folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for (i in 1:10) {
  train <- data[-folds[[i]], -1]
  test <- data[folds[[i]], -1]
  
  train_labels <- id_shuffle[-folds[[i]], 1]
  test_labels <- id_shuffle[folds[[i]], 1]
  
  result <-
    knn(
      train = train,
      test = test,
      cl = train_labels,
      k = 1
    )
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
  results <- c(results, acc)
}
#BEFORE PCA normalization
summary(results)
sd(results)

#AFTER PCA normalization
id_pca <-
  prcomp(id_shuffle[, -1], center = TRUE) # maybe remove scale=TRUE
data <- id_pca$x[, 1:indexFor80pct]
data <- as.data.frame(data)
data <- lapply(data, normalize)
data <- as.data.frame(data)

folds <- createFolds(id_shuffle$V1, k = 10)

results = c()
for (i in 1:10) {
  train <- data[-folds[[i]], -1]
  test <- data[folds[[i]], -1]
  
  train_labels <- id_shuffle[-folds[[i]], 1]
  test_labels <- id_shuffle[folds[[i]], 1]
  
  result <-
    knn(
      train = train,
      test = test,
      cl = train_labels,
      k = 1
    )
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table)) / sum(cfcMtx$table)
  results <- c(results, acc)
}
#AFTER PCA normalization
summary(results)
sd(results)

#2.4.1
smoothImage <- function(grayImg) {
  smoothed <-
    as.matrix(blur(
      as.im(grayImg),
      sigma = 0.5,
      normalise = FALSE,
      bleed = TRUE,
      varcov = NULL
    ))
  return(smoothed)
  
}


id_mat <- data.matrix(id, rownames.force = NA)

rotate <- function(x)
  t(apply(x, 2, rev))
imageSize <- sqrt(ncol(id_mat) - 1)
# Show first 10 images
for (i in 1:10)
{
  rotated <- c(id_mat[-400 + i * 400 + 1, 2:ncol(id_mat)])
  rotated <-
    ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  
  image <-
    matrix(rotated,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
  #image <- smoothImage(image)
  #image(image, zlim=c(0,1),col=gray(0:100/100))
}


# 2.4.2
id_pca <- prcomp(id[, -1], center = TRUE, scale = TRUE)

for (i in 1:10) {
  eigen <- id_pca$rotation[, i]
  eigen <- ((eigen - min(eigen)) / (max(eigen) - min(eigen)))
  image <-
    matrix(eigen,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
  
}


#2.4.3

id_pca <- prcomp(id[, -1], center = TRUE, scale = TRUE)

for (cipherNumber in 1:10) {
  trunc <- id_pca$x[-400 + cipherNumber * 400 + 1, ] %*%
    t(id_pca$rotation[, ])
  trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
  trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))
  
  image <-
    matrix(trunc,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
  
  
}

# 2.4.4
id_pca <- prcomp(id[, -1], center = TRUE, scale = TRUE)
cumsum(id_pca$sdev / totalvar)
#80 percent variance
for (cipherNumber in 1:10) {
  trunc <- id_pca$x[-400 + cipherNumber * 400 + 1, 1:indexFor80pct] %*%
    t(id_pca$rotation[, 1:indexFor80pct])
  trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
  trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))
  
  image <-
    matrix(trunc,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
}

#90 percent variance
for (cipherNumber in 1:10) {
  trunc <- id_pca$x[-400 + cipherNumber * 400 + 1, 1:indexFor90pct] %*%
    t(id_pca$rotation[, 1:indexFor90pct])
  trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
  trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))
  
  image <-
    matrix(trunc,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
}

#95 percent variance
for (cipherNumber in 1:10) {
  trunc <- id_pca$x[-400 + cipherNumber * 400 + 1, 1:indexFor95pct] %*%
    t(id_pca$rotation[, 1:indexFor95pct])
  trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
  trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))
  
  image <-
    matrix(trunc,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
}

#99 percent variance
for (cipherNumber in 1:10) {
  trunc <- id_pca$x[-400 + cipherNumber * 400 + 1, 1:indexFor99pct] %*%
    t(id_pca$rotation[, 1:indexFor99pct])
  trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
  trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))
  
  image <-
    matrix(trunc,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
}


#2.4.5

zero <- id_pca$x[-400 + 1 * 400 + 1, 1:10]

one <- id_pca$x[-400 + 2 * 400 + 1, 1:10]
zeros <- zero
ones <- one
one
zero
for (u in 1:10) {
  aVarible <- (u-1)*4000
  for (i in 2:400) {
    zero <- id_pca$x[-400 + 1 * 400 + i + aVarible, 1:10]
    
    one <- id_pca$x[-400 + 2 * 400 + i + aVarible, 1:10]
    
    zeros <- zeros + zero
    ones <- ones + one
  }
}

zeros <- zeros / 4000
ones <- ones / 4000
ones
zeros

trunc <- ones %*%
  t(id_pca$rotation[, 1:10])
trunc <- scale(trunc, center = -1 * id_pca$center, scale = FALSE)
trunc <- ((trunc - min(trunc)) / (max(trunc) - min(trunc)))

image <-
  matrix(trunc,
         nrow = imageSize,
         ncol = imageSize,
         byrow = FALSE)
image <- rotate(image)
image(image,  zlim = c(0, 1), col = gray(0:100 / 100))

one
zero

id_pca$rotation[1:10,1:10]
?prcomp


































































































































































































































#2.3 Preprocessing
#Setup
library(spatstat)
id_100 <- id_100[sample(nrow(id_100)),]
id_1Person <- id[0:8000,]

id_mat <- data.matrix(id_1Person, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x)
  t(apply(x, 2, rev))

#Gaussian Smoothing Sigma 0.5
smoothImage <- function(grayImg) {
  smoothed <-
    as.matrix(blur(
      as.im(grayImg),
      sigma = 0.5,
      normalise = FALSE,
      bleed = TRUE,
      varcov = NULL
    ))
  return(smoothed)
}

#Gaussian Smoothing
# Smooth all images

for (i in 1:nrow(id_mat))
{
  rotated <- c(id_mat[i, 2:ncol(id)])
  image <-
    matrix(rotated,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- smoothImage(image)
  id_mat[i, 2:ncol(id_mat)] <-
    matrix(image,
           nrow = 1,
           ncol = ncol(id_mat) - 1,
           byrow = FALSE)
}
id_100 <- as.data.frame(id_mat)
id_100[, 1] <- factor(id_100[, 1])

folds <- createFolds(id_100$V1, k = 10)
data <- id_100[,]
results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_100[-folds[[i]],1]
  test_labels <- id_100[folds[[i]],1]
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#Sigma 0.5
summary(results)
sd(results)





#Gaussian Smoothing Sigma 5
id_1Person <- id[0:8000,]

id_mat <- data.matrix(id_1Person, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x)
  t(apply(x, 2, rev))

smoothImage <- function(grayImg) {
  smoothed <-
    as.matrix(blur(
      as.im(grayImg),
      sigma = 5,
      normalise = FALSE,
      bleed = TRUE,
      varcov = NULL
    ))
  return(smoothed)
}

#Gaussian Smoothing
# Smooth all images
for (i in 1:nrow(id_mat))
{
  rotated <- c(id_mat[i, 2:ncol(id)])
  image <-
    matrix(rotated,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- smoothImage(image)
  id_mat[i, 2:ncol(id_mat)] <-
    matrix(image,
           nrow = 1,
           ncol = ncol(id_mat) - 1,
           byrow = FALSE)
}
id_100 <- as.data.frame(id_mat)
id_100[, 1] <- factor(id_100[, 1])

folds <- createFolds(id_100$V1, k = 10)
data <- id_100[,]
results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_100[-folds[[i]],1]
  test_labels <- id_100[folds[[i]],1]
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#Sigma 5
summary(results)
sd(results)

#Gaussian Smoothing Sigma 10
id_1Person <- id[0:8000,]

id_mat <- data.matrix(id_1Person, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x)
  t(apply(x, 2, rev))

smoothImage <- function(grayImg) {
  smoothed <-
    as.matrix(blur(
      as.im(grayImg),
      sigma = 10,
      normalise = FALSE,
      bleed = TRUE,
      varcov = NULL
    ))
  return(smoothed)
}

#Gaussian Smoothing
# Smooth all images
for (i in 1:nrow(id_mat))
{
  rotated <- c(id_mat[i, 2:ncol(id)])
  image <-
    matrix(rotated,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- smoothImage(image)
  id_mat[i, 2:ncol(id_mat)] <-
    matrix(image,
           nrow = 1,
           ncol = ncol(id_mat) - 1,
           byrow = FALSE)
}
id_100 <- as.data.frame(id_mat)
id_100[, 1] <- factor(id_100[, 1])

folds <- createFolds(id_100$V1, k = 10)
data <- id_100[,]
results = c()
for(i in 1:10){
  train <- data[-folds[[i]],-1]
  test <- data[folds[[i]],-1]
  
  train_labels <- id_100[-folds[[i]],1]
  test_labels <- id_100[folds[[i]],1]
  
  result <- knn(train = train, test = test, cl = train_labels, k = 1)
  cfcMtx <- confusionMatrix(data = result, reference = test_labels)
  acc <- sum(diag(cfcMtx$table))/sum(cfcMtx$table)
  results <- c(results, acc)
}
#Sigma 10
summary(results)
sd(results)


