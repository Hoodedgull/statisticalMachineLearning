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

























































































































































































































































#2.3 Preprocessing
library(spatstat)

id_mat <- data.matrix(id, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x)
  t(apply(x, 2, rev))

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
id <- as.data.frame(id_mat)
id[, 1] <- factor(id[, 1])
