library(class) #For knn
library(tictoc) #For timing
library(caret) #For k-folds

load("idList-cornered-100.Rdata")
data <- as.data.frame(idList[1])
for(i in 2:79){
  aPerson <- as.data.frame(idList[i])
  data <- rbind(data,aPerson)
  
}


train_data <- data[(10*2000+1):(15*2000),-1]
train_labels <- data[(10*2000+1):(15*2000),1]
test_data <- data[(15*2000+1):(20*2000),-1]
test_labels<- data[(15*2000+1):(20*2000),1]


id_mat <- data.matrix(data, rownames.force = NA)

rotate <- function(x)
  t(apply(x, 2, rev))
imageSize <- sqrt(ncol(id_mat) - 1)
# Show first 10 images
for (i in 1:10)
{
  rotated <- c(id_mat[-200 + i * 200 + 1, 2:ncol(id_mat)])
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




pca_result <- prcomp(data[,-1], center = TRUE, scale = TRUE)

for (i in 1:10) {
  eigen <- pca_result$rotation[, i]
  eigen <- ((eigen - min(eigen)) / (max(eigen) - min(eigen)))
  image <-
    matrix(eigen,
           nrow = imageSize,
           ncol = imageSize,
           byrow = FALSE)
  image <- rotate(image)
  image(image,  zlim = c(0, 1), col = gray(0:100 / 100))
  
}