display_pca_info <- function(pca_dataset, datasetName = "Dataset") {
  pca_info <- list()
  
  pca_info$eigs <- pca_dataset$sdev^2
  
  pca_info$Proportion <- pca_info$eigs/sum(pca_info$eigs)
  pca_info$Cumulative <- cumsum(pca_info$eigs)/sum(pca_info$eigs)
  
  
  plot(pca_dataset$sdev[1:20], xlab = "PCA", ylab = "SD", type="o", main = datasetName)
  plot(pca_info$Proportion[1:20],  type="o", col="blue", main= datasetName, xlab = "PCA")
  plot(pca_info$Cumulative[1:20], type="o", col="red", xlab = "PCA", ylab = "Cumulative Proportion Varience", main = datasetName)
  return(pca_info)
}

splitDatasetInHalf <- function(dataset) {
  outData <- list()
  
  half_size <- nrow(dataset) / 2
  datasetSize <- nrow(dataset)
  
  outData$test_set <- dataset[1:half_size,-1]
  outData$train_set <- dataset[(half_size+1):datasetSize,-1]
  
  outData$test_set_labels <- dataset[1:half_size, 1]
  outData$train_set_labels <- dataset[(half_size+1):datasetSize, 1]
  
  return(outData)
}

shuffleAndSplitData <- function(dataset) {
  
  set.seed(443)
  dataset_shuffle <- dataset[sample(nrow(dataset)),]
  
  out_data <- splitDatasetInHalf(dataset_shuffle)
  return(out_data)
}



 
