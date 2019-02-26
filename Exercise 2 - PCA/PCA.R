load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1]<- factor(id[,1])

set.seed(123)
id_shuffle <- id[sample(nrow(id)),]

id_pca <- prcomp(id_shuffle[,-1], center = TRUE, scale = TRUE)

totalvar = sum(id_pca$sdev)

#2.1.1
plot(id_pca$sdev[1:20], main= "standard deviation (from eigenvalues)", ylab="std. dev.", xlab="Principal component")
plot(id_pca$sdev[1:20]/totalvar, main = "Proportion of variance", xlab="Principal component", ylab= "proportion of total variance explained")
plot(cumsum(id_pca$sdev/totalvar), main="Cumulative sum of proportions of variance", xlab="Principal Component", ylab="cumulative sum")


#2.1.2
sum(id_pca$sdev[1:72]/totalvar)


train <- data[0:10000,-1]
test <- data[10001:40000,-1]
train_labels <- data[0:10000,1]
test_labels <- data[10001:40000,1]

data <- id_pca$x[,1:72]
  