library(class)
library(gmodels)
library(caret)
library(C50)
load("idList-cornered-100.Rdata")
id <- do.call(rbind, idList[15:16])
dataset <- as.data.frame(id)
set.seed(123)
dataset_shuffle <- dataset[sample(nrow(dataset)), ]
#4.1.1 Dunno what to do
id_pca <- prcomp(dataset_shuffle[, -1], center = TRUE, scale = TRUE)

Attributes <- id_pca$x[,1:5]

train_data <- Attributes[1:3600,]
test_data <- Attributes[3601:4000,]

train_label <- dataset_shuffle[1:3600,1]
test_label <- dataset_shuffle[3601:4000,1]



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


#4.1.3 Ish
c5prediction <- predict(m,test_data)
crosstable <- CrossTable(test_label,c5prediction,prop.chisq = FALSE)
crosstable$prop.tbl
sum(diag(crosstable$prop.tbl))
