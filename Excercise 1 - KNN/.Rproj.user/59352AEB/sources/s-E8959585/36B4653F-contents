library(class)
library(gmodels)
load("id100.Rda")

set.seed(123)
dataset <- id
dataset_shuffle <- dataset[sample(nrow(dataset)),]


train_set <- dataset_shuffle[1 : (nrow(dataset_shuffle)/2),2:ncol(dataset_shuffle)]
train_labels <- dataset_shuffle[1 : (nrow(dataset_shuffle)/2),1]
test_set <- dataset_shuffle[ (nrow(dataset_shuffle)/2 + 1) : nrow(dataset_shuffle) ,2:ncol(dataset_shuffle)]
test_labels <- dataset_shuffle[ (nrow(dataset_shuffle)/2 + 1) : nrow(dataset_shuffle) ,1]

res <- knn(train = train_set,test = test_set,cl = train_labels,k = 5)
table <- CrossTable(x= test_labels,y= res,prop.chisq=FALSE)
table$prop.tbl
sum(diag(table$prop.tbl))
