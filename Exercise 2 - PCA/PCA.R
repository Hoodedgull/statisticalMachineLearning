load("idList-co-100.Rda")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)
id[,1]<- factor(id[,1])

set.seed(123)
id_shuffle <- id[sample(nrow(id)),]

id_pca <- prcomp(id_shuffle[,-1], center = TRUE, scale = TRUE)
