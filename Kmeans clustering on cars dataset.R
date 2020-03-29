
cars <- read.csv("G:/R/cars.csv") #imported the dataset in R 
str(cars)
summary(cars)
car = na.omit(cars)

#cluster according to brand

label <- car$brand
car$brand <- NULL
str(car)

#Scale all your column variables
seeds_df_sc <- as.data.frame(scale(car))
summary(seeds_df_sc)
str(seeds_df_sc)


#Kmeans
#started with 3 clusters
set.seed(20)
clusterk <- kmeans(car[,1:2],3,nstart=20)
clusterk
table(clusterk$cluster, car$brand)
clusterk$cluster <- as.factor(clusterk$cluster)


clusterk <- kmeans(iris[,1:2],3,nstart=20)
clusterk$betweenss
clusterk$withinss
clusterk$tot.withinss
clusterk$totss


plot(car[c("mpg","cylinders","cubicinches")],col=clusterk)

#elbow method for k value 
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(car[,1:2], i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

set.seed(50)
kmeans = kmeans(x = car[,1:2], centers = 3)
y_kmeans = kmeans$cluster

#################################################################

# Visualising the clusters
library(cluster)
clusplot(car,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of species'),
         xlab = 'mpg',
         ylab = 'cylinders')
