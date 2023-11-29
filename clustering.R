USArrests <- read.csv('USArrest.csv',row.names = 1) #here 1 represents column1
# as the R will show row no while clustering but we want the state name so convert 
# row name from 1,2,3 to the states names.
View(USArrests)
summary(USArrests)

USArrests1 <- scale(USArrests)  # make another variable USAArrests1 the scaled one
View(USArrests1)
summary(USArrests1)

library(cluster)
library(factoextra)

km <- kmeans(USArrests1,2)
fviz_cluster(km,data=USArrests1)

str(km)

Accuracy <- km$betweenss/km$totss
Accuracy

# if accuracy is above 70% then it is a good model.
# To increase the accuracy if the accuracy is low increase the value of 
# betweenss and to increase that increase the no of clusters

# ELBOW METHOD (tells us the optimal no. of clusters for max. accuracy) 
number <- 1:10
wss <- 1:10

for(i in 1:10)
{
  wss[i] <- kmeans(USArrests1,i)$tot.withinss
}
plot(number, wss, type="b", pch=19)

#choose elbow in the graph for the minimal dip so choose 1st point in 
# the graph where there is a less dip, here that number is "4"

km <- kmeans(USArrests1,4)
fviz_cluster(km,data=USArrests1)
str(km1)

Accuracy1 <- km$betweenss/km$totss
Accuracy1

#silhoutte method (this method not an optimal method to find optimal 
# no of the clusters as the system automatically
# tells optimal point  so it is more prone to errors 
# for example here it is showing that the optimal no of clusters are 2 
# but actually it should be 4 as we found out manually using elbow method )
fviz_nbclust(USArrests1,kmeans,method='silhouette')

# create the variable in the data (to make it easy to check which state belongs
# to which cluster as it might be difficult to check on map)
# now here we are using USArrests because we won't be able to make sense from the 
# normalised data (i.e.USArrests1) so use USArrests which was not normalised initially.
USArrests$cluster <- km1$cluster
View(USArrests)

# Profiling of cluster (understanding(making sense from the dataset) 
# a cluster is called profiling of the cluster)
cmeans <- aggregate(USArrests,
                    by=list(USArrests$cluster),mean) #mean for mean of every cluster
cmeans