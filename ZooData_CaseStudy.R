rm(list=ls())
#data for association rules
library('arules')
TransFood = read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
TransFood = TransFood[, -1]
TransFood = as(as.matrix(TransFood), "transactions")

#data for clustering
Food_by_month = read.csv('http://homepages.uc.edu/~maifg/DataMining/data/qry_Food_by_Month.csv')
colnames(Food_by_month) = c("nicknames", "sales in Oct-10","sales in Nov-10", "sales in Dec-10", "sales in Jan-11", "sales in Feb-11", "sales in Mar-11")

#let's do clustering first:
food_scaled= scale(Food_by_month[,2:7])
head(food_scaled)
dim(food_scaled)

fit = kmeans(food_scaled, 3) #3 cluster solution

#Display number of clusters in each cluster
table(fit$cluster)

#plot cluster in k-means
library(fpc)
plotcluster(food_scaled, fit$cluster)

#See exactly which item are in 1st group
food_scaled[fit$cluster==1,]

#Display number of clusters in each cluster
table(fit$cluster)
library(fpc)
plotcluster(food_scaled, fit$cluster)

# get cluster means for scaled data
aggregate(food_scaled,by=list(fit$cluster),FUN=mean)

# Determine number of clusters
wss = (nrow(food_scaled)-1)*sum(apply(food_scaled,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(food_scaled, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#there is a steep decrease till 3 and after that from 5 to 6
fit3 = kmeans(food_scaled, 3)
fit6 = kmeans(food_scaled, 6)
plotcluster(food_scaled, fit3$cluster)
plotcluster(food_scaled, fit6$cluster)

#prediction strength
prediction.strength(food_scaled, Gmin=2, Gmax=10, M=10,cutoff=0.8)

#silhoutte method
d = dist(food_scaled, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(food_scaled, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')
#dunn method (BEST)
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
food.dist=dist(food_scaled)

#Obtain clusters using the Wards method
food.hclust=hclust(food.dist, method="ward")
plot(food.hclust)

#Cut dendrogram at the 3 clusters level and obtain cluster membership
food.3clust = cutree(food.hclust,k=3)
food.2clust = cutree(food.hclust,k=2)
plot(food.3clust)

#See exactly which item are in third group
food_scaled[food.3clust==3,]
#get cluster means for raw data
#Centroid Plot against 1st 2 discriminant functions
#Load the fpc library needed for plotcluster function
library(fpc)
#plotcluster(ZooFood, fit$cluster)
plotcluster(food_scaled, food.3clust)
plotcluster(food_scaled, food.2clust)

rect.hclust(tree = food.hclust, k=3)
rect.hclust(tree = food.hclust, k=2)

#model based cluster
install.packages('mclust')
library(mclust)
mclust_result=Mclust(food_scaled)
summary(mclust_result)
plot(mclust_result)


#Association

library('arules')
TransFood = read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
TransFood = TransFood[, -1]
TransFood = as(as.matrix(TransFood), "transactions")

summary(TransFood)
x = TransFood[size(TransFood) > 12] #find transaction with length>25
inspect(x)
itemFrequencyPlot(TransFood, support = 0.1, cex.names=1.2) #cex name isfor label size

#run apriori algorithm
basket_rules = apriori(TransFood,parameter = list(sup = 0.003, conf = 0.5,target="rules"))
summary(basket_rules)
inspect(head(basket_rules)) #lhs: atecedent, rhs: consequent
inspect(subset(basket_rules,size(basket_rules)>3))
inspect(subset(basket_rules, lift>10))
Slice.of.CheeseFood.rhs <- subset(basket_rules, subset = rhs %in% "Slice.of.CheeseFood" & lift>3.5)
inspect(Slice.of.CheeseFood.rhs)
Slice.of.CheeseFood.rhs <- subset(basket_rules, subset = lhs %in% "Slice.of.CheeseFood" & lift>1.5)
inspect(Slice.of.CheeseFood.lhs)





