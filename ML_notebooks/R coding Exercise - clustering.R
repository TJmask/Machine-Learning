library(cluster)
install.packages('rattle')
data(wine, package='rattle')
head(wine)
dim(wine)


wine.stand  = scale(wine[-1])  # To standarize the variables

# K-Means
k.means.fit = kmeans(wine.stand, 3) # k = 3
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size


wssplot = function(data, nc=15, seed=1234){
  wss   = (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers=i)$withinss)
  }
  plot(x    = 1:nc,
       y    = wss, 
       type = "b",
       xlab = "Number of Clusters",ylab="Within groups sum of squares")
}
wssplot(wine.stand, nc=6) 



clusplot(x      = wine.stand,
         clus   = k.means.fit$cluster,
         main   = '2D representation of the Cluster solution',
         color  = TRUE,
         shade  = TRUE,
         labels = 2,
         lines  = 0)


table(wine[,1],k.means.fit$cluster)


d     =  dist(wine.stand, method = "euclidean") # Euclidean distance matrix.
H.fit =  hclust(d, method="ward.D")
plot(H.fit)                                     # display dendogram
groups = cutree(H.fit, k=3)                     # cut tree into 5 clusters


# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 
table(wine[,1],groups)

d     = dist(wine.stand, method = "euclidean") # Euclidean distance matrix.
H.fit = hclust(d, method="ward.D")

plot(H.fit) # display dendogram
groups = cutree(H.fit, k=5) # cut tree into 5 clusters


# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=5, border="red") 
table(wine[,1],groups)






url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
head(food)

set.seed(123456789) ## to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat

## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])


plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)


## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7
set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])


library(cluster)
clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg, main='Dendrogram') ## dendrogram
