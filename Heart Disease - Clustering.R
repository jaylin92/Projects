rm(list = ls())

library(gridExtra)
library(ggplot2)
library(factoextra)
library(NbClust)
library(clValid)

setwd("C:/Users/J/Documents/R Projects/Data/heart-disease-uci")
heart_disease <- read.csv('heart.csv', sep=',')

lapply(heart_disease, min)

summary(heart_disease)

#check for duplicates
heart_disease <- heart_disease[!duplicated(heart_disease),]
dim(heart_disease)
dim(heart_disease)

# Remove id
heart_disease = heart_disease[ , !(names(heart_disease) %in% c("id"))]
#renaming first variable
names(heart_disease)[1] <-"age"
names(heart_disease)  

scaled = scale(heart_disease,center = TRUE, scale = TRUE)

summary(scaled)

### determine clusters
# Elbow method
fviz_nbclust(scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#elbow suggest 4

# Silhouette method
fviz_nbclust(scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#silhoutette suggests 2


# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#gap suggests 9


clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(scaled, nClust = 2:10, 
                  clMethods = clmethods, validation = "internal")
summary(intern)
# want to minimize connectivity
#silhouette : -1 poorly clustered, 1 well clustered
# dunn : want maximize dunn. ratio between smallest distance between obs not in same cluster to largest intra-cluster distance. 


########### kmeans

set.seed(123)

first_clust = kmeans(scaled, centers = 5, nstart = 1)
first_clust$centers

set.seed(1234)
second_clust = kmeans(scaled, centers = 5, nstart = 1)
second_clust$centers


heart_disease[,"first_clust"] = first_clust$cluster
heart_disease[,"second_clust"] = second_clust$cluster

# visualizations

fviz_cluster(first_clust, data = scaled)

fviz_cluster(second_clust, data = scaled)


####### trying out  different cluster numbers
k2 <- kmeans(scaled, centers = 2, nstart = 25)
k3 <- kmeans(scaled, centers = 3, nstart = 25)
k4 <- kmeans(scaled, centers = 4, nstart = 25)
k5 <- kmeans(scaled, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = scaled) + ggtitle("k = 5")


grid.arrange(p1,p2,p3, p4, nrow = 2)






############ Hierarchical clustering

d <- dist(scaled, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)



m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# testing for cluster structure
ac <- function(x) {
  agnes(scaled, method = x)$ac
}
# ward gives best struction, closest to 1
map_dbl(m, ac)

hc3 <- agnes(scaled, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)
# Number of members in each cluster
table(sub_grp)
# draw borders around clusters
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)


fviz_cluster(list(data = scaled, cluster = sub_grp))

## find optimal k
fviz_nbclust(scaled, FUN = hcut, method = "wss")
# elbow 4,5
fviz_nbclust(scaled, FUN = hcut, method = "silhouette")
# silhouette 2
gap_stat <- clusGap(scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
# gap 4




