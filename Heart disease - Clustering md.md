---
title: "R Notebook"
output: rmarkdown::github_document
---

```{r}
library(ggplot2)
setwd("C:/Users/J/Documents/R Projects/Data/heart-disease-uci")
heart_disease <- read.csv('heart.csv', sep=',')

#check for all variables are numeric
lapply(heart_disease, min)
summary(heart_disease)
```


```{r}
#check for duplicates
dim(heart_disease)
heart_disease <- heart_disease[!duplicated(heart_disease),]
dim(heart_disease)


# Remove id
heart_disease = heart_disease[ , !(names(heart_disease) %in% c("id"))]
#renaming age variable
names(heart_disease)[1] <-"age"
names(heart_disease)  

scaled = scale(heart_disease,center = TRUE, scale = TRUE)

summary(scaled)
```

```{r}
############ Kmeans

set.seed(123)


first_clust = kmeans(scaled, centers = 5, nstart = 1)
first_clust$centers


set.seed(1234)
second_clust = kmeans(scaled, centers = 5, nstart = 1)
second_clust$centers



heart_disease[,"first_clust"] = first_clust$cluster
heart_disease[,"second_clust"] = second_clust$cluster


plot_one = ggplot(heart_disease, aes(x =age, y = chol, color = as.factor(first_clust))) + 
  geom_point()
plot_one 

plot_two = ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(second_clust))) +  geom_point()
plot_two

```

```{r}

############ Hierarchical clustering

###### Complete linkage
hier_clust_1 = hclust(dist(scaled), method= "complete")

#dendrogram
plot(hier_clust_1)

# pruning
hc_1_assign <- cutree(hier_clust_1, 5)

######## Single linkage

hier_clust_2 = hclust(dist(scaled), method = "single")
plot(hier_clust_2)

# Getting cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)

# Adding cluster assignments
heart_disease['hc_clust'] = hc_1_assign

# remove sex, first_clust, second_clust
hd_simple = heart_disease[, !(names(heart_disease) %in% c("sex","first_clust","second_clust"))]

# summary stats
clust_summary = do.call(data.frame, aggregate(. ~ hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary
```
```{r}
# Plotting age and chol
plot_one = ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(hc_clust))) + 
  geom_point()
plot_one 

# Plotting oldpeak and trestbps
plot_two = ggplot(heart_disease, aes(x= oldpeak , y= trestbps, color = as.factor(hc_clust))) + 
  geom_point()
plot_two

```

