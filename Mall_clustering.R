rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(clValid)

df <- read.csv("C:/Users/J/Documents/R Projects/Data/customer-segmentation-tutorial-in-python/Mall_Customers.csv", header = TRUE)

# CustomerIDUnique ID assigned to the customer
# GenderGender of the customer
# AgeAge of the customer
# Annual Income (k$)Annual Income of the customee
# Spending Score (1-100)Score assigned by the mall based on customer behavior and spending nature


head(df)
names(df)
names(df)[4]<-"Annual.Income"
names(df)[5]<-"Spending.Score"
names(df)
#### Age

p <-ggplot(df, aes(x= Age)) + geom_histogram(binwidth = 1)+ geom_density(alpha = .2, fill ="black")
ggplot(df, aes(x= Age)) + geom_histogram(binwidth = 1)+ geom_density()

ggplot(df, aes(x=Age, fill=Gender)) + geom_density()
p <- ggplot(df, aes(x=Age, fill=Gender)) + geom_density(alpha = .4);p


p + geom_vline(aes(xintercept = mean(Age)), color = "blue", linetype = "dashed",size = 1) + geom_density(alpha = .2, fill ="black")


ggplot(df, aes(x= Age)) + geom_histogram(binwidth = 1) + geom_density(alpha = .2, fill ="#FF6666")

ggplot(df, aes(Age, fill = Gender)) + geom_histogram()
ggplot(df,aes(x=Age, color = Gender)) + geom_histogram(fill = "white", alpha = 0.5, position = "identity")

#Gender
df %>% ggplot(aes( Age, fill = Gender, col = I("white"))) + geom_histogram(breaks=seq(10, 80, by=10))   + facet_wrap(~Gender) 

df %>% ggplot(aes( Annual.Income, fill = Gender, col = I("white"))) + geom_histogram(breaks=seq(0, 150, by=20))   + facet_wrap(~Gender)

ggplot(df, aes(Gender)) + geom_bar(aes(fill=Gender)) + coord_flip()


### Income

ggplot(df, aes(x=Annual.Income)) +geom_histogram() 

ggplot(df, aes(Annual.Income, fill = Gender)) + geom_histogram()

### Spending score

ggplot(df, aes(x=Spending.Score)) +geom_histogram() 

ggplot(df, aes(Spending.Score, fill = Gender)) + geom_histogram()


# Exploration

ggcorr(df, palette= "RdBu",label = TRUE)
dfplot<- df[,c(3,4,5)]
ggcorr(dfplot,label = TRUE)
ggpairs(dfplot)

## Age vs Annual Income wrt Gender

ggplot(df,aes(x= Age, y = Annual.Income, color = Gender, shape = Gender))+ geom_point()
ggplot(df,aes(x= Age, y = Annual.Income, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm)
ggplot(df,aes(x= Age, y = Annual.Income, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm, se= FALSE, fullrange = TRUE)
ggplot(df,aes(x= Age, y = Annual.Income, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm, se= FALSE, fullrange = TRUE)+ facet_wrap(~Gender)
)


## Age vs Spending Score wrt Gender

ggplot(df,aes(x= Age, y = Spending.Score, color = Gender, shape = Gender))+ geom_point()
ggplot(df,aes(x= Age, y = Spending.Score, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm)
ggplot(df,aes(x= Age, y = Spending.Score, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm, se= FALSE, fullrange = TRUE)
ggplot(df,aes(x= Age, y = Spending.Score, color = Gender, shape = Gender))+ geom_point() + geom_smooth(method = lm, se= FALSE, fullrange = TRUE) + facet_wrap(~Gender)



###  Clustering
df %>% ggplot(aes(x = Annual.Income, y = Spending.Score))  + geom_point() 

df$Gender.f <- factor(df$Gender, levels=c("Male","Female"), labels=c(0,1))
set.seed(123)

# 5 clusters
clusters <- kmeans(df[,3:6], 5)
df$group <- as.factor(clusters$cluster)
str(clusters)

df %>% ggplot(aes(x = Annual.Income, y = Spending.Score, col = group))  + geom_point() 
df %>% ggplot(aes(x = Annual.Income, y = Spending.Score, col = group))  + geom_point() +facet_wrap(~Gender)

# 6 clusters

clusters <- kmeans(df[,3:6], 6)
df$group <- as.factor(clusters$cluster)
str(clusters)

df %>% ggplot(aes(x = Annual.Income, y = Spending.Score, col = group))  + geom_point() 
df %>% ggplot(aes(x = Annual.Income, y = Spending.Score, col = group))  + geom_point() +facet_wrap(~Gender)

df$Gender.n <- df$Gender.f %>% as.numeric(df$Gender.f)-1
head(df, 10)

# Median
sum <- df %>% group_by(group, Gender) %>% summarise(med.income = median(Annual.Income), med.spend = median(Spending.Score), med.age = median(Age)) ;sum 

sum2 <- df %>% group_by(group) %>% summarise(med.income = median(Annual.Income), med.spend = median(Spending.Score), med.age = median(Age), med.gender = median(Gender.n)) ;sum1

#Mean
sum3 <- df %>% group_by(group, Gender) %>% summarise(avg.income = mean(Annual.Income), avg.spend = mean(Spending.Score), avg.age = mean(Age)) ;sum3

sum4 <- df %>% group_by(group) %>% summarise(avg.income = mean(Annual.Income), avg.spend = mean(Spending.Score), avg.age = mean(Age), avg.gender = mean(Gender.n)) ;sum4

head(df)

## Determine # clusters
df2<- df[,c(3,4,5,7)]
wss <- 0
for (i in 1:15) {
  km.out <- kmeans(df2, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


df3<- df[,c(3,4,5)]
head(df3)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df3, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal")
summary(intern)
