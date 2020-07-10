rm(list = ls())

library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(pROC)
library(randomForest)
library(e1071)
library(caret)
library(InformationValue)


setwd("C:/Users/J//Documents/R Projects/Data/heart-disease-uci")
df <- read.csv('heart.csv', sep=',')




####################Variables
# age: The person's age in years
# sex: The person's sex (1 = male, 0 = female)
# cp: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)
# trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)
# chol: The person's cholesterol measurement in mg/dl
# fbs: The person's fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)
# restecg: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
# thalach: The person's maximum heart rate achieved
# exang: Exercise induced angina (1 = yes; 0 = no)
# oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot. See more here)
# slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
# ca: The number of major vessels (0-3)
# thal: A blood disorder called thalassemia (3 = normal; 6 = fixed defect; 7 = reversable defect)
# target: Heart disease (0 = no, 1 = yes)

#### cleaning
#checking na in df
sum(is.na(df))

#check for duplicates
heart <- df[!duplicated(df),]
dim(df)
dim(heart)

#renaming first variable
names(heart)[1] <-"age"
names(heart)  

######### Exploring Data

head(heart)
hist(heart$target)
table(heart$target)




attach(heart)
head(corr[, 1:13])


corr <- round(cor(heart), 1)
head(corr[, 1:13])
p.mat <- cor_pmat(heart)
head(p.mat[, 1:13])


# correlation coefficients
ggcorrplot(corr)
ggcorrplot(corr, type = "lower", outline.col = "white")
ggcorrplot(corr, type = "lower", outline.col= "white", lab = TRUE)



# scatterplot of cholesterol and resting blood pressure
ggplot(heart, aes(x= chol, y= trestbps)) + geom_point() 
cor(chol,trestbps)


#histogram of cholesterol  + age
ggplot(data = heart, aes(chol)) + geom_histogram()

ggplot(data = heart, aes(age)) + geom_histogram(binwidth = 1) + stat_function(fun = dnorm, colour = "red",
                arg = list(mean = mean(heart$age, na.rm = TRUE),
                           sd = sd(heart$age, na.rm = TRUE)))

ggplot(heart,aes(x= sex, y= age, color = sex))+ geom_bo


# distribution of Sex, male = 1
ggplot(data = heart, aes(sex)) + geom_histogram()
table(sex)

ggplot(data = heart, aes(target)) + geom_histogram()



##### Logistic regression   
# ROC

glm.fit = glm(target~., data = heart,family = binomial)
# ROC CURVE
roc(target, glm.fit$fitted.values, plot= TRUE)

table(glm.fit)
par(pty="s")

# 1- Specificity
roc(target, glm.fit$fitted.values, plot= TRUE, legacy.axes= TRUE)

roc(target, glm.fit$fitted.values, plot= TRUE, legacy.axes= TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage")

# Color + AUC
roc(target, glm.fit$fitted.values, plot= TRUE, legacy.axes= TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage", col = "#377eb8", lwd= 4, print.auc= TRUE)

# extracting info at particular threshold
roc.info <-roc(target, glm.fit$fitted.values, plot= TRUE, legacy.axes= TRUE)
roc.df <- data.frame(tpp = roc.info$sensitivities*100,
                     fpp=(1- roc.info$specificities)*100,thresholds = roc.info$thresholds)

head(roc.df) # 100% of tpp(heart condition) are correctly classified, 100% of fpp (no heart condition) incorrectly classified at -Inf
# want highest tpp and lowest fpp


##### Random Forest
set.seed(123)
rf.model <- randomForest(factor(target)~., data = heart)
par(pty="m")
plot.roc(target, rf.model$votes[,1], percent = TRUE, col = "#4daf4a", lwd= 4, print.auc = TRUE, add=TRUE, print.auc.y=40)
summary(rf.model)


##### SVM 
svm.model<- svm(target~., data= heart, type = "C-classification", kernel= "radial")
summary(svm.model)
plot(svm.model, data = heart, trestbps~chol)

# Confusion matrix
pred <- predict(svm.model, heart )
tab <- table(Predicted = pred, Actual = heart$target)
tab
# Missclassifiction rate
1-sum(diag(tab))/sum(tab) # Misclassification error 7.9%

#### Tuning
set.seed(123)

heart2 <- heart
heart2$target <- as.factor(heart2$target)
# high cost = high penalty for non seperateable points
tune.model <- tune(svm, target~., data = heart2,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

plot(tune.model)
# better results are in darker region, lower misclassification error. 
# lower values for cost, various epsilon values.
# smaller cost gives a higher tolerance for misclassification, smoother surface 
# higher cost can lead to overfitting
summary(tune.model)

# best model
svm.best.model <- tune.model$best.model
summary(svm.best.model)
plot(svm.best.model, data = heart, chol~trestbps, slice = list(chol= 3, trestbps = 4))
 
pred <-predict(svm.best.model, heart)
tab <- table(Predicted = pred, Actual = heart$target)
tab

1-sum(diag(tab))/sum(tab) #misclassification error 3.6%


############### Train/test

set.seed(42)

samplesize <- floor(.80*nrow(heart2))
train.ind<-sample(seq_len(nrow(heart2)),size= samplesize)
train <- heart2[train.ind,]
test <- heart2[-train.ind,]

##### SVM

svm.model<- svm(target~., data= train, type = "C-classification", kernel= "radial")
summary(svm.model)
plot(svm.model, data = train, trestbps~chol)

# Confusion matrix
pred <- predict(svm.model, test )
tab <- table(Predicted = pred, Actual = test$target)
tab
# Missclassifiction rate
1-sum(diag(tab))/sum(tab) # Misclassification error 22.37%


#### Tune Model


set.seed(42)

heart2 <- heart
heart2$target <- as.factor(heart2$target)

samplesize <- floor(.70*nrow(heart2))
train.ind<-sample(seq_len(nrow(heart2)),size= samplesize)
train <- heart2[train.ind,]
test <- heart2[-train.ind,]

# high cost = high penalty for non seperateable points
tune.model <- tune(svm, target~., data = train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

plot(tune.model)
# better results are in darker region, lower misclassification error. 
# lower values for cost, various epsilon values.
# smaller cost gives a higher tolerance for misclassification, smoother surface 
# higher cost can lead to overfitting

summary(tune.model)

# best model
svm.best.model <- tune.model$best.model
summary(svm.best.model)

pred <-predict(svm.best.model, test)
tab <- table(Predicted = pred, Actual = test$target)
tab

1-sum(diag(tab))/sum(tab) #misclassification error 26.31%





##### Logistic regression   
# ROC 



samplesize <- floor(.80*nrow(heart2))
train.ind<-sample(seq_len(nrow(heart2)),size= samplesize)
train <- heart2[train.ind,]
test <- heart2[-train.ind,]


glm.fit = glm(target~., data = train,family = binomial)
summary(glm.fit)

glm.pred <- predict(glm.fit, test, type= "response")

optCutoff <- optimalCutoff(test$target, glm.pred)
summary(glm.fit)
misClassError(test$target,glm.pred, threshold = optCutoff)

plotROC(test$target, glm.pred)

Concordance(test$target, glm.pred)

sensitivity(test$target, glm.pred, threshold = optCutoff)
specificity(test$target, glm.pred, threshold = optCutoff)

tab <-confusionMatrix(test$target, glm.pred, threshold = optCutoff)

tab2 <- array(0, dim=dim(tab))
for (i in 1:ncol(tab)) {tab2[,i] <- tab[,i]}

1-sum(diag(tab2))/sum(tab2) #misclassification error 26.31%



