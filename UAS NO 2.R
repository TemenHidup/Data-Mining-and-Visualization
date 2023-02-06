library(readxl)
library(MASS)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR) 
library(caret)
library(WVPlots)
library(vip)
library(dials)
library(doParallel)
library(lmtest)

bcw <- read_excel("C:/Users/nico/Desktop/UAS Data Mining/UAS DatMin/STAT6157016 - Data Mining and Visualization - Copy/Breast Cancer Wisconsin (Diagnostic).xlsx")

bcw

sum(is.na(bcw))

# Frekuensi Diagnosis M dan B
# M = malignant , B = Benign
dia <- ggplot(bcw,aes(x = diagnosis))+
  geom_bar(fill = "cornflowerblue")+
  labs(title = "Diagnosis")

dia

itungDia <- table(bcw$diagnosis)

itungDia

# 0 = B = 227 , 1 = M = 173


bcw$diagnosis <- as.factor(bcw$diagnosis)
summary(bcw)

# Declare 0 = B, 1 = M
bcw$diagnosis <- ifelse(bcw$diagnosis == "M", 1, 0)
bcw$diagnosis <- factor(bcw$diagnosis, levels = c(0, 1))


#Data Split
set.seed(549)
training.samples <- createDataPartition(y = bcw$diagnosis, p=0.7, list = FALSE)
train.data <- bcw[training.samples, ]
test.data <- bcw[-training.samples, ]

diag <- glm(diagnosis ~., family = "binomial", data = train.data,)
summary(diag)

prob <- predict(diag , newdata = test.data , type = "response")
predicted <- ifelse(prob>0.5 , 1 , 0)
mean(predicted == test.data$diagnosis)

acc <- sum(test.data$diagnosis == predicted)/length(test.data$diagnosis)
prcs <- sum(test.data$diagnosis == 1 & predicted == 1)/(sum(predicted == 1))
rcl <- sum(test.data$diagnosis == 1 & predicted == 1)/(sum(test.data == 1))

print(acc)
print(prcs)
print(rcl)

#ROC - AUC Curve
tebRoc <- prediction(prob, test.data$diagnosis)
pRoc <- performance(tebRoc, measure = "tpr" , 
                    x.measure = "fpr")
auc <- performance(tebRoc, measure ="auc")
auc <- auc@y.values[[1]]
print(auc)