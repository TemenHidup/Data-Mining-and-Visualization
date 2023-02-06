library(ggplot2)
library(dplyr)
library(WVPlots)
library(readxl)
library(lme4)
library(emmeans)
library(effects)
library(Hmisc)
library(cowplot)


ins <- read_excel("C:/Users/nico/Desktop/UAS Data Mining/UAS DatMin/STAT6157016 - Data Mining and Visualization - Copy/Insurance.xlsx")

print(ins)

# Finding the missing data of the data set Insurance

sum(is.na(ins))


ageExp <- ggplot(ins, aes(x = age,y = expenses)) +
  geom_point()
  
print(ageExp)

bmiExp <- ggplot(ins, aes(x = bmi,y = expenses)) +
  geom_point()

print(bmiExp)

sexExp <- ggplot(ins, aes(x = sex,y = expenses)) +
  geom_point() 

sexExp

chilEXP <- ggplot(ins, aes(x = children,y = expenses)) +
  geom_point() 

chilEXP


smoExp <- ggplot(ins, aes(x = smoker, y = expenses)) +
  geom_point() 

smoExp

regExp <- ggplot(ins, aes(x = region, y = expenses)) +
  geom_point() 

regExp

#Significance Test 
summary(ins)
insu <- lm(expenses ~. , data=ins)
summary(insu)
exp(coefficients(insu))

# Model Reggresion
plot(ins$age,ins$expenses,
     main='Regression for Age and Expenses',
     xlab='Age',ylab='Expenses')

plot(ins$bmi,ins$expenses,
     main='Regression for bmi and Expenses',
     xlab='bmi',ylab='Expenses')

plot(ins$children,ins$expenses,
     main='Regression for total children and Expenses',
     xlab='Children',ylab='Expenses')

ageLm <- ggplot(ins, aes(x = age,y = expenses)) +
  geom_point() +
  geom_smooth(method = "lm")

ageLm

bmiLm <- ggplot(ins, aes(x = bmi,y = expenses)) +
  geom_point()+
  geom_smooth(method = "lm")

bmiLm

chilLm <- ggplot(ins, aes(x = children,y = expenses)) +
  geom_point() +
  geom_smooth(method = "lm")

chilLm
