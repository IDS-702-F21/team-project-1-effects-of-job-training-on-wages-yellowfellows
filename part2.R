library(arm)
library(rms)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(plyr)
require(gridExtra)


# Data loading:
data <- read.csv("lalondedata.txt", sep=',')

head(data)
str(data)
dim(data)
summary(data)
data$X <- NULL
data$treat <- as.factor(data$treat)
data$black <- as.factor(data$black)
data$hispan <- as.factor(data$hispan)
data$married <- as.factor(data$married)
data$nodegree <- as.factor(data$nodegree)
data$pos_income <- as.integer(data$re78 > 0)
data$pos_income_f <- as.factor(data$pos_income)

# Modeling
null_model <- glm(pos_income_f ~ treat, data = data, family=binomial)
full_model <- glm(pos_income_f ~ treat + black + hispan + age + educ + married,
                  data=data, family=binomial)
step_model <- step(null_model, scope = formula(full_model),
                   direction='both', trace=0)
summary(step_model)
step_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(step_model) >= 0.5, "1", "0")),
                                     data$pos_income_f, positive = "1")
step_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
step_model_matrix$byClass[c("Sensitivity","Specificity")]

roc(data$pos_income_f, fitted(step_model),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3")

step_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(step_model) >= 0.774, "1", "0")),
                                     data$pos_income_f, positive = "1")
step_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
step_model_matrix$byClass[c("Sensitivity","Specificity")]


