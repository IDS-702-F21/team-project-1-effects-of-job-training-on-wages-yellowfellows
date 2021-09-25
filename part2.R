rm(list = ls())

library(arm)
library(rms)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(plyr)
require(gridExtra)


##########################################################################
############################ 0. Loading the data #########################
##########################################################################


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
# data$race <- data$black


##########################################################################
############################### 1. EDA ###################################
##########################################################################


cond_prob <- function (df, col1, col2) {
  round(apply(table(df[, c(col1, col2)])/sum(table(df[, c(col1, col2)])),
              2,function(x) x/sum(x)), 2)
}


cond_prob(data, "pos_income_f",  "treat")
chisq.test(table(data[,c("pos_income_f", "treat")]))

cond_prob(data, "pos_income_f",  "black")
chisq.test(table(data[,c("pos_income_f", "black")]))  # **

cond_prob(data, "pos_income_f",  "hispan")
chisq.test(table(data[,c("pos_income_f", "hispan")]))

cond_prob(data, "pos_income_f",  "married")
chisq.test(table(data[,c("pos_income_f", "married")]))

cond_prob(data, "pos_income_f",  "nodegree")
chisq.test(table(data[,c("pos_income_f", "nodegree")]))


ggplot(data,aes(x=pos_income_f, y=age, fill=pos_income_f)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  theme_classic()

ggplot(data,aes(x=pos_income_f, y=educ, fill=pos_income_f)) +  # **
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  theme_classic()

# Interactions
cond_prob(data[data$black == 1,], "pos_income_f",  "treat")    # **  
cond_prob(data[data$black == 0,], "pos_income_f",  "treat")
table(data[, c("treat", "black")])


cond_prob(data[data$hispan == 1,], "pos_income_f",  "treat")   
cond_prob(data[data$hispan == 0,], "pos_income_f",  "treat")

data$sum_r <- (as.integer(data$black) - 1) + (as.integer(data$hispan) - 1)
table(data$sum_r)

cond_prob(data[data$married == 1,], "pos_income_f",  "treat")  # ** 
cond_prob(data[data$married == 0,], "pos_income_f",  "treat")

cond_prob(data[data$nodegree == 1,], "pos_income_f",  "treat")   
cond_prob(data[data$nodegree == 0,], "pos_income_f",  "treat")


##########################################################################
############################### 2. Model Selection #######################
##########################################################################


null_model <- glm(pos_income_f ~ treat, data = data, family=binomial)
full_model <- glm(pos_income_f ~ treat + black + hispan + 
                    age + educ + married + treat * black + treat * married,
                  data=data, family=binomial)
step_model <- step(null_model, scope = list(lower=null_model, upper=full_model),
                   direction='both', trace=0)

# Model with no interactions
nointeract_model <- glm(pos_income_f ~ treat + age + black,
                      data=data, family=binomial)
anova(step_model, nointeract_model, test='Chisq')  # non significant interaction
                                                   # treat * black

step_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(step_model) >= 0.5, "1", "0")),
                                     data$pos_income_f, positive = "1")
step_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
step_model_matrix$byClass[c("Sensitivity","Specificity")]

roc(data$pos_income_f, fitted(step_model),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3")

step_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(step_model) >= 0.779, "1", "0")),
                                     data$pos_income_f, positive = "1")
step_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
step_model_matrix$byClass[c("Sensitivity","Specificity")]


ni_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(nointeract_model) >= 0.5, "1", "0")),
                                     data$pos_income_f, positive = "1")
ni_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
ni_model_matrix$byClass[c("Sensitivity","Specificity")]

roc(data$pos_income_f, fitted(nointeract_model),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3")

ni_model_matrix <- confusionMatrix(as.factor(ifelse(fitted(nointeract_model) >= 0.763, "1", "0")),
                                   data$pos_income_f, positive = "1")
ni_model_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data)
ni_model_matrix$byClass[c("Sensitivity","Specificity")]


##########################################################################
############################### 3.Model assessment #######################
##########################################################################


rawresid1 <- residuals(step_model, "resp")
binnedplot(x=fitted(step_model),y=rawresid1,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

rawresid2 <- residuals(nointeract_model, "resp")
binnedplot(x=fitted(nointeract_model),y=rawresid2,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")


##########################################################################
############################### 4. Inference #############################
##########################################################################
