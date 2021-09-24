library(ggplot2)
library(car)
library(MASS)

# Loading the data
data <- read.csv("lalondedata.txt", sep=',')

head(data)
str(data)
dim(data)
summary(data)
data$treat <- as.factor(data$treat)
data$black <- as.factor(data$black)
data$hispan <- as.factor(data$hispan)
data$married <- as.factor(data$married)
data$nodegree <- as.factor(data$nodegree)

# EDA:
ggplot(data, aes(x=re78)) + geom_histogram()
ggplot(data, aes(x=log(re78))) + geom_histogram()

ggplot(data, aes(x=treat, y=re78)) + geom_boxplot()

ggplot(data, aes(x=age, y=re78)) + geom_point() + geom_jitter()

ggplot(data, aes(x=educ, y=re78)) + geom_point() + geom_jitter()

ggplot(data, aes(x=black, y=re78)) + geom_boxplot()   # ***

ggplot(data, aes(x=hispan, y=re78)) + geom_boxplot()

ggplot(data, aes(x=married, y=re78)) + geom_boxplot() # ***

ggplot(data, aes(x=nodegree, y=re78)) + geom_boxplot()

without_zero <- data[data$re78 > 0, ]

ggplot(without_zero, aes(x=age, y=re78, colour=treat)) +
  geom_point() + geom_jitter()

ggplot(without_zero, aes(x=educ, y=re78, colour=treat)) +
  geom_point() + geom_jitter()

# Interactions:
ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~black, ncol=4) # **

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~hispan, ncol=4)

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~married, ncol=4) # not significant

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~nodegree, ncol=4)

# Modeling
baseline_model <- lm(re78 ~ treat + married + black + black*treat, data=data)
summary(baseline_model)  
plot(baseline_model)

# Model selection
data$X <- NULL

null_model <- lm(re78 ~ treat, data=data)
full_model <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree,
                 data=data)
step_model <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)
summary(step_model) # highly non normal residuals...
plot(step_model)

null_model_log <- lm(log(re78) ~ treat, data=data)
full_model_log <- lm(log(re78) ~ treat + age + educ + 
                       black + hisp + married + nodegree,
                     data=data)

step_model_log <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)

incl75 <- lm(re78 ~ . - re74, data=data)
summary(incl75)

data$re78_p <- data$re78 + 1  # many zeros in re78
data$re78_p <- log(data$re78_p)

log_mod <- lm(re78_p ~ . - re74 - re75 - re78, data = data)
summary(log_mod)
plot(log_mod)

# boxcox_trans <- boxcox(full_model,lambda = seq(-5, 5, length = 50))
# lambda_trans <- boxcox_trans$x[boxcox_trans$y == max(boxcox_trans$y)]
# lambda_trans
plot(data$age, full_model$residuals)
plot(data$educ, full_model$residuals)

new_model1 <- lm(re78 ~ treat + married + educ + black, data=data)














