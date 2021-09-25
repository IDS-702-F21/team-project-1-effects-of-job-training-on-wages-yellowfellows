rm(list = ls())

library(ggplot2)
library(MASS)
library(rms)


##########################################################################
############################ 0. Loading the data #########################
##########################################################################


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


##########################################################################
############################### 1. EDA ###################################
##########################################################################


# Single variables
ggplot(data, aes(x=re78)) + geom_histogram() # skewed to the right
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
  facet_wrap(~married, ncol=4)

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~nodegree, ncol=4)


##########################################################################
############################### 2.Model selection ########################
##########################################################################


# Baseline model based on the EDA
baseline_model <- lm(re78 ~ treat + married + black + black*treat, data=data)
summary(baseline_model)  
plot(baseline_model)

# Model selection
# Step AIC:
null_model <- lm(re78 ~ treat, data=data)
full_model <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree,
                 data=data)
step_model <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)

# Forward AIC:
forward_model <- step(null_model,
                      scope=formula(full_model),
                      direction='forward',
                      trace=0)
summary(forward_model) # The same as stepwise

# Backward AIC:
backward_model <- step(null_model,
                       scope=formula(full_model),
                       direction='backward',
                       trace=0)
summary(backward_model) # Constant model without variables

# Step with interaction terms:
full_model2 <- lm(re78 ~ treat + age + educ + black +
                    hispan + married + nodegree + 
                    treat*black + treat*hispan,
                  data=data)
step_model2 <- step(null_model,
                    scope=formula(full_model2),
                    direction='both',
                    trace=0)
summary(step_model2)    # Interaction term wasn't chosen


##########################################################################
############################### 3.Model assessment #######################
##########################################################################


summary(step_model)                   
plot(step_model, which=1)             # normality assumption?
plot(step_model, which=2)
plot(step_model, which=3)
plot(step_model, which=4)             # no influential points
plot(step_model, which=5)             # several outliers
plot(data$educ, step_model$residuals) # linearity assumption holds
plot(data$age, step_model$residuals)
vif(step_model)                       # no multicollinearity


##########################################################################
############################### 4. Inference #############################
##########################################################################


confint(step_model)
summary(step_model)


##########################################################################
############################### 5. Experiments ###########################
##########################################################################


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

boxcox_trans <- boxcox(step_model, lambda = seq(1, 5, length = 50))
lambda_trans <- boxcox_trans$x[boxcox_trans$y == max(boxcox_trans$y)]
lambda_trans


plot(data$age, full_model$residuals)
plot(data$educ, full_model$residuals)

new_model1 <- lm(re78 ~ treat + married + educ + black, data=data)














