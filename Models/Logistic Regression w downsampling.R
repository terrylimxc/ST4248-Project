library(caret)

############# importing data #################
data <- read.csv("cleanwitholdnameswooutlier.csv")
data <- subset(data, select = -c(NUM_POORHLTH, X)) #col X here is the index of each row
data$BMI <- data$BMI/100 #account for the 2dp

#converting the categorical data to factors
str(data)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], 
                                         as.factor)
data$NUM_DRINKSPERWK <- as.integer(data$NUM_DRINKSPERWK)
data$NUM_POORMENTHLTH <- as.integer(data$NUM_POORMENTHLTH)
data$NUM_POORPHYHLTH <- as.integer(data$NUM_POORPHYHLTH)
data$NUM_SLEEP <- as.integer(data$NUM_SLEEP)

attach(data)

#### training the model #####
set.seed(4248)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE) #using IS_DIABETIC.1 because of the dummyVars above. But, IS_DIABETIC.1 == IS_DIABETIC
train <- idx #index of training observations
#test <- as.numeric(rownames(data[-idx,]))

set.seed(4248)  
data_ds <- downSample(x = subset(data[train,], 
                                 select = -c(IS_DIABETIC)),
                      y = data$IS_DIABETIC[train])

library(glmnet)
library(dplyr)
grid<-10^seq(10,-2,length=100)
data <- data %>% rename(Class = IS_DIABETIC)
data_reg <- rbind(data_ds, data[-train,])
xfactors <- model.matrix(Class ~ .-BMI -NUM_DRINKSPERWK -NUM_POORMENTHLTH -NUM_POORPHYHLTH-NUM_SLEEP, data = data_reg)
head(xfactors)
x <- as.matrix(data.frame(data_reg$NUM_DRINKSPERWK, data_reg$NUM_POORMENTHLTH, data_reg$NUM_POORPHYHLTH, data_reg$NUM_SLEEP, data_reg$BMI, xfactors))
lasso.mod<-glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=1,lambda=grid, thresh=1e-12, family = "binomial")

set.seed(4248)
#I think the below part does cross validation on the model
cv.out<-cv.glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=1, family = "binomial")
plot(cv.out)#19 refers to the number of non 0 coef, the opper and bottom lines are like the confidence interval
#This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves along the λ sequence (error bars). Two special values along the λ sequence are indicated by the vertical dotted lines. lambda.min is the value of λ that gives minimum mean cross-validated error, while lambda.1se is the value of λ that gives the most regularized model such that the cross-validated error is within one standard error of the minimum.
# The values at the top signify the number of coefficients that are non-zero

bestlam<-cv.out$lambda.min
bestlam
lasso.pred<-predict(lasso.mod,s=bestlam,newx=x[(nrow(data_ds)+1):nrow(x),], type = "response")
pred <- ifelse(lasso.pred <= 0.5,0, 1)
table(predict=pred, truth = data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
mean(pred == data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
#accuracy = 0.7308702
#recall = 0.7551367437
#precision = 0.3128632654
conf <- table(predict=pred, truth = data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
(conf[1,1]+conf[2,2]) / length(glm.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(data_reg$Class[(nrow(data_ds) + 1):nrow(x)], pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1



lasso.mod<-glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=1,lambda=bestlam, thresh=1e-12, family = "binomial")
lasso.mod$beta





















############# importing data #################
data <- read.csv("cleanwitholdnameswooutlier.csv")
data <- subset(data, select = -c(NUM_POORHLTH, X)) #col X here is the index of each row
data$BMI <- data$BMI/100 #account for the 2dp

#converting the categorical data to factors
str(data)
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], 
                                         as.factor)
data$NUM_DRINKSPERWK <- as.integer(data$NUM_DRINKSPERWK)
data$NUM_POORMENTHLTH <- as.integer(data$NUM_POORMENTHLTH)
data$NUM_POORPHYHLTH <- as.integer(data$NUM_POORPHYHLTH)
data$NUM_SLEEP <- as.integer(data$NUM_SLEEP)



# ############# doing some feature scaling and normalization before fitting the model ##############
# #### conducting one-hot encoding for the categorical var ######
# library(caret)
# library(tibble)
# 
# dmy <- dummyVars(" ~ .", data = data, fullRank = T)
# data_transformed <- data.frame(predict(dmy, newdata = data))
# 
# glimpse(data_transformed)
# 
# #### scaling the continuous features ######
# library(dplyr)
# 
# data_scaled <- data_transformed %>% mutate_at(c("NUM_DRINKSPERWK", "BMI", "NUM_SLEEP", "NUM_POORMENTHLTH", "NUM_POORPHYHLTH"), ~(scale(.) %>% as.vector))
# glimpse(data_scaled)
# cols <- c("NUM_DRINKSPERWK", "NUM_POORMENTHLTH", "NUM_POORPHYHLTH", "NUM_SLEEP", "BMI")
# 
# data_scaled[colnames(data_scaled)[!(colnames(data_scaled) %in% cols)]] <- lapply(data_scaled[colnames(data_scaled)[!(colnames(data_scaled) %in% cols)]], as.factor)
# 
# attach(data_scaled)
#### training the model #####
set.seed(4248)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE) #using IS_DIABETIC.1 because of the dummyVars above. But, IS_DIABETIC.1 == IS_DIABETIC
train <- idx #index of training observations
#test <- as.numeric(rownames(data[-idx,]))

set.seed(4248)  
data_ds <- downSample(x = subset(data[train,], 
                                 select = -c(IS_DIABETIC)),
                      y = data$IS_DIABETIC[train])

# Fit Logistic Regression Model
model <-glm(Class~.,data=data_ds,family=binomial)
summary(model)
coef(model)


#Predicting the validation set + confusion matrix
glm.probs <- predict(model, data_scaled[-train,], type = "response")
glm.pred <- rep("0", 49909) #updated to 49910 cos now the number of rows in the dataframe has reduced
glm.pred[glm.probs>.5] = "1"

table(predict = glm.pred, truth = data_scaled$IS_DIABETIC.1[-train])
mean(glm.pred == data_scaled$IS_DIABETIC.1[-train])

## To-do
#go and check if the accuracy is better and the recall is better anot
#maybe we can adjust the threshold to account for the imbalance classification, if not then use resampling


######## using glmnet (lasso) ##########
library(glmnet)
attach()
grid<-10^seq(10,-2,length=100)
data <- data %>% rename(Class = IS_DIABETIC)
data_reg <- rbind(data_ds, data[-train,])
xfactors <- model.matrix(Class ~ .-BMI -NUM_DRINKSPERWK -NUM_POORMENTHLTH -NUM_POORPHYHLTH-NUM_SLEEP, data = data_reg)
head(xfactors)
x <- as.matrix(data.frame(data_reg$NUM_DRINKSPERWK, data_reg$NUM_POORMENTHLTH, data_reg$NUM_POORPHYHLTH, data_reg$NUM_SLEEP, data_reg$BMI, xfactors))
attach(data)
lasso.mod<-glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=1,lambda=grid, thresh=1e-12, family = "binomial")

set.seed(4248)
#I think the below part does cross validation on the model
cv.out<-cv.glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=1, lambda = grid, family = "binomial")
plot(cv.out) #19 refers to the number of non 0 coef, the opper and bottom lines are like the confidence interval
#This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves along the λ sequence (error bars). Two special values along the λ sequence are indicated by the vertical dotted lines. lambda.min is the value of λ that gives minimum mean cross-validated error, while lambda.1se is the value of λ that gives the most regularized model such that the cross-validated error is within one standard error of the minimum.
# The values at the top signify the number of coefficients that are non-zero

bestlam<-cv.out$lambda.min
bestlam
lasso.pred<-predict(lasso.mod,s=bestlam,newx=x[(nrow(data_ds)+1):nrow(x),], type = "response")
pred <- ifelse(lasso.pred <= 0.5,0, 1)
table(predict=pred, truth = data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
mean(pred == data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
#accuracy = 0.7308702
#recall = 0.7551367437
#precision = 0.3128632654
conf <- table(predict=pred, truth = data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
(conf[1,1]+conf[2,2]) / length(glm.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(data_reg$Class[(nrow(data_ds) + 1):nrow(x)], pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1

# grid<-10^seq(10,-2,length=100) #seq is a sequence of values from 10 to -2 and there are 100 equally spaced values
# cv.out<-cv.glmnet(x,data_ds$Class,alpha=0, family = "binomial")
# bestlam<-cv.out$lambda.min
# ridge.mod<-glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=0,lambda=grid, thresh=1e-12, family = "binomial")
# ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[(nrow(data_ds) + 1):nrow(x),], type = "response")
# ridge.pred
# 
# glmmod <- glmnet(x[1:nrow(data_ds),], y=data_ds$Class, alpha = 1, family = "binomial") #standardize = True by default
# plot(glmmod, xvar="lambda")
# 
# 
# mean((ridge.pred-y.test)^2)
# out<-glmnet(x,y,alpha=0)
# predict(out,type="coefficients",s=bestlam)[1:20,]
# 
# 
# cv.glmmod <- cv.glmnet(x[train,], y=as.numeric(IS_DIABETIC)[train], alpha=1)
# plot(cv.glmmod)
# bestlam<-cv.glmmod$lambda.min
# 
# ridge.pred<-predict(glmmod,s=bestlam,newx=x[-train,], type = "response")
# pred <- ifelse(ridge.pred <= 0.5,0, 1)
# table(predict=pred, truth=IS_DIABETIC.1[-train])
# mean(pred == IS_DIABETIC.1[-train])

######## using glmnet (ridge) ##########
# glmmod <- glmnet(x[train,], y=IS_DIABETIC[train], alpha = 0, family = "binomial") #standardize = True by default
# plot(glmmod, xvar="lambda")
# 
# cv.glmmod <- cv.glmnet(x[train,], y=as.numeric(IS_DIABETIC)[train], alpha=0)
# plot(cv.glmmod)
# bestlam<-cv.glmmod$lambda.min
# 
# ridge.pred<-predict(glmmod,s=bestlam,newx=x[-train,], type = "response")
# pred <- ifelse(ridge.pred <= 0.5,0, 1)
# table(predict=pred, truth=IS_DIABETIC[-train])
# mean(pred == IS_DIABETIC[-train])
ridge.mod<-glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=0,lambda=grid, thresh=1e-12, family = "binomial")

set.seed(4248)
#I think the below part does cross validation on the model
cv.out<-cv.glmnet(x[1:nrow(data_ds),],data_ds$Class,alpha=0, family = "binomial")
plot(cv.out) #19 refers to the number of non 0 coef, the opper and bottom lines are like the confidence interval
#This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves along the λ sequence (error bars). Two special values along the λ sequence are indicated by the vertical dotted lines. lambda.min is the value of λ that gives minimum mean cross-validated error, while lambda.1se is the value of λ that gives the most regularized model such that the cross-validated error is within one standard error of the minimum.
# The values at the top signify the number of coefficients that are non-zero

bestlam<-cv.out$lambda.min
bestlam
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[(nrow(data_ds)+1):nrow(x),], type = "response")
pred <- ifelse(ridge.pred <= 0.5,0, 1)
table(predict=pred, truth = data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
mean(pred == data_reg$Class[(nrow(data_ds) + 1):nrow(x)])
#accuracy = 0.7286862
#recall = 0.7697321808
#precision = 0.313119668


############ Using glmnet to try to do hyperparam tuning ####################
library(tidymodels)
library(glmnet)

# show tunable hyperparameters for various logistic regression functions
show_model_info("logistic_reg")
#> Information for `logistic_reg`
#>  modes: unknown, classification 
#> 
#>  engines: 
#>    classification: glm, glmnet, keras, LiblineaR, spark, stan
#> 
#>  arguments: 
#>    glmnet:    
#>       penalty --> lambda
#>       mixture --> alpha
#>    LiblineaR: 
#>       penalty --> cost
#>       mixture --> type
#>    spark:     
#>       penalty --> reg_param
#>       mixture --> elastic_net_param
#>    keras:     
#>       penalty --> penalty
#> 
#>   [fit and prediction modules omitted to be more concise]


#glmnet is for elastic net regression. This penaizes the size of the estiamted coefficients, something not present in glm
mod <- logistic_reg(mode = "classification", engine = "glmnet", #use glmnet instead of glm because there are no hyperparams for us to tune for glmn
                    penalty = tune(), mixture = tune())

## specify recipe: model formula and preprocessing steps (if any)
rec <- recipe(IS_DIABETIC ~ ., data = data)

## specify workflow (recipe and model specification)
w <- workflow(preprocessor = rec, spec = mod)

## specify resamples for cross-validation
set.seed(28483)
r <- vfold_cv(data = data, v = 5, repeats = 1, strata = "IS_DIABETIC") #v is the number of folds

## specify tuning grid (hyperparameter search space)
g <- expand_grid(penalty = c(0,1,2),
                 mixture = seq(0,1,by=0.2))

## tune logistic regression model
fit_tune <- tune_grid(w, resamples = r, grid = g)


tune.met <- collect_metrics(fit_tune)
tune.met

ggplot(tune.met, aes(x = mixture, y = mean, colour = factor(penalty))) +
  geom_line() +
  facet_wrap(~.metric) +
  theme_bw()