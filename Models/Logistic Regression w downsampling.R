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
