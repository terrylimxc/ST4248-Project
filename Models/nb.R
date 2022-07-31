library(caret)

data <- read.csv("C:/Users/terry/Downloads/updated clean+outliersremoved.csv")


data <- read.csv("C:/Users/terry/Downloads/data_scaled.csv")
data <- data[-c(1)] # Remove column X
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], 
                                         as.factor)
data$NUM_DRINKSPERWK <- as.integer(data$NUM_DRINKSPERWK)
data$NUM_POORMENTHLTH <- as.integer(data$NUM_POORMENTHLTH)
data$NUM_POORPHYHLTH <- as.integer(data$NUM_POORPHYHLTH)
data$NUM_SLEEP <- as.integer(data$NUM_SLEEP)

attach(data)

# Train Test Split 70-30
set.seed(4248)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE)
#train <- data[idx,]
train <- idx

#set.seed(4248) 
#train <- downSample(x = train[, !(names(train) %in% c("IS_DIABETIC.1"))],
#                   y = as.factor(train$IS_DIABETIC.1))

set.seed(4248)  
train <- downSample(x = subset(data[train,], 
                                 select = -c(IS_DIABETIC)),
                      y = as.factor(data$IS_DIABETIC[train]))

test <- data[as.numeric(rownames(data[-idx,])),]




#naive bayes
library(e1071)
my.nb = naiveBayes(Class~., data=train)
nb.yhat <- predict(my.nb,newdata=test[,-which(names(test) %in% c("IS_DIABETIC"))])
table(nb.yhat, test$IS_DIABETIC)

library(ModelMetrics)

# Scaled
# Accuracy
#(32256+4653)/(32256+4653+2422+10596) = 0.7392593
# Recall
#(4653)/(4653+2422) = 0.6576678
# Precision
#(4653)/(4653+10596) = 0.3051348
# F1 Score
# (2*0.3051348*0.6576678)/(0.3051348+0.6576678) = 0.4168608
# AUC
auc(test$IS_DIABETIC, nb.yhat) # AUC: 0.7075377

# Unscaled
# Accuracy
# (32591+4619)/(32591+4619+2438+10261) = 0.7455569
# Recall
#(4619)/(4619+2438) = 0.6545274
# Precision
#(4619)/(4653+10261) = 0.309709
# F1 Score
# (2*0.309709*0.6545274)/(0.309709+0.6545274) = 0.4204633
# AUC
auc(test$IS_DIABETIC, nb.yhat) # AUC: 0.7047625

for(i in 1:11){
  temp = naiveBayes(Class~., data=train, laplace = i-1)
  pred <- predict(temp,newdata=test[,-which(names(test) %in% c("IS_DIABETIC.1"))])
  print(confusionMatrix(pred, as.factor(test$IS_DIABETIC.), mode="everything"))
}

# Recall
# 5696/ (5696+1423) = 0.8001124
# Accuracy
# (25543 + 5696) / (25543 + 5696 + 17248 + 1423) = 0.6259066
# Precision
# 5695 / (5696+17248) = 0.2482566
# F1 Score
# (2*0.248213*0.8001124) / (0.248213+0.8001124) = 0.3788867

# naive bayes with kernel
kernel.nb = naiveBayes(Class~., data=train, usekernel=TRUE, poisson=TRUE)
kernel.nb.yhat <- predict(kernel.nb,newdata=test[,-which(names(test) %in% c("IS_DIABETIC.1"))])
table(kernel.nb.yhat, test$IS_DIABETIC.)
confusionMatrix(kernel.nb.yhat, as.factor(test$IS_DIABETIC.), mode="everything")

# naive bayes with cv
cv.nb = caret::train(train[-c(53)],train$Class,'nb',trControl=trainControl(method='cv',number=5))


# LDA
library(MASS)
lda.fit<-lda(Class~.,data=train)
lda.fit
plot(lda.fit)
lda.pred<-predict(lda.fit, test)
lda.class<-lda.pred$class
table(lda.class,test$IS_DIABETIC.1)
# Accuracy
#(30391+5534)/(30391+5534+1523+12461) = 0.7198101

# Recall
#(5534)/(5534+1523) = 0.7841859

# Precision
#(5534)/(5534+12461) = 0.3075299

# F1 Score
# (2*0.3075299*0.7841859)/(0.3075299+0.7841859) = 0.4418011
auc(test$IS_DIABETIC., lda.class) # AUC: 0.7466972

# QDA
qda.fit<-qda(Class~.,data=train)
qda.class<-predict(qda.fit,test)$class
table(qda.class,test$IS_DIABETIC.1)
# Accuracy
#(23510+6102)/(23510+6102+955+19342) = 0.5933198

# Recall
#(6102)/(6102+955) = 0.8646734

# Precision
#(6102)/(6102+19342) = 0.2398208

# F1 Score
# (2*0.2398208*0.8646734)/(0.2398208+0.8646734) = 0.3754962
auc(test$IS_DIABETIC., qda.class) # AUC: 0.7066529

