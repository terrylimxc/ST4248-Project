library(caret)


# Remove correlated variable NUM_POORHLTH
data <- read.csv("new.csv")
data <- subset(data, select = -c(NUM_POORHLTH, ID))
attach(data)

# Train Test Split 70-30
set.seed(31061813)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE)
train <- idx #index of training observations
#test <- as.numeric(rownames(data[-idx,]))

# Fit Logistic Regression Model
model <-glm(IS_DIABETIC~.,data=data,family=binomial, subset=train)
summary(model)
coef(model)

#Predicting the validation set + confusion matrix
glm.probs <- predict(model, data[-train,], type = "response")
glm.pred <- rep("0", 72318)
glm.pred[glm.probs>.5] = "1"

table(glm.pred, IS_DIABETIC[-train])
mean(glm.pred == IS_DIABETIC[-train]) #this function returns the accuracy of the model on the validation set
#Accuracy of baseline model = 0.864