library(caret)
library(ModelMetrics)

# Remove correlated variable NUM_POORHLTH
#data <- read.csv("new.csv")
#data <- subset(data, select = -c(NUM_POORHLTH, ID))
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


# Train Test Split 70-30
set.seed(4248)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE)
train <- idx #index of training observations
#test <- as.numeric(rownames(data[-idx,]))

# Fit Logistic Regression Model
model <-glm(IS_DIABETIC~.,data=data,family=binomial, subset=train)
summary(model)
coef(model)


#Predicting the validation set + confusion matrix
glm.probs <- predict(model, data[-train,], type = "response")
glm.pred <- rep("0", 49909) #updated to 49909 cos now the number of rows in the dataframe has reduced
glm.pred[glm.probs>.5] = "1"

table(glm.pred, data$IS_DIABETIC[-train])
table(predict=glm.pred, truth=IS_DIABETIC [-train])
mean(glm.pred == IS_DIABETIC[-train]) #this function returns the accuracy of the model on the validation set
#Accuracy of baseline model = 0.8617484
conf <- table(predict=glm.pred, truth=IS_DIABETIC [-train])
(conf[1,1]+conf[2,2]) / length(glm.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(IS_DIABETIC[-train], as.factor(glm.pred))
f1 <- 2*(precision*recall)/ (precision + recall)
f1
