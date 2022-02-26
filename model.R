library(caret)


# Remove correlated variable NUM_POORHLTH
data <- read.csv("C:/Users/terry/Downloads/new.csv")
data <- subset(data, select = -c(NUM_POORHLTH))
attach(data)

# Train Test Split 70-30
set.seed(31061813)
idx <- createDataPartition(y = IS_DIABETIC, p=0.7, list=FALSE)
train <- idx
test <- as.numeric(rownames(data[-idx,]))

# Fit Logistic Regression Model
model <-glm(IS_DIABETIC~.,data=data,family=binomial, subset=train)
summary(model)
coef(model)
