library(tidyverse)

data <- read_csv("updated clean+outliersremoved.csv", col_types='-ifffffffiifffffiff-iff')
data$IS_DIABETIC <- factor(data$IS_DIABETIC, levels = c("0","1"), labels=c("0","1"))

# Train Test Split 70-30
set.seed(4248)
library(caret)
library(gam)
idx <- createDataPartition(y = data$IS_DIABETIC, p=0.7, list=FALSE)
train <- idx #index of training observations
summary(data[train,])
16469/(16469+99989)
summary(data[-train,])
7057/(7057+42852)

names<-colnames(downdata)
names<-c(names[1:length(names)-1],'IS_DIABETIC')
names

# downsampling train set
set.seed(4248)
downdata <- downSample(x=subset(data[train,], select=-c(IS_DIABETIC)), y=data[train,]$IS_DIABETIC)
colnames(downdata) <- names
summary(downdata)

# try forward stepwise selection using BIC as criteria
library(MASS)
step.fwdbic <- stepAIC(glm(IS_DIABETIC~., data=data[train,], family='binomial'), trace=FALSE, k=log(length(train)))
coef(step.fwdbic)

# fit glm model using selected variables
fwdbic.fit <- glm(IS_DIABETIC~BMI+HAS_STROKE+HAS_HD+HAS_PHYACT+HAS_MONEYPROB+GENHLTH_LVL+HAS_DIFFWALK+SEX+AGEGRP+CHECKUP+RACE+NUM_DRINKSPERWK, data=data[train,], family='binomial')
summary(fwdbic.fit)
fwdbic.prob <- predict(fwdbic.fit, newdata=data[-train,], type='response')
fwdbic.preds <- ifelse(fwdbic.prob>0.16, 1, 0)
res <- table(prediction=fwdbic.preds,truth=data$IS_DIABETIC[-train])
res
#accuracy
(res[1,1] + res[2,2])/(res[1,1]+res[1,2]+res[2,1]+res[2,2])
#precision
res[2,2]/(res[2,2]+res[2,1])
#recall
res[2,2]/(res[2,2]+res[1,2])
#f1
2*(res[2,2]/(res[2,2]+res[1,2]) * res[2,2]/(res[2,2]+res[2,1]))/ (res[2,2]/(res[2,2]+res[1,2]) + res[2,2]/(res[2,2]+res[2,1]))
# auc
pROC::auc(data$IS_DIABETIC[-train],as.numeric(fwdbic.preds))

# fwdbic on undersampled data
step.fwdbicds <- stepAIC(glm(IS_DIABETIC~., data=downdata, family='binomial'), trace=FALSE, k=log(nrow(downdata)))

coef(step.fwdbicds)
fwdbicds.probs <- predict(step.fwdbicds, newdata=data[-train,], type='response')
fwdbicds.preds <- ifelse(fwdbicds.probs > 0.5, 1, 0)
res <- table(prediction=fwdbicds.preds,truth=data$IS_DIABETIC[-train])
res
#accuracy
(res[1,1] + res[2,2])/(res[1,1]+res[1,2]+res[2,1]+res[2,2])
#precision
res[2,2]/(res[2,2]+res[2,1])
#recall
res[2,2]/(res[2,2]+res[1,2])
#f1
2*(res[2,2]/(res[2,2]+res[1,2]) * res[2,2]/(res[2,2]+res[2,1]))/ (res[2,2]/(res[2,2]+res[1,2]) + res[2,2]/(res[2,2]+res[2,1]))
# auc
pROC::auc(data$IS_DIABETIC[-train],as.numeric(fwdbicds.preds))

fwd.fit <- glm(IS_DIABETIC~.-HAS_ECIG-NUM_POORMENTHLTH-NUM_SLEEP, data=data[train,], family='binomial')
summary(fwd.fit)
fwd.prob <- predict(fwd.fit, newdata=data[-train,], type='response')
fwd.preds <- ifelse(fwd.prob>0.2, 1, 0)
res <- table(prediction=fwd.preds,truth=data$IS_DIABETIC[-train])
res
#accuracy
(res[1,1] + res[2,2])/(res[1,1]+res[1,2]+res[2,1]+res[2,2])
#precision
res[2,2]/(res[2,2]+res[2,1])
#recall
res[2,2]/(res[2,2]+res[1,2])
#f1
2*(res[2,2]/(res[2,2]+res[1,2]) * res[2,2]/(res[2,2]+res[2,1]))/ (res[2,2]/(res[2,2]+res[1,2]) + res[2,2]/(res[2,2]+res[2,1]))
# auc
pROC::auc(data$IS_DIABETIC[-train],as.numeric(preds))