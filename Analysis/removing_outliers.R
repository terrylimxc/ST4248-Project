library(ggplot2)
library(reshape)

data <- read.csv("clean.csv")
str(data)
#x_bmi5 is considered continuous feature
#sleptim1, poorhlth, drnkwk1

#menthlth, physhlth, sleeptim1, poorhlth, drnkwk1m, x_bmi5

# x <- boxplot(data$BMI, ylab = "BMI")
# title("Boxplot of BMI")
# length(x$out) #x$out refers to the the values of the outliers
# x_indices <- which(data$BMI %in% x$out) #this refers to the indices

y <- boxplot(data$NUM_DRINKSPERWK, ylab = "Alcoholic drinks per week")
title("Boxplot of Alcoholic drinks per week")
length(unique(y$out))
y_indices <- which(data$NUM_DRINKSPERWK %in% y$out)
length(y_indices)
boxplot(data$NUM_DRINKSPERWK)$stats[c(1, 5), ] #this is the upper value of the whisker from the boxplot

nrow(data)
data_wo_outlier <- data[-y_indices,]
nrow(data_wo_outlier)
nrow(data) - nrow(data_wo_outlier)

write.csv(data_wo_outlier, "cleanwitholdnameswooutlier.csv")

data[y_indices,]
