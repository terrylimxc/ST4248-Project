# Run Line 2 to install the packages
install.packages(c("corrplot", "rcompanion", "psych", "ltm"))

# Import libraries
library(readxl)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(psych)
library(ltm)

data <- read.csv("C:/Users/terry/Downloads/updated clean+outliersremoved.csv")

cols <- c("SMOKER_TYPE", "HAS_STROKE", "HAS_HD", "HAS_PHYACT", "HAS_HLTHPLAN", 
          "HAS_MONEYPROB", "GENHLTH_LVL", "HAS_DIFFWALK", "SEX", "AGEGRP", 
          "INCOMEGRP", "MARITALGRP", "CHECKUP", "RACE", "HAS_ECIG")

data[cols] <- lapply(data[cols], as.factor)


# Check for correlation between continuous variables using Pearson Correlation
data_con <- data[c("BMI", "NUM_POORMENTHLTH", "NUM_POORPHYHLTH", "NUM_SLEEP", 
                   "NUM_POORHLTH", "NUM_DRINKSPERWK")]

corrplot(cor(data_con), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

## Moderate correlation between NUM_POORHLTH and NUM_POORPHYHLTH (0.611) 
## and between NUMPOORHLTH and NUM_POORMENTHLTH (0.451)

### Proposed to remove NUM_POORHLTH


# Check for correlation between categorical variables (non-binary)
cols <- c("SMOKER_TYPE", "GENHLTH_LVL", "AGEGRP", "INCOMEGRP", "MARITALGRP", 
          "CHECKUP", "RACE", "HAS_ECIG")

data_cat <- data[cols]
pairs <- t(combn(cols,2))
num_rows <- length(pairs)/2
cramer_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  cramer_vals[i] <- cramerV(data_cat[[row[1]]], data_cat[[row[2]]])  
}

cramer_vals[cramer_vals >= 0.5]
## All the Cramer's V values are < 0.5 and are close to 0. Thus, variables are not correlated.


# Check for correlation between categorical variables (binary)
cols2 <- c("HAS_STROKE", "HAS_HD", "HAS_PHYACT", "HAS_HLTHPLAN", "HAS_MONEYPROB", 
          "HAS_DIFFWALK", "SEX", "IS_DIABETIC")

data_cat2 <- data[cols2]
pairs <- t(combn(cols2,2))
num_rows <- length(pairs)/2
phi_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  temptable <- table(data_cat2[[row[1]]], data_cat2[[row[2]]])
  phi_vals[i] <- phi(temptable)  
}
phi_vals[phi_vals >= 0.5]
phi_vals[phi_vals <= -0.5]
## All the Phi Coefficients values are < 0.5 or > -0.5 and are close to 0. 
## Thus, variables are not correlated.

# Check for correlation between continuous and binary categorical variables
pairs <- t(combn(colnames(data)[!colnames(data) %in% cols],2))
num_rows <- length(pairs)/2
pb_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  if ((row[1] %in% cols) | (row[1] %in% cols2)){ # 1st var categorical
    if ((row[2] %in% cols) | (row[2] %in% cols2)){ # Both categorical
      pb_vals[i] <- NA
    }
    else { # 1st categorical, 2nd continuous
      pb_vals[i] <- biserial.cor(as.numeric(data[[row[2]]]), as.numeric(data[[row[1]]]))
    }
  }
  else { # 1st var continuous
    if ((row[2] %in% cols) | (row[2] %in% cols2)){ # 1st continuous, 2nd categorical
      pb_vals[i] <- biserial.cor(as.numeric(data[[row[1]]]), as.numeric(data[[row[2]]]))
    }
    else{ # Both continuous
      pb_vals[i] <- NA
    }
  }
}
pb_vals[pb_vals >= 0.5]
pb_vals[pb_vals <= -0.5]

## All good



