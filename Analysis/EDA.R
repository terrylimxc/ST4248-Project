library(tidyverse)

data <- read_csv("updated clean+outliersremoved.csv", col_types='-ifffffffiifffffiff-iff')
data$IS_DIABETIC <- factor(data$IS_DIABETIC, levels = c("0","1"), labels=c("0","1"))


summary(data)
dim(data)
# we see that more likely to be diabetic when bmi is higher
ggplot(data) +
  geom_boxplot(aes(x=IS_DIABETIC, y=BMI))
# smoker type: type 1 = more likely compared to other groups
ggplot(data) +
  geom_bar(aes(x=SMOKER_TYPE, fill=IS_DIABETIC), position='fill')

ggplot(data) +
  geom_freqpoly(aes(x = BMI, colour=IS_DIABETIC))

factplot <- function(var){
  ggplot(data) +
    geom_bar(aes(x=eval(parse(text=var)), fill=IS_DIABETIC), position='fill') +
    xlab(var) + 
    ylab("Proportion of diabetes")+ scale_fill_discrete(name="Diabetes Status:", labels = c("= No Diabetes", "= Diabetes")) + theme(legend.position="bottom")
}

# stroke
factplot("HAS_STROKE", data) + scale_fill_discrete(name="Diabetes", labels = c("1 = Diabetes", "0 = No Diabetes")) + theme(legend.position="bottom")

# heart disease (1 for heart disease is more likely to have diabetes)
factplot("HAS_HD")

# physical activity (0 for physical activity is more likely to have diabetes)
factplot("HAS_PHYACT") + scale_x_discrete(limits=c("0", "1"))

# health plan (1 for healthplan more likely to have diabetes)
factplot("HAS_HLTHPLAN")\

var <- c('HAS_MONEYPROB', 'GENHLTH_LVL', 'HAS_DIFFWALK', 'SEX', 'AGEGRP', 'INCOMEGRP', 'MARITALGRP', 'CHECKUP', 'RACE', 'HAS_ECIG')

for (var in varnames){
  ggplot(data) +
    geom_bar(aes(x=var, fill=IS_DIABETIC), position='fill')
}
factplot(var[1])
factplot(var[2])+ scale_x_discrete(limits=c("0", "1","2","3","4"))
factplot(var[3]) 
factplot(var[4])
factplot(var[5]) + scale_x_discrete(limits=c("0", "1","2","3","4","5",'6','7','8','9','10','11','12'))
factplot(var[6]) + scale_x_discrete(limits=c("0", "1","2","3","4"))
factplot(var[7])
factplot(var[8]) + scale_x_discrete(limits=c("0", "1","2","3","4"))
factplot(var[9])
factplot(var[10])

ggplot(data) +
  geom_boxplot(aes(x=IS_DIABETIC, y=NUM_POORMENTHLTH))

ggplot(data) +
  geom_boxplot(aes(x=IS_DIABETIC, y=NUM_POORPHYHLTH))

ggplot(data) +
  geom_boxplot(aes(x=IS_DIABETIC, y=NUM_SLEEP))

ggplot(data) +
  geom_boxplot(aes(x=IS_DIABETIC, y=NUM_DRINKSPERWK))

ggplot(data) +
  geom_bar(aes(x=AGEGRP, fill=IS_DIABETIC))
