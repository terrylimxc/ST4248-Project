library(readxl)
library(tidyverse)

data <- read.csv("C:/Users/terry/Downloads/clean.csv")

names(data)
data[, c('DIABETE4' ,'_BMI5', 'SMOKE100', 'CVDSTRK3', '_MICHD', '_TOTINDA', '_RFDRHV7', 'HLTHPLN1', 'MEDCOST', 'GENHLTH', 'MENTHLTH', 'PHYSHLTH', 'DIFFWALK', 'COLGSEX', '_AGEG5YR', 'INCOME2', 'MARITAL', 'SLEPTIM1', 'CHECKUP1', '_IMPRACE')]

str(data)
extracted <- data[, c("DIABETE4", "X_BMI5", "X_SMOKER3", "CVDSTRK3", "X_MICHD", "X_TOTINDA", "HLTHPLN1", "MEDCOST", "GENHLTH", "MENTHLTH", "PHYSHLTH", "DIFFWALK", "X_SEX", "X_AGEG5YR", "X_INCOMG", "MARITAL", "SLEPTIM1", "CHECKUP1", "X_IMPRACE", "POORHLTH", "X_DRNKWK1", "ECIGNOW")]

extracted <- mutate(extracted, ECIGNOW = if_else(is.na(ECIGNOW), 4, ECIGNOW)) %>% mutate(POORHLTH = if_else(is.na(POORHLTH), 88, POORHLTH))

drop <- extracted %>% drop_na()

# need to drop all the 7, 9, 77, 99 then need to change some of the values to 0 and combine categories

# drop values larger than 7
drop <- drop[drop$DIABETE4 < 7, ]

# drop those who dont know if they had a stroke or refused

clean <- drop %>% filter(X_SMOKER3 < 9 & CVDSTRK3 < 7 & X_TOTINDA < 9  & HLTHPLN1<7 & MEDCOST<7 & GENHLTH<7 & MENTHLTH != 77 & MENTHLTH != 99 & PHYSHLTH != 77 & PHYSHLTH != 99 & DIFFWALK<7 & X_AGEG5YR < 14 & X_INCOMG < 9 & MARITAL<9 & SLEPTIM1 < 77 & CHECKUP1 != 7 & CHECKUP1!=9 & POORHLTH!=77 & POORHLTH!=99 & X_DRNKWK1 != 99900 & ECIGNOW <7)

# combine diabetes
clean <- clean %>% mutate(IS_DIABETIC = if_else(DIABETE4 < 2, 1, 0))

# [1] "DIABETE4"    "X_BMI5"      "X_SMOKER3"  
# [4] "CVDSTRK3"    "X_MICHD"     "X_TOTINDA"  
# [7] "HLTHPLN1"    "MEDCOST"     "GENHLTH"    
# [10] "MENTHLTH"    "PHYSHLTH"    "DIFFWALK"   
# [13] "X_SEX"       "X_AGEG5YR"   "X_INCOMG"   
# [16] "MARITAL"     "SLEPTIM1"    "CHECKUP1"   
# [19] "X_IMPRACE"   "POORHLTH"    "X_DRNKWK1"  
# [22] "ECIGNOW"     "IS_DIABETIC"

# change the values to 0 if none
clean <- clean %>% mutate(
  CVDSTRK3 = if_else(CVDSTRK3 == 2, 0, CVDSTRK3),
  X_MICHD = if_else(X_MICHD == 2, 0, X_MICHD),
  X_SMOKER3 = if_else(X_SMOKER3 == 4, 0, if_else(X_SMOKER3 == 3, 1, if_else(X_SMOKER3 == 2, 2, 3))),
  X_TOTINDA = if_else(X_TOTINDA == 2, 0, X_TOTINDA),
  HLTHPLN1 = if_else(HLTHPLN1 == 2, 0, HLTHPLN1),
  MEDCOST = if_else(MEDCOST == 2, 0, MEDCOST),
  GENHLTH = if_else(GENHLTH == 2, 0, GENHLTH),
  MENTHLTH = if_else(MENTHLTH == 88, 0, MENTHLTH),
  PHYSHLTH = if_else(PHYSHLTH == 88, 0, PHYSHLTH),
  DIFFWALK = if_else(DIFFWALK == 2, 0, DIFFWALK),
  X_SEX = if_else(X_SEX == 2, 0, X_SEX),
  CHECKUP1 = if_else(CHECKUP1 == 8, 0, CHECKUP1),
  POORHLTH = if_else(POORHLTH == 88, 0, POORHLTH),
  ECIGNOW = if_else(ECIGNOW == 4, 0, if_else(ECIGNOW == 3, 1, if_else(ECIGNOW == 2, 2, 3))),
  X_AGEG5YR = X_AGEG5YR - 1,
  X_INCOMG = X_INCOMG - 1,
  MARITAL = MARITAL - 1,
  X_IMPRACE = X_IMPRACE - 1
)

# try minus 1 for X_AGEG5YR, X_INCOMG, MARITAL, X_IMPRACE, 
write.csv(clean, 'C:/Users/chewb/Desktop/New folder/Y3S2/ST4248/project/clean.csv')
