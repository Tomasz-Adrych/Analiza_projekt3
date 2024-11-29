install.packages("dlookr")

library(tidyverse)
library(readr)
library(dlookr)

silownia <- read_csv("silownia_new.csv", show_col_types = FALSE)

#W grudniu być gotowym do analizy
#Workout_type pomysł 1

 ifelse(silownia_bez_NA$Workout_Type == "Yoga", 1, 
                   ifelse(silownia_bez_NA$Workout_Type == "HIIT", 2, 
                            ifelse(silownia_bez_NA$Workout_Type == "Cardio", 3, 4)))


#Age pomysł 1
 age1 <- round (imputate_na(silownia, Age, Gender, method = "rpart"))
 summary(age1)

 plot(age1)

ramka <- replace_na(data.frame(silownia, age1))
age_test <- ramka %>% 
  mutate(Age = age1) %>%
    select(-age1)

#BMI pomysł 1
#stworzyć funkcje która luczy BMI, użyć do replace_NA
