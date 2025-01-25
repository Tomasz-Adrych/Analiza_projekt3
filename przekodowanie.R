install.packages("dlookr")

library(tidyverse)
library(readr)
library(dlookr)

silownia <- read_csv("silownia_new.csv", show_col_types = FALSE)

#W grudniu być gotowym do analizy
#Workout_type pomysł 1


#Age pomysł 1
 age1 <- round (imputate_na(silownia, Age, Gender, method = "rpart"))
 summary(age1)

plot(age1)

silownia <- replace_na(data.frame(silownia, age1))
silownia <- silownia %>% 
  mutate(Age = age1) %>%
    select(-age1)


