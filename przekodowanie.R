silownia <- read_csv("silownia_new.csv")


silownia_bez_NA <- na.omit(silownia)

  ifelse(silownia_bez_NA$Workout_Type == "Yoga", 1, 
                     ifelse(silownia_bez_NA$Workout_Type == "HIIT", 2, 
                            ifelse(silownia_bez_NA$Workout_Type == "Cardio", 3, 4)))

silownia_bez_NA %>%
  group_by(silownia_bez_NA$Workout_Type) %>%
    filter (silownia_bez_NA$Age)
