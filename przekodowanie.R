silownia <- read_csv("silownia_new.csv")


silownia_bez_NA <- na.omit(silownia)

przekodowne_ <- ifelse(is.na(silownia_bez_NA$Workout_Type), NA, 
            ifelse(silownia_bez_NA$Workout_Type == "Yoga"), 1, 
                   ifelse(silownia_bez_NA$Workout_Type == "HIIT"), 2, 
                          ifelse(silownia_bez_NA$Workout_Type == "Cardio"), 3, 4))
