#Reguły walidacyjne dla zmiennych 

#Age: Zmienna pomiędzy 18 a 100.
#Płeć: 0 lub 1.
#Waga: min 35, maks 200. 
#Wzrost: min 1 m, maks 2,5 m.
#Maksymalne tętno (uderzenia na minutę) podczas sesji treningowych: 
#min 30, 
#maks 220-wiek. 
#Średnie tętno podczas sesji treningowych: 
#min 30, 
#dla cardio maks 0,7*maks maksymalnego tętna,
#dla yogi maks 0,6*maks maksymalnego tętna,
#dla hiit maks 0,75*maks maksymalnego tętna,
#dla siłowego maks 08*maks maksymalnego tętna.
#Tętno w spoczynku przed treningiem: min 30, maks 100.
#Czas trwania każdej sesji treningowej w godzinach: od 0 do 24.
#Całkowita liczba kalorii spalonych podczas każdej sesji: 
#min 0,
#dla cardio maks 500 na pół godziny, 
#dla yogi maks 500 na godzinę, 
#dla hiit maks 500 na pół godziny,
#dla siłowego maks 300 na godzinę.
#Rodzaj wykonanego treningu: Strength, Yoga, HIIt, Cardio. 
#Procent tkanki tłuszczowej użytkownika:
#dla mężczyzn min 6%,
#dla kobiet min 12%.
#Dzienne spożycie wody podczas treningu: mask 5 litrów.
#Liczba sesji treningowych w tygodniu: od 0 do 7.
#Poziom doświadczenia: od 1 do 3.
#BMI: iloraz wagi i wzrostu


library(dplyr)
library(janitor)

# 
silownia <- read.csv("silownia_new.csv")
silownia <- janitor::clean_names(silownia)

# Reguły walidacyjne
validate_silownia <- function(silownia) {
  silownia %>%
    mutate(
      age_valid = ifelse(is.na(age) | (age >= 18 & age <= 100), TRUE, FALSE),
      gender_valid = ifelse(gender %in% c("Male", "Female"), TRUE, FALSE),
      weight_valid = ifelse(weight_kg > 35 & weight_kg <= 200, TRUE, FALSE),
      height_valid = ifelse(height_m >= 1 & height_m <= 2.5, TRUE, FALSE),
      max_bpm_valid = ifelse(max_bpm >= 30 & max_bpm <= 220, TRUE, FALSE),
      avg_bpm_valid = ifelse(avg_bpm <= max_bpm & avg_bpm > resting_bpm, TRUE, FALSE),
      resting_bpm_valid = ifelse(resting_bpm >= 30 & resting_bpm <= 100, TRUE, FALSE),
      session_duration_valid = ifelse(session_duration_hours > 0 & session_duration_hours <= 12, TRUE, FALSE),
      calories_burned_valid = ifelse(calories_burned > 0, TRUE, FALSE),
      workout_type_valid = ifelse(is.na(workout_type) | workout_type %in% c("Yoga", "Cardio", "Strength", "HIIT"), TRUE, FALSE),
      fat_percentage_valid = ifelse(fat_percentage >= 6 & fat_percentage <= 100, TRUE, FALSE),
      water_intake_valid = ifelse(water_intake_liters >= 0.5 & water_intake_liters <= 5, TRUE, FALSE),
      workout_frequency_valid = ifelse(workout_frequency_days_week >= 0 & workout_frequency_days_week <= 7, TRUE, FALSE),
      experience_level_valid = ifelse(experience_level >= 1 & experience_level <= 3, TRUE, FALSE),
      bmi_valid = ifelse(is.na(bmi) | (bmi > 10 & bmi <= 50), TRUE, FALSE)
    )
}

# Zastosowanie reguł walidacyjnych
validated_silownia <- validate_silownia(silownia)

# Sprawdzenie 
validation_summary <- validated_silownia %>%
  summarise(
    age_valid = all(age_valid),
    gender_valid = all(gender_valid),
    weight_valid = all(weight_valid),
    height_valid = all(height_valid),
    max_bpm_valid = all(max_bpm_valid),
    avg_bpm_valid = all(avg_bpm_valid),
    resting_bpm_valid = all(resting_bpm_valid),
    session_duration_valid = all(session_duration_valid),
    calories_burned_valid = all(calories_burned_valid),
    workout_type_valid = all(workout_type_valid),
    fat_percentage_valid = all(fat_percentage_valid),
    water_intake_valid = all(water_intake_valid),
    workout_frequency_valid = all(workout_frequency_valid),
    experience_level_valid = all(experience_level_valid),
    bmi_valid = all(bmi_valid)
  )

# Zobrazowanie 
print(validation_summary)
table (silownia$workout_type)
