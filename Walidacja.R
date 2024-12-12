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

install.packages("validate")
install.packages("janitor")
library(tidyverse)
library(validate)
library(readr)
library(janitor)
silownia <- read_csv("silownia_new.csv")
silownia <- janitor::clean_names(silownia)


silownia$gender <- ifelse (silownia$gender == "Male", 0, 
                           ifelse(silownia$gender == "Female", 1, 2))

silownia$workout_type <- ifelse (silownia$workout_type == "Strength", 1, 
                                 ifelse(silownia$workout_type == "HIIT", 2, 
                                        ifelse(silownia$workout_type== "Cardio", 3,
                                               ifelse(silownia$workout_type == "Yoga", 4, 5))))


zasady <- validator(
  age >= 18 & age <= 100
  , weight_kg >= 35 & weight_kg <= 200
  , height_m >= 1 & height_m <= 2
  , max_bpm >= 30 & max_bpm <= 220 
  , resting_bpm >= 30 & resting_bpm <= 100
  , session_duration_hours > 0 & session_duration_hours < 24
  , workout_type == c(1,2,3,4)
  , if (workout_type == 3)  Calories_Burned <= 500 * (session_duration_hours / 2)  ,
   if (workout_type == 4)  Calories_Burned <= 500 * (session_duration_hours)  , 
   if (workout_type == 2)  Calories_Burned <= 500 * (session_duration_hours / 2)  ,
   if (workout_type == 1)  Calories_Burned <= 300 * (session_duration_hours)  ,
   if (gender == 0) fat_percentage >= 6
  ,if (gender == 1) fat_percentage >= 12
  , water_intake_liters <= 5
  )
?validator
out <- confront(silownia, zasady)

summary(out)

plot(out)

#do zrobienia w domu, pozamieniać nazwy kolumn tak aby kod mógł poprawenie zadziałać 