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

library(tidyverse)
library(validate)
library(readr)

silownia_new <- read_csv("silownia_new.csv")

zasady <- validator(
  Age >= 18 & age <= 100
  , Weight >= 35 & Weight <= 200
  , Height >= 1 & Height <= 2
  , Max_BPM >= 30 & Max_BPM = 220 - Age
  , Resting_BPM >= 30 & Resting_BPM <= 100
  , Session_Duration > 0 & Session_Duration < 24
  , 
)

