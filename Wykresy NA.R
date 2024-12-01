library(readr)
library(dplyr)
library(visdat)
library(naniar)

#Wczytanie pliku
silownia <- read_csv("silownia_new.csv")
tbl_df(silownia)

#Liczba brakujących wartości w pliku, kolumnach i wierszach
sum(is.na(silownia))
colSums(is.na(silownia))
rowSums(is.na(silownia))
n_miss(silownia)

#Wykresy brakujących wartości 
vis_miss(silownia)
vis_miss(silownia, cluster=TRUE)

#Wykres powiązań pomiędzy zmiennymi, w których występują wartości NA 
gg_miss_upset(silownia, nsets = 3)

#Wykresy częstości występowania brakujących zmiennych w zależności od płci i rodzaju ćwiczeń
gg_miss_var(silownia, facet = Gender)
gg_miss_var(silownia, facet = Workout_Type)
