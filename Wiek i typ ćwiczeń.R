library(dplyr)

silownia <- read.csv("silownia_new.csv")

#Dane bez brakujących wartości
silownia_pelne <- na.omit(silownia)

#Przedziały wiekowe: 1:(18-25), 2:(26-33), 3:(34-41), 4:(42-50), 5:(50-59).
silownia_pelne$Age <- ifelse (silownia_pelne$Age <= 25, 1, 
                              ifelse(silownia_pelne$Age <= 33, 2, 
                                     ifelse(silownia_pelne$Age <= 41, 3,
                                            ifelse(silownia_pelne$Age <= 50, 4, 5))))

#Liczba osób w danym przedziale wiekowym na danych zajęciach 
table(silownia_pelne$Age, silownia_pelne$Workout_Type)


