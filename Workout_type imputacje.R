install.packages("mice")
library(mice)

#Wczytanie danych
silownia <- read.csv("silownia_new.csv")

#Zamiana zmiennych tekstowych na numeryczne 
#Zmienna Gender: 0 oznacza mężczyznę, 1 kobietę
#Zmienna Workout_Type: 1 to trening siłowy, 2 to trening HIIT, 3 to cardio, a 4 oznacza yogę
silownia$Gender <- ifelse (silownia$Gender == "Male", 0, 
                           ifelse(silownia$Gender == "Female", 1, 2))

silownia$Workout_Type <- ifelse (silownia$Workout_Type == "Strength", 1, 
                                 ifelse(silownia$Workout_Type == "HIIT", 2, 
                                        ifelse(silownia$Workout_Type == "Cardio", 3,
                                               ifelse(silownia$Workout_Type == "Yoga", 4, 5))))

#Dane bez NA
silownia_puste <- na.omit(silownia)


#Model logitowy dla danych bez NA; żadna zmienna objaśniająca nie jest statystycznie istotna
logit <- glm(as.factor(Workout_Type) ~ ., 
             data = silownia_puste, family = binomial)
summary(logit)

#Imputacja modelami łańcuchowymi
silownia$Workout_Type <- as.factor(silownia$Workout_Type)

#Tworzenie macierzy zmiennych wykorzystywanych do imputacji
#Pominięto zmienne Age i BMI, które również zawierały NA
przyklad <- make.predictorMatrix(silownia)

przyklad[, c("Age", "BMI")] <- 0
przyklad[c("Age", "BMI"), ] <- 0

#Wybór zmiennej Workout_type jako tej, która ma zostać uzupełniona
wybrana_zmienna <- is.na(silownia)
wybrana_zmienna[, colnames(silownia) != "Workout_Type"] <- FALSE

#Imputacja danych
wynik <- mice(silownia, predictorMatrix = przyklad, where = wybrana_zmienna)

#Podstawienie uzupełnionej zmiennej do zbioru danych
uzupelnione_dane <- complete(wynik)
silownia$Workout_Type <- uzupelnione_dane$Workout_Type

#Sprawdzenie
summary(silownia)