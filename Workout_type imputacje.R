install.packages("mice")
library(mice)
install.packages("rpart")
library(rpart)

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

#Dane do obliczeń; bez zmiennych Agei i BMI zawierających braki
robocze <- silownia[, 2:14]

#Zmienna Workout_type jako factor
robocze$Workout_Type <- as.factor(robocze$Workout_Type)

#####################################################
#Wielowymiarowe wypełnianie przez równania łańcuchowe 

#Tworzenie macierzy zmiennych wykorzystywanych do imputacji
#Wybór zmiennej Workout_type jako tej, która ma zostać uzupełniona
wybrana_zmienna <- is.na(robocze)

#Imputacja danych
wynik <- mice(robocze, where = wybrana_zmienna)

#Podstawienie uzupełnionej zmiennej do zbioru danych
dane_mice <- complete(wynik)

#Sprawdzenie
summary(dane_mice$Workout_Type)

########################################
#RPART - drzewa losowe – klasyfikacyjne

#Wyszczególnienie braków i danych do stworzenia drzewa decyzyjnego
dane_pelne <- robocze[!is.na(robocze$Workout_Type), ]
braki <- robocze[is.na(robocze$Workout_Type), ]

# Model drzewa decyzyjnego
model <- rpart(Workout_Type ~ ., data = dane_pelne, method = "class")

# Przewidywanie brakujących wartości
braki$Workout_Type <- predict(model, braki, type = "class")

#Podstawienie uzupełnionej zmiennej do zbioru danych
dane_rpart <- robocze
dane_rpart$Workout_Type <- ifelse(is.na(robocze$Workout_Type) == TRUE, braki$Workout_Type, robocze$Workout_Type)

#Zmienna Workout_type jako factor
dane_rpart$Workout_Type <- as.factor(dane_rpart$Workout_Type)

#Sprawdzenie
summary(dane_rpart$Workout_Type)

####################################################
#Sprawdzenie, która z metod lepiej przewidziała NA
podstawowe <- round((table(silownia$Workout_Type) / sum(table(silownia$Workout_Type))) * 100, digits = 2)
imput_mice <- round((table(dane_mice$Workout_Type) / sum(table(dane_mice$Workout_Type))) * 100, digits = 2)
imput_rbind <- round((table(dane_rpart$Workout_Type) / sum(table(dane_rpart$Workout_Type))) * 100, digits = 2)

tabela <- cbind(podstawowe, imput_mice, imput_rbind)








