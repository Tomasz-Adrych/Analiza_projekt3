install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("ISLR")


library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 

install.packages("readr")
library(readr)

silownia <- read_csv("silownia_new.csv")

# liczba wierszy kompletnych 
sum(complete.cases(silownia))

#oblicza % wierszy w zbiorze danych, które nie zawierają braków danych
nrow(silownia[complete.cases(silownia), ])/nrow(silownia)*100


#Czy dane zawierają inne wartości specjalne? Jeśli tak, zastąp je wartością NA.
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

sapply(silownia, is.special)


################################################
for (n in colnames(silownia)){
  is.na(silownia[[n]]) <- is.special(silownia[[n]])
}
summary(silownia)


##################################################
# WAŻNE OD TEGO MIEJSCA - liczę BMI dla wartości NA 

silownia <- silownia %>%
  mutate(
    BMI = if_else(
      is.na(BMI),                                                # Jeśli BMI jest NA
      silownia$`Weight (kg)` / (silownia$`Height (m)`^2),        # Oblicz BMI jako masa / wzrost^2
      BMI                                                        # W przeciwnym razie zachowaj istniejącą wartość
    )
  )

# zaokrąglam do 2 miejsc po przecinku 
silownia <- silownia %>%
  mutate(BMI = round(BMI, 2))


install.packages("ggplot2")
library(ggplot2)
library(dplyr)


# Wykres 1: Zależność między wiekiem a BMI z podziałem na płeć

ggplot(silownia, aes(x = Age, y = BMI, color = Gender)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Zależność między wiekiem a BMI",
    x = "Wiek",
    y = "BMI"
  ) +
  theme_minimal()



#Wykres 2  - średnia liczba spalonych kalorii w zależności od typu teningu

# Grupowanie danych i obliczenie średnich
avg_calories <- aggregate(Calories_Burned ~ Workout_Type, data = silownia, mean)

# Wykres słupkowy
ggplot(avg_calories, aes(x = Workout_Type, y = Calories_Burned, fill = Workout_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Calories_Burned, 1)), vjust = -0.5, size = 5) + # Dodanie liczb nad słupkami
  labs(
    title = "Średnia liczba spalonych kalorii w zależności od typu treningu",
    x = "Typ treningu",
    y = "Średnie spalone kalorie"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")




# Wykres 3 - Spożycie wody w zależności od poziomu doświadczenia
ggplot(silownia, aes(x = factor(Experience_Level, labels = c("Beginner", "Intermediate", "Advanced")), 
                         y = `Water_Intake (liters)`, fill = factor(Experience_Level))) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) +
  geom_jitter(width = 0.2, aes(color = `Workout_Frequency (days/week)`), size = 3, alpha = 0.7) +
  labs(
    title = "Spożycie wody w zależności od poziomu doświadczenia",
    x = "Poziom doświadczenia",
    y = "Spożycie wody (litry)",
    color = "Częstotliwość treningów (dni/tydzień)"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()


# Wykres 4 - wykres spalonych kalorii do wagi 
ggplot(silownia, aes(x = `Weight (kg)`, y = Calories_Burned, color = factor(Experience_Level))) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "Zależność spalonych kalorii od wagi",
    x = "Waga (kg)",
    y = "Spalone kalorie",
    color = "Poziom doświadczenia"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()



