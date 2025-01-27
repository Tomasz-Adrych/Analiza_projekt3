#Wizualizacja

library (ggplot2)
library(readr)
library(dplyr)

#Wczytanie danych
silownia <- read_csv("silownia_new.csv")

#Zmiana nazwy zmiennej
silownia <- silownia %>% rename(Workout_Frequency = 'Workout_Frequency (days/week)')

#Wykres zależności poziomu doświadczenia od częstotliwości treningów
ggplot(silownia, aes(x = Workout_Frequency, y = Experience_Level, color = as.factor(Experience_Level))) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  scale_y_continuous(
    breaks = 1:3  
  ) +
  scale_color_manual(
    values = c("1" = "red", "2" = "green", "3" = "blue"),
    name = "Experience Level"  
  ) +
  labs(
    title = "Poziom doświadczenia w zależności od częstotliwości treningów",
    x = "Workout Frequency (days/week)",
    y = "Experience Level"
  ) +
  theme_minimal()




















