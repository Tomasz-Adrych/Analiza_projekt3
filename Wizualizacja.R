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




# Obliczanie średnich AVG_BPM dla każdego typu treningu
avg_bpm_summary <- silownia %>%
  group_by(Workout_Type) %>%
  summarise(mean_bpm = mean(Avg_BPM, na.rm = TRUE))

#Wykres zależności AVG_BPM od rodzaju treningu
ggplot(silownia, aes(x = Workout_Type, y = Avg_BPM, fill = Workout_Type)) +
  geom_violin(alpha = 0.7) +  
  geom_point(data = avg_bpm_summary, aes(x = Workout_Type, y = mean_bpm), 
             color = "red", size = 3) +  # Średnie jako czerwone punkty
  geom_line(data = avg_bpm_summary, aes(x = Workout_Type, y = mean_bpm, group = 1), 
            color = "red", linetype = "dashed", linewidth = 1) +  # Linia łącząca średnie
  labs(
    title = "Rozkład AVG_BPM w zależności od rodzaju treningu",
    x = "Rodzaj treningu",
    y = "AVG_BPM"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
















