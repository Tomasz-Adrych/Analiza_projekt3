#Model

library(readr)

#Wczytanie danych
silownia <- read_csv("silownia_new.csv")


#Procent tkanki tłuszczowej w zależności od wagi uczestnika oraz częstotliwości treningu
model <- stats::aov(
  formula = Fat_Percentage ~ `Weight (kg)` * `Workout_Frequency (days/week)`,
  data = silownia
)
ggcoefstats (model)








