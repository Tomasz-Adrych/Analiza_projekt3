#Testowanie statystyczne

install.packages("ggstatsplot")  
library(ggstatsplot)
library(readr)

#Wczytanie danych
silownia <- read_csv("silownia_new.csv")

#Zalezność pomiędzy czasem trwania treningu a BMI
ggscatterstats (
  data= silownia,
  x= 'Session_Duration (hours)',
  y= BMI
)

#Zależność pomiędzy wiekiem a częstotliwością ćwiczeń
ggbetweenstats (
  data= silownia,
  x= 'Workout_Frequency (days/week)',
  y= Age
)