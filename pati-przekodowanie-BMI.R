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





