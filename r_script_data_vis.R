
#Receall Packages
library (ggplot2)
library(psych)
library(gt)
library(readxl)
library(tidyverse)
library(janitor)

#Find the data file and read it
read_csv("./autoimmune.csv")

# Assign the data a df name (raw version)
autoimmune.df <- read.csv("./autoimmune.csv")

#Create a 'clean' copy to work in and select ANA status, gender, age, participant ID, sickness duration, low.grade.fever, 
#Fatigue.or.chronic.tiredness, Rashes.and.skin.lesions, General..unwell..feeling, Joint.pain

autoimmune.df.clean <- autoimmune.df |>
  select(
    Patient_ID,
    Age,
    Gender,
    Sickness_Duration_Months,
    ANA,
    C3,
    C4,
    Low.grade.fever,
    Fatigue.or.chronic.tiredness,
    Rashes.and.skin.lesions,
    General..unwell..feeling,
    Joint.pain
  )

#check to see if df is in integer (it's not)

is.integer(autoimmune.df.clean)
typeof(autoimmune.df.clean)

#save all symptoms as 'symptom' object to use later

symptoms <- c(
  "ANA",
  "Low.grade.fever",
  "Fatigue.or.chronic.tiredness",
  "Rashes.and.skin.lesions",
  "General..unwell..feeling",
  "Joint.pain"
)

#Coerce data frame values from list to integers 

autoimmune.df.clean <- autoimmune.df.clean |>
  mutate(
    across(
      c(Patient_ID, Age, Gender, Sickness_Duration_Months,
        ANA, C3, C4, Low.grade.fever,
        Fatigue.or.chronic.tiredness,
        Rashes.and.skin.lesions,
        General..unwell..feeling,
        Joint.pain),
      ~ as.integer(unlist(.x))
    )
  )

#Check is all columns in autoimmune.df.clean are indeed integer

sapply(autoimmune.df.clean, is.integer)
all(sapply(autoimmune.df.clean, is.integer))

#ok, now Gender is NA.... take the original gender column back from autoimmune.df

autoimmune.df.clean$Gender <- autoimmune.df$Gender

#filter only Female participants

female.autoimmune.df <- autoimmune.df.clean |>
  filter(Gender == "Female")






