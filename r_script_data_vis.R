
#Receall Packages
library (ggplot2)
library(psych)
library(gt)
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)

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

#filter only meale participants

male.autoimmune.df <- autoimmune.df.clean |>
  filter(Gender == "Male")

# Find percentage of ANA, C3, C4 Negative and all three negative
#in Female sample and save as percentage df

female.percentage.df <- female.autoimmune.df |>
  summarise(
    ANA_Negative = mean(ANA == 0) * 100, 
    C3_Negative = mean(C3 == 0) * 100, 
    C4_Negative = mean(C4 == 0) * 100,
    All_Three_Negative = mean(ANA == 0 &
                              C3 == 0 &
                              C4 == 0) *100)


# Find percentage of ANA, C3, C4 negative in male and all three negative 
# and save as percentage df

male.percentage.df <- male.autoimmune.df |>
  summarise(
    ANA_Negative = mean(ANA == 0) * 100, 
    C3_Negative = mean(C3 == 0) * 100, 
    C4_Negative = mean(C4 == 0) * 100,
    All_Three_Negative = mean(ANA == 0 &
                                  C3 == 0 &
                                  C4 == 0)* 100)

# create a serology comparison table for both men and women (with N, mean age, mean duration)
# and percentages of negative for ANA, C3, C4,and all three 

serology.comparison.df <- bind_rows(
  Female = female.percentage.df,
  Male = male.percentage.df,
  .id = "Gender"
)

#But first you have to calculate means of demographics

demographics.df <- autoimmune.df.clean |>
  group_by(Gender)|>
  summarise(
    N = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    Mean_Duration = mean(Sickness_Duration_Months, na.rm = TRUE)
  )
 
#Join serology.df with demographic.df (sereology is on left (first) so use left_join())

serology.comparison.df <- serology.comparison.df |>
  left_join(demographics.df, by = "Gender")

#move demographics columns first, but after gender

serology.comparison.df <- serology.comparison.df |>
  relocate(
    Gender,
    N,
    Mean_Age,
    Mean_Duration, 
    .before = ANA_Negative
  )

#create serology.long.df for ggplot to use and save seperate from serology.comparison.df

serology.long <- serology.comparison.df |>
  pivot_longer(
    cols = c(
      ANA_Negative,
      C3_Negative,
      C4_Negative,
      All_Three_Negative,
    ),
    names_to = "Marker",
    values_to = "Percent"
  )

#Create new pryamid chart to show age range acorss both genders 
#use autoimmune.clean.df because it has individual ages

demographic.pryamid.df <- autoimmune.df.clean
  

#Create graph for sex serelogical differences (ANA, C3, C4 seperately)
 
#Create graph for sex serelogical differences (ALl negative) 
  
#Create graph for those with all negative, yet symptomological positives

    
# Create a Venn Diagram


  
  
  


