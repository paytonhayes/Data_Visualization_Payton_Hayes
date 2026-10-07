
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
    Low.grade.fever,
    Fatigue.or.chronic.tiredness,
    Rashes.and.skin.lesions,
    General..unwell..feeling,
    Joint.pain
  )

#remove duplicate id (femal ID 20??) WHY IS THERE STILL DUPLICATES IN FEMALE

autoimmune.df.clean <- autoimmune.df.clean |>
  distinct(Patient_ID, .keep_all = TRUE)

#check to see if duplicates are gone

anyDuplicated(autoimmune.df.clean$Patient_ID)

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
      c(Patient_ID, Age, Sickness_Duration_Months,
        ANA, C3, Low.grade.fever,
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
    Both_Negative = mean(ANA == 0 &
                         C3 == 0) *100)

# Find percentage of ANA, C3, C4 negative in male and all three negative 
# and save as percentage df

male.percentage.df <- male.autoimmune.df |>
  summarise(
    ANA_Negative = mean(ANA == 0) * 100, 
    C3_Negative = mean(C3 == 0) * 100, 
    Both_Negative = mean(ANA == 0 &
                           C3 == 0) *100)

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
#create serology.long.df and all.neg.long.df for ggplot to use and save seperate from serology.comparison.df

serology.long <- serology.comparison.df |>
  pivot_longer(
    cols = c(
      ANA_Negative,
      C3_Negative,
    ),
    names_to = "Marker",
    values_to = "Percent"
  )

#create a df with depicting symptomology female participants who are all three negative
#take female.autoimmune.df and filter rows based on neg for all three

female.neg.w.symptom <- female.autoimmune.df |>
  filter(
    ANA == 0, 
    C3 == 0
  )

#add symptom count column to female.neg.w.symptom data frame

female.neg.w.symptom <- female.neg.w.symptom |>
  mutate(
    Symptom_Count = 
      Fatigue.or.chronic.tiredness + 
      Rashes.and.skin.lesions + 
      Joint.pain + 
      Low.grade.fever + 
      General..unwell..feeling)

#Create distribution chart to show age range acorss both genders 
#use autoimmune.clean.df because it has individual ages
#center title using plot.title

ggplot(
  autoimmune.df.clean,
  aes(
    x = Age,
    fill = Gender
  )
) +
  geom_histogram(
    alpha = 0.4,
    position = "identity",
    bins = 30
  ) +
  labs(
    title = "Age Distribution by Gender",
    x = "Age (Years)",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

ggplot(
  autoimmune.df.clean,
  aes(
    x = Age,
    fill = Gender
  )
) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Gender) +
  labs(
    title = "Age Distribution by Gender",
    x = "Age (Years)",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

#Create graph for sex serelogical differences (ANA, C3, seperately)

ggplot(serology.long,
       aes(x = Marker,
           y = Percent,
           fill = Gender)
       )+
  geom_col(position = "dodge")+
    labs(
      title = "Serologic Negativity by Gender",
      x = "Serologic Marker",
      y = "Percent Negative (%)") +
  theme_minimal()+
  theme(plot.title = element_text(
    hjust = 0.5, 
    face = "bold")
  )
  

#Create df and graph for sex serelogical differences (ALl negative) 

comp.neg.df <- serology.comparison.df |>
  select(
    Gender,
    Both_Negative
  )

ggplot(
  comp.neg.df,
  aes(
  x = Gender,
  y = Both_Negative,
  fill = Gender
  )
) +
  geom_col() +
  geom_text(
    aes(label = round(Both_Negative, 1)),
    vjust = -0.5
  ) +
  labs(
    title = "Complete Seronegativity by Gender",
    x = "Gender",
    y = "Percent Negative for ANA and C3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

#Create violin graph for females with both negative, yet symptomological positives

ggplot(
  female.neg.w.symptom,
  aes(
    x = "",
    y = Symptom_Count
  )
)+
  geom_violin(fill = "steelblue", alpha = 0.6)+
  labs(
    title = "Symptom Burden Among ANA and C3 Seronegative Females",
    y = "Number of Symptoms",
    x = NULL) +
    theme_minimal()
  


