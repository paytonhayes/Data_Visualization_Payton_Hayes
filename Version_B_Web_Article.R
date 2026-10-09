#Receall Packages
library (ggplot2)
library(psych)
library(gt)
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)


#Assign the data a df name (raw version)
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

#remove duplicate ids

autoimmune.df.clean <- autoimmune.df.clean |>
  distinct(Patient_ID, .keep_all = TRUE)

#check to see if duplicates are gone

anyDuplicated(autoimmune.df.clean$Patient_ID)

#check to see if df is in integer (it's not)

is.integer(autoimmune.df.clean)
typeof(autoimmune.df.clean)

#save all symptoms as 'symptom' object to use later? potentially?

symptoms <- c(
  "ANA",
  "Low.grade.fever",
  "Fatigue.or.chronic.tiredness",
  "Rashes.and.skin.lesions",
  "General..unwell..feeling",
  "Joint.pain"
)

#Coerce data frame values from list to integers, excluding Gender

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

#filter only Female participants

female.autoimmune.df <- autoimmune.df.clean |>
  filter(Gender == "Female")

#filter only male participants

male.autoimmune.df <- autoimmune.df.clean |>
  filter(Gender == "Male")

# Find percentage of ANA, C3, negative and both negative
#in Female sample and save as female.percentage.df

female.percentage.df <- female.autoimmune.df |>
  summarise(
    ANA_Negative = mean(ANA == 0) * 100, 
    C3_Negative = mean(C3 == 0) * 100, 
    Both_Negative = mean(ANA == 0 &
                         C3 == 0) *100)

#Find percentage of ANA, C3 negative in male and both
#and save as percentage df

male.percentage.df <- male.autoimmune.df |>
  summarise(
    ANA_Negative = mean(ANA == 0) * 100, 
    C3_Negative = mean(C3 == 0) * 100, 
    Both_Negative = mean(ANA == 0 &
                           C3 == 0) *100)

# create a serology comparison table for both men and women (with N, mean age, mean duration)
# and percentages of negative for ANA, C3, and both

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
#create serology.long.df and all.neg.long.df for ggplot to use 
#and save seperate from serology.comparison.df

serology.long <- serology.comparison.df |>
  pivot_longer(
    cols = c(
      ANA_Negative,
      C3_Negative,
    ),
    names_to = "Marker",
    values_to = "Percent"
  )

#create a df with depicting symptomology for female participants who are both negative
#take female.autoimmune.df and filter rows based on neg for both

female.neg.w.symptom <- female.autoimmune.df |>
  filter(
    ANA == 0, 
    C3 == 0
  )

#add a symptom count column to female.neg.w.symptom data frame

female.neg.w.symptom <- female.neg.w.symptom |>
  mutate(
    Symptom_Count = 
      Fatigue.or.chronic.tiredness + 
      Rashes.and.skin.lesions + 
      Joint.pain + 
      Low.grade.fever + 
      General..unwell..feeling)

#Create pie chart to show age split
#use autoimmune.clean.df because it has individual ages
#center title using plot.title

gender.df <- autoimmune.df.clean |>
  count(Gender)|>
  mutate(
    Percent = round(100 * n / sum(n), 1)
  )

pie.sex <- ggplot(
  gender.df,
  aes(x = "", y = n, fill = Gender))+
  
  geom_col(width = 1)+
  coord_polar("y")+
  geom_text(
    aes(
      label = paste0(Percent, "%")
    ),
    position = position_stack(vjust= 0.5),
    color = "white",
    fontface = "bold",
    size = 5
  )+
  scale_fill_manual(values = c(
    "Female" = "plum3",
    "Male" = "olivedrab3"
  ),
  name = NULL)+
  labs(
    title = "Participant Split by Gender")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5,
                              face = "bold"))

#Create graph for sex serelogical differences (ANA, C3)

bar.sex.web <- ggplot(serology.long,
       aes(x = Marker,
           y = Percent,
           fill = Gender)
       )+
  geom_col(
    width = 0.6,
    position = position_dodge(width = 1))+
  scale_fill_manual(
    values = c(
      "Female" = "plum3",
      "Male" = "olivedrab3"
    ),
    name = NULL
    )+
  scale_y_continuous(
    breaks = seq(0,60, by = 5)
  )+
  scale_x_discrete(
    labels = c(
      "ANA_Negative" = "ANA Negative",
      "C3_Negative" = "C3 Negative"
    )
  )+
    labs(
      title = "Negative for ANA and C3 by Gender",
      x = "Blood Marker",
      y = "Participants Negative (%)") +
  theme_minimal()+
  theme(plot.title = element_text(
    hjust = 0.5, 
    face = "bold",
    margin = margin(b = 15)),
    
    axis.title.y = element_text(
      face = "bold",
      margin = margin(r = 15)
    ),
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 15)
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
#Create df and graph for sex serelogical differences (ALl negative) 

comp.neg.df <- serology.comparison.df |>
  select(
    Gender,
    Both_Negative
  )

bar.sex.web.seperate <- ggplot(
  comp.neg.df,
  aes(
  x = Gender,
  y = Both_Negative,
  fill = Gender
  )
) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "Female" = "plum3",
      "Male" = "olivedrab3"
    )
  )+
  geom_text(
    aes(label = paste0(round(Both_Negative, 1), "%")),
    vjust = -0.5,
    fontface = "bold"
  ) +
  labs(
    title = "People Negative for Both ANA and C3",
    x = "",
    y = "Participants Negative (%)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
        breaks = seq(0,20, by = 2.5)
  )+
  theme_minimal() +
theme(
  plot.title = element_text(
    hjust = 0.5,
    face = "bold",
    margin = margin(b = 15)
  ),
  axis.title.x = element_text(
    face = "bold"
  ),
  axis.text.x = element_text(
    face = "bold"
  ),
  axis.title.y = element_text(
    face = "bold",
    margin = margin(r = 15)
  ),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  legend.position = "none"
)

#Create bar graph for females with both negative, yet still have stumptoms

bar.symptom.female <- ggplot(
  female.neg.w.symptom,
  aes(x = factor(Symptom_Count))
) +
  annotate(
    "rect",
    xmin = 3.5,
    xmax = 7,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.08
  )+
  geom_bar(
    fill = "plum3",
    color = "black"
  ) +
  geom_vline(
    xintercept = 3.5,
    color = "red",
    linetype = "dashed",
    linewidth = 1.5
  ) +
  annotate(
    "label",
    x = 5.65,
    y = 20,
    label = "Women still reported 
    at least
    3 out of 5 symptoms",
    vjust = 1.5,
    fill = "white",
    fontface = "bold"
  ) +
  labs(
    title = "Symptoms in Women Negative for Both ANA and C3",
    x = "Number of Symptoms",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      face = "bold",
      margin = margin(r = 15)
    )
  )

    