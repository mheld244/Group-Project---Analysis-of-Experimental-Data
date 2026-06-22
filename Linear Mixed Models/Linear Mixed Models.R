#Library
library(lme4)
library(lmerTest)
library(tidyr)
library(dplyr)
library(ggplot2)

#Import Data
data <- read.csv2(file.choose(), header = TRUE)
head(data)
data$PSMU[data$PSMU == "#skipped#"] <- NA
data$PSMU <- as.numeric(data$PSMU)

##Multivariate Mixed Model##

data_long <- data %>%
  pivot_longer(
    cols = c(Fatigue,
             DeprMood,
             Loneliness,
             Concentrat,
             LossOfInt,
             Inferior,
             Hopeless,
             Stress,
             PSMU,
             Active),
    names_to = "symptom",
    values_to = "score"
  )
head(data_long)

model <- lmer(
  score ~ Day * symptom + (1 | Participant),
  data = data_long
)

summary(model)
anova(model)

##Linear Mixed Models##

#Fatigue
model_fatigue <- lmer(
  Fatigue ~ Day + (1 | Participant),
  data = data
)

summary(model_fatigue)
anova(model_fatigue)

#Depressive Mood
model_deprmood <- lmer(
  DeprMood ~ Day + (1 | Participant),
  data = data
)

summary(model_deprmood)
anova(model_deprmood)

#Loneliness
model_loneliness <- lmer(
  Loneliness ~ Day + (1 | Participant),
  data = data
)

summary(model_loneliness)
anova(model_loneliness)

#Concentration
model_concentration <- lmer(
  Concentrat ~ Day + (1 | Participant),
  data = data
)

summary(model_concentration)
anova(model_concentration)

#Loss Of Interest
model_lossofint <- lmer(
  LossOfInt ~ Day + (1 | Participant),
  data = data
)

summary(model_lossofint)
anova(model_lossofint)

#Inferior
model_inferior <- lmer(
  Inferior ~ Day + (1 | Participant),
  data = data
)

summary(model_inferior)
anova(model_inferior)

#Hopeless
model_hopeless <- lmer(
  Hopeless ~ Day + (1 | Participant),
  data = data
)

summary(model_hopeless)
anova(model_hopeless)

#Stress
model_stress <- lmer(
  Stress ~ Day + (1 | Participant),
  data = data
)

summary(model_stress)
anova(model_stress)

#PSMU
model_psmu <- lmer(
  PSMU ~ Day + (1 | Participant),
  data = data
)

summary(model_psmu)
anova(model_psmu)

#ASMU
model_asmu <- lmer(
  Active ~ Day + (1 | Participant),
  data = data
)

summary(model_asmu)
anova(model_asmu)

##Correction For Multiple Testing##

p_values <- c(
  Fatigue = 0.6347,
  Depressed_Mood = 0.1606,
  Loneliness = 0.01242,
  Concentration = 8.007e-14,
  Loss_of_Interest = 0.03037,
  Inferiority = 0.3006,
  Hopeless = 0.01673,
  Stress = 2.746e-06,
  PSMU = 1.659e-08,
  ASMU = 6.168e-09
)

p.adjust(p_values, method = "BH")

##Figures##

Day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

#PSMU
psmu_data <- data.frame(
  Day = Day,
  PSMU = c(29.31, 30.76, 34.23, 32.84, 31.95, 28.10, 31.32)
)

psmu_data$Day <- factor(psmu_data$Day,
                        levels = Day)

ggplot(psmu_data, aes(x = Day, y = PSMU, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Estimated PSMU levels across the week",
    x = "Day of week",
    y = "PSMU (estimated mean)"
    
  )

#Concentration
conc_data <- data.frame(
  Day = Day,
  Concentration = c(25.97, 26.86, 28.55, 28.32, 28.41, 23.29, 23.75)
)

conc_data$Day <- factor(conc_data$Day,
                        levels = Day)

ggplot(conc_data, aes(x = Day, y = Concentration, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(size = 3, color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Estimated concentration levels across the week",
    x = "Day of week",
    y = "Concentration (estimated mean)"
  )

#Stress
stress_data <- data.frame(
  Day = Day,
  Stress = c(20.61, 20.09, 21.36, 21.61, 22.10, 18.89, 18.88)
)

stress_data$Day <- factor(
  stress_data$Day,
  levels = Day
)

ggplot(stress_data, aes(x = Day, y = Stress, group = 1)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(size = 3, color = "firebrick") +
  theme_minimal() +
  labs(
    title = "Estimated stress levels across the week",
    x = "Day of week",
    y = "Stress (estimated mean)"
  )

#ASMU
asmu_data <- data.frame(
  Day = Day,
  ASMU = c(21.04, 20.79, 21.67, 23.56, 22.76, 18.49, 20.11)
)

asmu_data$Day <- factor(
  asmu_data$Day,
  levels = Day
)

ggplot(asmu_data, aes(x = Day, y = ASMU, group = 1)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(size = 3, color = "purple") +
  theme_minimal() +
  labs(
    title = "Estimated ASMU levels across the week",
    x = "Day of week",
    y = "ASMU (estimated mean)"
  )

#Hopelessness
hopeless_data <- data.frame(
  Day = Day,
  Hopelessness = c(12.32, 11.67, 12.34, 12.29, 12.12, 11.23, 10.86)
)

hopeless_data$Day <- factor(
  hopeless_data$Day,
  levels = Day
)

ggplot(hopeless_data, aes(x = Day, y = Hopelessness, group = 1)) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_point(size = 3, color = "darkorange") +
  theme_minimal() +
  labs(
    title = "Estimated hopelessness levels across the week",
    x = "Day of week",
    y = "Hopelessness (estimated mean)"
  )