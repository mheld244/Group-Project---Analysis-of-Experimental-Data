library(psych)
library(lavaan)
library(tidyverse)
library(GPArotation)
##Factor analysis##

#1: EFA

#Reshape long into wide format: aggregated data
df_wide <- dataset %>%
  group_by(Participant) %>%
  summarise(
    Fatigue = mean(Fatigue, na.rm = TRUE),
    DeprMood = mean(DeprMood, na.rm = TRUE),
    Loneliness = mean(Loneliness, na.rm = TRUE),
    Concentrat = mean(Concentrat, na.rm = TRUE),
    LossOfInt = mean(LossOfInt, na.rm = TRUE),
    Inferior = mean(Inferior, na.rm = TRUE),
    Hopeless = mean(Hopeless, na.rm = TRUE),
    Stress = mean(Stress, na.rm = TRUE),
  )

df <- select(df_wide, Fatigue:Hopeless)

#Assumption checks
cortest.bartlett(R=cor(df), n=125)
KMO(df)

#determine number of factors
fa.parallel(df, fa= "fa",fm= "ml", show.legend=F)

#extract factors  
efa.result <-factanal(df, factors = 2, rotation = "promax")
print(efa.result, cutoff = 0.4) #view factor loadings

#method 2 - different rotation method
efa.result <- factanal(df, factors = 2, rotation= "oblimin")
print(efa.result)
