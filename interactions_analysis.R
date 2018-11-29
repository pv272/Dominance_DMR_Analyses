##### load data
library(dplyr)
library(purrr)
library(lubridate)
library(spaMM)
rm(list = ls())

load("AllCall_Tidy.rda")
load("Group_AllInfo.rda")
str(Group_AllInfo)
str(AllCall_Tidy)

id <- Group_AllInfo %>%
  select(AnimalID, Colony, Date, Weight, Sex, Age, BreedingStatus) %>%
  distinct()

int <- AllCall_Tidy %>% 
  select(ObsRef, Winner, Loser, Date)

W <- int %>% 
  select(Winner, Date, ObsRef) %>%
  left_join(id, by =c("Date" = "Date", "Winner" = "AnimalID")) %>% 
  rename(Colony_W = Colony, 
         Weight_W = Weight, 
         Sex_W = Sex, 
         Age_W = Age, 
          Breed_W = BreedingStatus) %>% 
  mutate(RowID = 1:nrow(int))
L <- int %>% 
  select(Loser, Date, ObsRef) %>%
  left_join(id, by =c("Date" = "Date", "Loser" = "AnimalID")) %>% 
  rename(Colony_L = Colony, 
         Weight_L = Weight, 
         Sex_L = Sex, 
         Age_L = Age, 
         Breed_L = BreedingStatus) %>% 
  mutate(RowID = 1:nrow(int))

int_W_L <- W %>% 
  left_join(L) %>%
  na.omit() %>%
  mutate(D_Weight = Weight_W - Weight_L, 
         D_Age = Age_W - Age_L, 
         D_Breed = paste0(Breed_W, Breed_L, sep = "_"), 
         D_Sex = paste0(Sex_W, Sex_L, sep = "_")) %>%
  filter(Colony_W == Colony_L)


###################################### BEFORE FITTING THE MODELS CREATE A MATRIX OF INTERACTIONS
source("Elo_functions.R")
#######################################################################

corr.obj <- buildcorrsigned(int_W_L$Winner, int_W_L$Loser)
DF1$pairsID <- corr.obj$pairsID
data_mod <- DF1
data_mod$win_social <- DF1$win