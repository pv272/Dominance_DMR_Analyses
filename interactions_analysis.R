##### load data
library(dplyr)
library(purrr)
library(lubridate)
library(spaMM)
library(tidyr)
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
         D_Breed = paste(Breed_W, Breed_L, sep = "_"), 
         D_Sex = paste(Sex_W, Sex_L, sep = "_")) %>%
  filter(Colony_W == Colony_L) %>%
  mutate(Colony = Colony_W)


###################################### BEFORE FITTING THE MODELS CREATE A MATRIX OF INTERACTIONS
buildcorrsigned <- function(id1, id2) {
  if(any(as.character(id1) == as.character(id2)))
    stop("id1 and id2 are sometimes the same")
  id1 <- paste(id1, sep = ":")
  id2 <- paste(id2, sep = ":")
  pairs <- paste(id1, id2, sep="_")
  u_pairs <- unique(pairs)
  E <- expand.grid(id1 = u_pairs, id2 = u_pairs)
  AB <- strsplit(as.character(E$id1), "_")
  E$A <- unlist(lapply(AB, function(i) i[1]))
  E$B <- unlist(lapply(AB, function(i) i[2]))
  CD <- strsplit(as.character(E$id2), "_")
  E$C <- unlist(lapply(CD, function(i) i[1]))
  E$D <- unlist(lapply(CD, function(i) i[2]))
  E$U <- apply(E, 1, function(i) length(unique(i[c("A", "B", "C", "D")])))
  E$Corr <- NA
  E$Corr[(E$U <= 2) & (E$A == E$C)] <- 1
  E$Corr[(E$U <= 2) & (E$A == E$D)] <- -1
  E$Corr[E$U == 3 & ((E$A == E$C)|(E$B == E$D))] <- 0.5
  E$Corr[E$U == 3 & ((E$A == E$D)|(E$B == E$C))] <- -0.5
  E$Corr[E$U == 4] <- 0
  E$A <- E$B <- E$C <- E$D <- NULL
  M <- matrix(E$Corr, ncol=length(u_pairs), nrow=length(u_pairs))
  rownames(M) <- colnames(M) <- u_pairs
  
  return(list(corrM = M, pairsID = pairs))
}


######################################################################

get_TJUR <- function(model, name) {
  data <- model$data
  data$pred <- spaMM::predict.HLfit(model, re.form = NA)[, 1]
  out <- data.frame(name = name,
                    D_tjur = mean(ifelse(data$win, data$pred, 1- data$pred)) -
                      mean(ifelse(data$win == F, data$pred, 1- data$pred)))
  return(out)
}    

#######################################################################
########## flip the values to get the heaviest as focal 

int <- int_W_L %>% 
  select(D_Weight, D_Age, D_Breed, D_Sex, Winner, Loser, Colony) %>% 
  split(.$D_Weight >= 0)


int_body <- int[["FALSE"]] %>% 
  rename(Focal = Loser, 
         Other = Winner) %>% 
  mutate(Win = FALSE, 
         D_Age = - D_Age, 
         D_Weight = -D_Weight, 
         D_Breed = ifelse(D_Breed == "Breeder_Helper", "Helper_Breeder", 
                          ifelse(D_Breed == "Helper_Breeder", "Breeder_Helper", 
                          as.character(D_Breed))), 
         D_Sex = ifelse(D_Sex == "F_M", "M_F", 
                        ifelse(D_Sex == "M_F", "F_M", 
                        as.character(D_Sex)))) %>%
  bind_rows(int[["TRUE"]] %>% 
              rename(Focal = Winner, 
                     Other = Loser) %>% 
              mutate(Win = TRUE))  %>% 
  nest(-Colony)

int_body <- int_body %>% 
  mutate(n = map_dbl(data, ~ nrow(.x))) %>%
  mutate(levels_Breed = map(data, ~ levels(as.factor(.x$D_Breed))), 
         levels_Sex = map(data, ~ levels(as.factor(.x$D_Sex)))) %>%
  filter(n >= 200) %>%
  mutate(corr_obj_body = map(data, ~ buildcorrsigned(.x$Focal, .x$Other)), 
         data = map2(data, corr_obj_body, ~ .x  %>% mutate(Pairs = .y$pairsID)), 
         data = map2(data, Colony, ~ .x  %>% mutate(Colony = as.character(.y))))
         # models = map2(data,corr_obj_body, ~
         #                 fitme(Win ~ 1 + corrMatrix(1|Pairs),
         #                       corrMatrix = .y$corrM,
         #                       family = "binomial", 
         #                       data = .x,
         #                       method = "PQL",
         #                       control.HLfit = list(LevenbergM = TRUE),
         #                       verbose = c(TRACE = 1L))
         #                   ))



## models on all data 
data_body <- bind_rows(int_body$data)
corr_obj_body <- buildcorrsigned(data_body$Focal, data_body$Other)  
mod_body <- fitme(Win ~ 1 + corrMatrix(1|Pairs) + (1|Colony),
                      corrMatrix = corr_obj_body$corrM,
                      family = "binomial", 
                      data = data,
                      method = "PQL",
                      control.HLfit = list(LevenbergM = TRUE),
                      verbose = c(TRACE = 1L))
              
predict(mod_body, re.form = NA)

get_TJUR(mod_body, "Mod_body")
################################################################################################################################################
int <- bind_rows(int)
levels(as.factor(int$D_Breed))
int_breed <- int %>% 
  filter(int$D_Breed %in% c("Breeder_Helper", "Helper_Breeder")) %>%
  split(.$D_Breed)

int_breed <- int_breed[["Helper_Breeder"]] %>% 
  rename(Focal = Loser, 
         Other = Winner) %>% 
  mutate(Win = FALSE, 
         D_Age = - D_Age, 
         D_Weight = -D_Weight, 
         D_Breed = "Breeder_Helper",
         D_Sex = ifelse(D_Sex == "F_M", "M_F", 
                        ifelse(D_Sex == "M_F", "F_M", 
                               as.character(D_Sex)))) %>%
  bind_rows(int_breed[["Breeder_Helper"]] %>% 
              rename(Focal = Winner, 
                     Other = Loser) %>% 
              mutate(Win = TRUE))
  


corr_obj_breed <- buildcorrsigned(int_breed$Focal, int_breed$Other)  
int_breed$Pairs <- corr_obj_breed$pairsID
mod_breed <- fitme(Win ~ 1 + corrMatrix(1|Pairs) + (1|Colony),
                  corrMatrix = corr_obj_breed$corrM,
                  family = "binomial", 
                  data = int_breed,
                  method = "PQL",
                  control.HLfit = list(LevenbergM = TRUE),
                  verbose = c(TRACE = 1L))

################################################################################################################################################

int <- bind_rows(int) %>% split(.$D_Age >= 0)
int_Age <- int[["FALSE"]] %>% 
  rename(Focal = Loser, 
         Other = Winner) %>% 
  mutate(Win = FALSE, 
         D_Age = - D_Age, 
         D_Weight = - D_Weight, 
         D_Breed = ifelse(D_Breed == "Breeder_Helper", "Helper_Breeder", 
                          ifelse(D_Breed == "Helper_Breeder", "Breeder_Helper", 
                                 as.character(D_Breed))), 
         D_Sex = ifelse(D_Sex == "F_M", "M_F", 
                        ifelse(D_Sex == "M_F", "F_M", 
                               as.character(D_Sex)))) %>%
  bind_rows(int[["TRUE"]] %>% 
              rename(Focal = Winner, 
                     Other = Loser) %>% 
              mutate(Win = TRUE))



corr_obj_Age <- buildcorrsigned(int_Age$Focal, int_Age$Other)  
int_Age$Pairs <- corr_obj_Age$pairsID
mod_Age <- fitme(Win ~ 1 + corrMatrix(1|Pairs) + (1|Colony),
                   corrMatrix = corr_obj_Age$corrM,
                   family = "binomial", 
                   data = int_Age,
                   method = "PQL",
                   control.HLfit = list(LevenbergM = TRUE),
                   verbose = c(TRACE = 1L))

predict(mod_Age, re.form = NA)

################################################################################################################################################

int <- bind_rows(int) %>% filter(D_Sex %in% c("F_M", "M_F")) %>% split(.$D_Sex)
int_Sex <- int[["F_M"]] %>% 
  rename(Focal = Loser, 
         Other = Winner) %>% 
  mutate(Win = FALSE, 
         D_Age = - D_Age, 
         D_Weight = - D_Weight, 
         D_Breed = ifelse(D_Breed == "Breeder_Helper", "Helper_Breeder", 
                          ifelse(D_Breed == "Helper_Breeder", "Breeder_Helper", 
                                 as.character(D_Breed))), 
         D_Sex = ifelse(D_Sex == "F_M", "M_F", 
                        ifelse(D_Sex == "M_F", "F_M", 
                               as.character(D_Sex)))) %>%
  bind_rows(int[["M_F"]] %>% 
              rename(Focal = Winner, 
                     Other = Loser) %>% 
              mutate(Win = TRUE))



corr_obj_sex <- buildcorrsigned(int_Sex$Focal, int_Sex$Other)  
int_Sex$Pairs <- corr_obj_sex$pairsID
mod_Sex <- fitme(Win ~ 1 + corrMatrix(1|Pairs) + (1|Colony),
                 corrMatrix = corr_obj_sex$corrM,
                 family = "binomial", 
                 data = int_Sex,
                 method = "PQL",
                 control.HLfit = list(LevenbergM = TRUE),
                 verbose = c(TRACE = 1L))

predict(mod_Sex, re.form = NA)[1]
predict(mod_Age, re.form = NA)[1]
predict(mod_body, re.form = NA)[1]
predict(mod_breed, re.form = NA)[1]
