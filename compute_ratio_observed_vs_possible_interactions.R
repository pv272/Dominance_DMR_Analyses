
library(RMySQL)
library(getPass)
library(EloRating)
library(tidyverse)
library(lubridate)
library(dplyr)
library(purrr)
source("Elo_functions.R")
con <- dbConnect(MySQL(), user = 'philippev',password = getPass(),  
                 dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')

#set seed to get always the same randomization
set.seed(123)
animals <- con %>% dbGetQuery("SELECT * FROM user_philippev.ID_Characteristic")
 
dbDisconnect(con)

load("master_df.rda")



d <- master_df$data[[10]]


get_birthdate <- function(ID, DF1, VAR, name){
VAR <- "AnimalID"
VAR <-enquo(VAR)

out <- DF1 %>% filter(!!VAR == ID) %>% 
  mutate(!!name := 2)

return(out)
}

d$Winner[1]
x <- vector()
get_birthdate("G3F002", DF1 = animals, VAR = AnimalID, name = "Phil")
animals$
d$new_col <- map_chr(d$Winner, ~ get_birthdate(ID = .x, DF1 = animals))




d <- master_df$new_df[[33]]
name_list <- tibble(ID = unique(c(d$Winner, d$Loser)))

x_updated <- left_join(name_list, animals, by = c("ID" = "AnimalID")) %>%
  select(ID, BirthDate, DeathDate) %>%
  mutate(ID2 = ID) %>%
  mutate(BirthDate = ymd(BirthDate), 
         DeathDate = ymd(DeathDate))
IDs <- name_list$ID

get_interaction <- function(x) {
  B <- x_updated$BirthDate[x_updated$ID == x]
  if (is.na(B))  B <- min(x_updated$BirthDate, na.rm =T)
  
  D <- x_updated$DeathDate[x_updated$ID == x]
  if (is.na(D)) D <- ymd(Sys.Date())
  
  out <- x_updated %>% filter(ID != x) %>%
    mutate(ID2 = x, 
           B = B, 
           D = D) %>% 
    filter(is.na(BirthDate) | BirthDate <= D) %>%
    filter(is.na(DeathDate) | DeathDate >= B) %>%
  mutate(pairID = map2_chr(ID, ID2, ~ get_pair(.x, .y)))
  return(out)
}
full <- map_df(IDs, ~ get_interaction(.x)) %>%
  group_by(pairID) %>% slice(1)

d <- d %>% mutate(pair_ID = map2_chr(Winner, Loser, ~get_pair(.x, .y)))

unique_interaction <- unique(d$pair_ID) 
ratio <- length(unique_interaction) / nrow(full)

