## get the elo-score, based on Mathias script 
rm(list = ls())
source("Elo_functions.R")
library(readr)
d <- read_csv("SubCall.csv")
d <- d %>% filter(d$ObsType == "Scan")

d <- split(d, d$Colony)

d_test <- d[[20]]
IA_data <- d_test
all_ids <- unique(c(d_test$Loser, d_test$Winner))



x <- elo.model1(par = 4.19, burn_in = 0, init_elo = 1000, IA_data = d_test, all_ids = all_ids, return_likelihood =F) 
library(dplyr)
x1 <- x %>% select(Loser, elo_l_after, ObsDate) %>% 
  rename(ID = Loser, Elo = elo_l_after, date = ObsDate) %>%
  bind_rows(x %>% select(Winner, elo_w_after, ObsDate) %>% 
              rename(ID = Winner, Elo = elo_w_after, date = ObsDate)) %>%
  mutate(year = lubridate::year(date))

x1$ID[x1$Elo == max(x1$Elo)]
library(ggplot2)                                          
ggplot(x1, aes(x = date, y = Elo, col = ID)) +
  geom_line(aes(x = date, y = Elo, col = ID))+
  geom_point(shape = 3)+
  facet_wrap(~year, scales = "free_x")
                                                         


K_optim <- optim(par=4, burn_in=0, elo.model1, all_ids = all_ids, IA_data = d_test,
      return_likelihood=T, init_elo = 10, method= "Nelder-Mead")
?optim
