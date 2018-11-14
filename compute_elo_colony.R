source("Elo_functions.R")  ## warnings can be ignored
## get the elo-score, based on Mathias script 
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
## read data
d <- read_csv("SubCall.csv")
d <- d %>% filter(d$ObsType == "Scan") ## keep only scan obs

# create a nested df by colony
d2 <- d %>% nest(-Colony) %>%
  mutate(n_obs = map_dbl(data, ~ .x %>% group_by(.$ObsType) %>% 
           summarize(ObsCount=n_distinct(.$ObsRef)) %>% 
           pull(ObsCount))) %>% ## get the number of interaction by colony (scan only)
  mutate(all_ids= map(data, ~ unique(c(.x$Winner, .x$Loser)))) %>% ## get list of all ids (needed for fitting the elo)
  filter(n_obs > 10) %>% ## keep only colonies with more than 10 obs
  mutate(K = map2_dbl(data, all_ids, ~ optim(par=4, burn_in=0, elo.model1, all_ids = .y, IA_data = .x,
                   return_likelihood=T, init_elo = 100, method= "Brent", upper = 15, lower = 0)$par)) ## optim the K value for each colony

out1 <- d2 %>% mutate(new_df = pmap(list(X = d2$data, Y = d2$all_ids, Z = as.list(d2$K)), function(X, Y, Z) {
          out <- elo.model1(par = Z, burn_in = 0, init_elo = 1000, IA_data = X, all_ids = Y, return_likelihood =F)
          }
     )) ## fit the elo with correct K values 


out11 <- out1 %>% mutate(plot = map(new_df, ~ plot_elo(.x))) ## add a column with plot for each colony
master_df <- out11 
save(master_df, file = "master_df.rda", compress = "xz") ## save new master_df with all infos. 
