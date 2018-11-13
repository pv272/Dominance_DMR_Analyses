## get the elo-score, based on Mathias script 
rm(list = ls())
library(readr)
d <- read_csv("SubCall.csv")
d <- d %>% filter(d$ObsType == "Scan")

d <- split(d, d$Colony)

d_test <- d[[20]]
IA_data <- d_test
all_ids <- unique(c(d_test$Loser, d_test$Winner))

## Mattias Models fitting 
# Model 1 -- constant initial socres + fitting k
################################################################################
elo.model1 <- function(par, burn_in=1, init_elo = 1000, IA_data, all_ids, return_likelihood = T)
{
  k <- par
  # Initialize output columns
  if (!return_likelihood) IA_data$elo_l_before <- IA_data$elo_w_before <- IA_data$elo_l_after <-
      IA_data$elo_w_after <- NA
  # Set intitial elo scores
  currentELO <- rep(init_elo,length(all_ids))
  names(currentELO) <- all_ids
  # Initialize the log likelihood
  L <- 0
  # Start loop
  for(i in 1:nrow(IA_data))
  # i <- 2
    {
    ind1 <- which(names(currentELO)==IA_data$Winner[i])
    ind2 <- which(names(currentELO)==IA_data$Loser[i])
    
    if (!return_likelihood)
    {
      IA_data$elo_w_before[i] <- currentELO[ind1]
      IA_data$elo_l_before[i] <- currentELO[ind2]
    }
    # calculate predited winning probablity of the winner
    p_win <- as.numeric(1/(1+exp(-.01*(currentELO[ind1] - currentELO[ind2]))))
     # print(IA_data$elo_w_after[i])
    # Calculation of new ELO scores
    if (i <= burn_in) # during burn-in period all k values are fixed to 100
    {
      currentELO[ind1] <- currentELO[ind1] + 100 * (1 - p_win) # new Elo score of the Winner
      currentELO[ind2] <- currentELO[ind2] - 100 * (1 - p_win) # new Elo score of the Loser
    }
    else # after the burn-in period fitted k values are used
    {
      currentELO[ind1] <- currentELO[ind1] + exp(k) * (1 - p_win) # new Elo score of the Winner
      currentELO[ind2] <- currentELO[ind2] - exp(k) * (1 - p_win) # new Elo score of the Loser
    }
    # write calculated elo scores to output columns
    if (!return_likelihood)
    {
      IA_data$elo_w_after[i] <- currentELO[ind1]
      IA_data$elo_l_after[i] <- currentELO[ind2]
    }
    # Update log likelihood
    if (i > burn_in) L <- L + log(p_win)
  }
  if (return_likelihood) return(-1*L)
  else return(IA_data)
}

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
