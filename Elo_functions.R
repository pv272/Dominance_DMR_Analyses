### FROM MATTIAS SCRIPT 
#################################################################################
# Model 3 -- fitting of initial scores and k
################################################################################
elo.model3 <- function(par, IA_data, all_ids, return_likelihood = T)
{
  k <- par[1]
  init_elo <- par[2:length(par)]
  # Initialize output columns
  if (!return_likelihood) IA_data$elo_l_before <- IA_data$elo_w_before <- IA_data$elo_l_after <-
    IA_data$elo_w_after <- NA
  # Set intitial elo scores
  currentELO <- c(init_elo)
  names(currentELO) <- all_ids
  # Initialize the log likelihood
  L <- 0
  # Start loop
  for(i in 1:nrow(IA_data))
  {
    ind1 <- which(names(currentELO)==IA_data$Winner[i])
    ind2 <- which(names(currentELO)==IA_data$Loser[i])
    if (!return_likelihood)
    {
      IA_data$elo_w_before[i] <- currentELO[ind1]
      IA_data$elo_l_before[i] <- currentELO[ind2]
    }# calculate predited winning probablity of the winner
    p_win <- 1/(1+exp(-.01*(currentELO[ind1] - currentELO[ind2])))
    # Calculation of new ELO scores
    currentELO[ind1] <- currentELO[ind1] + exp(k) * (1 - p_win) # new Elo score of the Winner
    currentELO[ind2] <- currentELO[ind2] - exp(k) * (1 - p_win) # new Elo score of the Loser
    # write calculated elo scores to output columns
    if (!return_likelihood)
    {
      IA_data$elo_w_after[i] <- currentELO[ind1]
      IA_data$elo_l_after[i] <- currentELO[ind2]
    }
    # Update log likelihood
    L <- L + log(p_win)
  }
  if (return_likelihood) return(-1*L)
  else return(IA_data)
}

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

### 

plot_elo <- function(x) {
  out <-  x %>% select(Loser, elo_l_after, ObsDate) %>% 
    rename(ID = Loser, Elo = elo_l_after, date = ObsDate) %>%
    bind_rows(x %>% select(Winner, elo_w_after, ObsDate) %>% 
                rename(ID = Winner, Elo = elo_w_after, date = ObsDate)) %>%
    mutate(year = lubridate::year(date))
  
  ggplot(out, aes(x = date, y = Elo, col = ID)) +
    geom_line(aes(x = date, y = Elo, col = ID))+
    geom_point(shape = 3)+
    facet_wrap(~year, scales = "free_x")
} ## function to plot the elo based on the master_df 


#### get pair of 2 IDs  put them in order
get_pair <- function(A, B) {
  x <- tibble(name = c(A, B), 
              order = order(name)) %>% 
    arrange(order) %>% pull(name)
  out <- paste0(x, collapse = "_")
}
