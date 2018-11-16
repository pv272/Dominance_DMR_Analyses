library(RMySQL)
library(getPass)
library(EloRating)
# library(tidyverse)
library(lubridate)
library(dplyr)
library(purrr)
library(dbplyr)


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


####### get ID infos 
# DF1 <- Membership
# DF2 <- Membership[1:10, c(2,3)] %>% rename(ID = AnimalID, 
#                                               Date = MemberFrom)

get_group <- function(DF1, DF2){
  
  DF2 %>% inner_join(DF1, by = c("ID" = "AnimalID")) %>%
    mutate(Date = ymd(Date), 
           MemberFrom = ymd(MemberFrom), 
           MemberTo = ymd(MemberTo)) %>% 
    filter(Date >= MemberFrom & Date <= MemberTo)
}




get_group_from_ID_and_Date <- function(ID =NA, 
                                       DATE =NA, ## in date format to speed up
                                       DF_M = NULL, ## to be provided for large data set, so no need to connect to the db.
                                       password = "XXX"){ ## if DB not true need a df in R with info on ID 

if(!(is.Date(DATE))){
  DATE <- ymd(DATE)
}
if (is.null(DF_M)) { 
  con <- dbConnect(MySQL(), user = 'philippev', password = password,  
                   dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
  DF_M <- dbGetQuery(con, "SELECT AnimalRef, AnimalID, DATE(MemberFrom) as MemberFrom, DATE(MemberTo) as MemberTo, 
                           ColonyRef, Colony, MemberDays FROM MoleratViews_Pending.MemberShipBetween
WHERE ColonyRef <> 120") %>% mutate(MemberFrom = ymd(MemberFrom), 
                                 MemberTo = ymd(MemberTo))
  dbDisconnect(con)

} else if (!(is.Date(DF_M$MemberFrom) & is.Date(DF_M$MemberTo))) {
  DF_M <- DF_M %>% mutate(MemberFrom = ymd(MemberFrom), 
                                        MemberTo = ymd(MemberTo))
}
  
out <- DF_M$Colony[DF_M$AnimalID == ID & DF_M$MemberTo >= DATE & DF_M$MemberFrom <= DATE]
return(out)
}

# 
# DF3 <- get_group(DF1, DF2)
get_group_compo <- function(DF3, DF1) {
  t3 <- DF3 %>% select(Colony, Date) %>% inner_join(., DF1, by= c("Colony" = "Colony")) %>%
    is.equal(t, t2)
    mutate(Date = ymd(Date), 
           MemberFrom = ymd(MemberFrom), 
           MemberTo = ymd(MemberTo)) %>% 
    filter(Date >= MemberFrom & Date <= MemberTo)
  
}
# test <- get_group_compo(DF3, DF1)


### example ###########################################################
# DF_test_function <-DF2 %>% mutate(new = map2(ID, Date, ~ get_group_from_ID_and_Date(ID = .x, DATE = .y, DF_membership = Membership)))
# DF_test_df <- get_group(DF1, DF2)

#set seed to get always the same randomization
get_group_members <- function(ID = NULL, ## if group has to be search based on an ID 
                              GROUP = NULL, ## if group is already known 
                              DATE =NA, ## in date format to speed up
                              DF_M = NA ## need to be provided
                              ){
  
  if (is_empty(GROUP)) {
  GROUP <- as.character(get_group_from_ID_and_Date(ID = ID, DATE = DATE, DF_M = DF_M))
  }
  
  if(!(is.Date(DATE))){
  DATE <- ymd(DATE)
  }
  
  if (!(is.Date(DF_M$MemberFrom) & is.Date(DF_M$MemberTo))) {
    
    DF_M <- DF_M %>% mutate(MemberFrom = ymd(MemberFrom), 
                            MemberTo = ymd(MemberTo))
  }
  
  out <- DF_M[DF_M$Colony %in% GROUP & DF_M$MemberTo >= DATE & DF_M$MemberFrom <= DATE, ] 
  out$date <- DATE
  return(out)
}
# test_function4 <- map2(DF_test_function$ID, DF_test_function$Date, ~ get_group_members(.x, .y, DF_membership = Membership))
# x <- bind_rows(test_function4)
# 
# test_df4 <- get_group_compo(DF3, DF1)
# anti_join(test_df4, x)
