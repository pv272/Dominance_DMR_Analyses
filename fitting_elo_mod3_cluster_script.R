rm(list = ls())
library(purrr)
library(dplyr)
library(tidyr)
load("AllCall_Elo.rda")
source("function_cluster.R")
### optim K by colony /// TO BE RUN ON CLUSTER 

# d3 <- AllCall_Elo %>% nest(-Colony) %>%
#   mutate(all_ids = map(data, ~ unique(c(.x$Winner, .x$Loser)))) %>% ## get list of all ids (needed for fitting the elo)
#   mutate(lik1 = map2(data, all_ids, ~ optim(par=4,  
#                                                fn = elo.model1,
#                                                burn_in=10, 
#                                                all_ids = .y, 
#                                                IA_data = .x,
#                                                return_likelihood=T, 
#                                                init_elo = 100, 
#                                                method= "Brent", 
#                                                upper = 15, 
#                                                lower = 0)), 
#         lik3 = map2(data, all_ids, ~ optim(par=c(5, rep(0, length(.y))),  
#                                                fn = elo.model3,
#                                                all_ids = .y, 
#                                                IA_data = .x,
#                                                return_likelihood=T, 
#                                                method='BFGS', 
#                                                control = list(maxit = 10000, reltol=1e-10))))
# 
# save(d3, file = "d3.rda")

##### run the first part on the cluster 
load("d3.rda") ## output of the computation on cluster


### get AIC ## compare the AIC for model 1 and 3!! Be carefull the output of the function is -log(Lik)
### so (1) the smaller the value, the better the model fits, and so (2) no need to put a "-" when computing the AIC

lik1 <- sum(map_dbl(d3$lik1, ~ .x$value)) ## sum of -log(lik) # mod1
lik3 <- sum(map_dbl(d3$lik3, ~ .x$value)) ## sum of -log(lik) # mod3
par1 <- sum(map_dbl(d3$lik1, ~ length(.x$par))) ## sum of fitted param # mod1
par3 <- sum(map_dbl(d3$lik3, ~ length(.x$par))) ## sum of fitted param # mod3

AIC1 <-  2*par1 + 2*lik1 # AIC1
AIC3 <-  2*par3 + 2*lik3 # AIC3

############################ get AIC with mod 3 and plot it

d3 <- d3 %>%
  mutate(df_elo3 = pmap(list(x = all_ids, y = lik3, z = data), function(x,y,z){
                        elo.model3(par = y[["par"]], all_ids = x, IA_data = z, return_likelihood = F) ## fit elo with mod3 and good params
})) %>% mutate(plot3 = map(df_elo3, ~ .x %>% # add a new col in the df with the plots for each colony
                           select(Date, elo_w_after, Winner) %>% ## just reorder the data to have all Ids in 1 col 
                           rename(elo = elo_w_after, id = Winner) %>%
                           bind_rows(.x %>% 
                                       select(Date, elo_l_after, Loser) %>%
                                       rename(elo = elo_l_after, id = Loser)) %>%
                           ggplot(aes(x = Date, y = elo, col = id)) +
                           geom_line() +
                             geom_point(shape = 4))
                           )


###### same for mod1 
d3 <- d3 %>%
  mutate(df_elo1 = pmap(list(x = all_ids, y = lik1, z = data), function(x,y,z){
    elo.model1(par = y[["par"]], all_ids = x, IA_data = z, init_elo = 100, burn_in = 10, return_likelihood = F) ## fit elo with mod3 and good params
  })) %>% mutate(plot1 = map(df_elo1, ~ .x %>% # add a new col in the df with the plots for each colony
                              select(Date, elo_w_after, Winner) %>% ## just reorder the data to have all Ids in 1 col 
                              rename(elo = elo_w_after, id = Winner) %>%
                              bind_rows(.x %>% 
                                          select(Date, elo_l_after, Loser) %>%
                                          rename(elo = elo_l_after, id = Loser)) %>%
                              ggplot(aes(x = Date, y = elo, col = id)) +
                              geom_line() +
                              geom_point(shape = 4))
  )



### to access a plot or all do: 

d3$plot3 ## display all plots fitted with mod 3
d3$plot1 ## display all plots fitted with mod 1

### or a specific one

d3$plot3[[2]]
d3$plot1[[2]]
