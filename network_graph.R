rm(list = ls())
library(readr)
library(dplyr)
library(igraph)
library(purrr)
d <- read_csv("SubCall.csv")
d <- d %>% filter(d$ObsType == "Scan")

d <- split(d, d$Colony)

get_pair <- function(A, B) {
 x <- tibble(name = c(A, B), 
         order = order(name)) %>% 
    arrange(order) %>% pull(name)
  out <- paste0(x, collapse = "_")
}

### create a df of interaction 
d2 <- d[[33]] %>% select(Winner, Loser) %>% 
  mutate(pair_ID = map2_chr(Winner, Loser, ~ get_pair(.x,.y))) %>%
  group_by(pair_ID) %>% 
  summarise(n = n()) %>%
  tidyr::separate(pair_ID, into =c("A", "B"), sep = "_") %>% 
  filter(n > 3)

all_names <- c(d2$A, d2$B)
unique_names <- unique(c(d2$A, d2$B))
d_nodes <- data.frame(name = unique_names, 
                      sex =  ifelse(stringr::str_detect(unique_names, "M"), "male", "female"))


x <- graph_from_data_frame(d = d2, vertices = d_nodes, directed = FALSE)

g <- simplify(x)
E(x)
vertex_attr(g)
edge_attr(g)

V(g)$color <- ifelse(V(g)$sex == "female", "orange", "dodgerblue")


plot(g, edge.width = E(g)$n)
plot(g, layout = layout_nicely(g), edge.width = E(g)$n/2, edge.connectivity(g))

