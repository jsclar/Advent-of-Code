library(tidyverse)
library(magrittr)


t1 <- "day12_t1.txt"
t2 <- "day12_t2.txt"
t3 <- "day12_t3.txt"
fd <- "day12.txt"

my_dat <- tibble(raw = read_lines(str_c("2021/dat/", fd))) %>%
  separate(raw, into = c("from", "to"), sep = "-")

rev_dat <- my_dat %>% rename(from = to, to = from)

vp <- my_dat %>% bind_rows(rev_dat)

nodes <- vp %>% group_by(from) %>% summarise() %>% pull(from)

seed_node <- "start"

count_paths <- function(cur_node, remaining_nodes, paths){
  #base case, if we are at the end, we have found a way
  if(cur_node == "end") return(1)
  
  #if our current node can only be visited once, remove it from list of remaining nodes
  if(cur_node == str_to_lower(cur_node))
    remaining_nodes <- remaining_nodes[-which(remaining_nodes == cur_node)[1]]
  
  targets <- paths %>% filter(from == cur_node) %>% pull(to)
  
  proposals <- targets[which(targets %in% remaining_nodes)]
  
  #if there are no valid paths on, return 0
  if(length(proposals) == 0) return(0)
  
  pos_paths <- 0
  for(i in 1:length(proposals)){
    nn <- proposals[i]
    
    pos_paths <- pos_paths + count_paths(nn, remaining_nodes, paths)
  }
  return(pos_paths)
}

#part 1
p1_ans <- count_paths(seed_node, nodes, vp)
print(p1_ans)

### part 2
replicable_nodes <- nodes[which(nodes == str_to_lower(nodes))]
replicable_nodes <- replicable_nodes[-which(replicable_nodes %in% c("start", "end"))]

pp <- 0
for(i in 1:length(replicable_nodes)){
  pn <- c(nodes, replicable_nodes[i])
  x <- count_paths(seed_node, pn, vp)
  pp <- pp + x
}

pp - (length(replicable_nodes) - 1) * p1_ans
