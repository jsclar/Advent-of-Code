library(tidyverse)
library(magrittr)

options(scipen = 999)

my_raw <- read_lines("2021/dat/day14.txt")

polymer <- my_raw[1]

links <- tibble(raw = my_raw[3:length(my_raw)]) %>%
  separate(raw, into = c("from", "plus"), sep = "[:space:]->[:space:]") %>%
  mutate(to = str_c(str_sub(from, 1, 1), plus, str_sub(from, 2, 2)))

#str_sub("NPN", 1, -2) drop the last letter 
advance_polymer <- function(my_polymer){
  #tokenize polymer
  my_subs <- c()
  for(i in 1:(nchar(my_polymer)-1)){
    my_subs[i] <- str_sub(my_polymer, i, i+1)
  }
  
  #replace everything replacable from links df
  my_subs[my_subs %in% links$from] <- links$to[sapply(my_subs[my_subs %in% links$from], FUN=function(x)which(links$from == x))]
  
  #recombine list
  mnp <- ""
  for(i in 1:(length(my_subs)-1)){
    mnp <- str_c(mnp, str_sub(my_subs[i], 1, -2))
  }
  mnp <- str_c(mnp, my_subs[length(my_subs)])
  return(mnp)
}

#part 1
ap <- polymer

for(i in 1:10) ap <- advance_polymer(ap)

tokens1 <- str_extract_all(ap, ".{1}")
count1 <- table(tokens1)

max(count1) - min(count1)

###need a new strat for p2

ft <- tibble(key = links$from, count = 0)

my_subs <- c()
for(i in 1:(nchar(polymer)-1)){
  my_subs[i] <- str_sub(polymer, i, i+1)
}

for(i in 1:length(my_subs)){
  ft$count[which(ft$key == my_subs[i])] <- ft$count[which(ft$key == my_subs[i])] + 1
}

links %<>%
  mutate(n1 = str_sub(to, 1, 2),
         n2 = str_sub(to, 2, 3))

advance_polymer_table <- function(my_ft){
  nft <- tibble(key = links$from, count = 0)
  
  for(i in 1:nrow(my_ft)){
    if(my_ft$count[i] == 0) next
    nft$count[which(nft$key == links$n1[which(links$from == my_ft$key[i])])] <- 
      nft$count[which(nft$key == links$n1[which(links$from == my_ft$key[i])])] + my_ft$count[i]
    nft$count[which(nft$key == links$n2[which(links$from == my_ft$key[i])])] <- 
      nft$count[which(nft$key == links$n2[which(links$from == my_ft$key[i])])] + + my_ft$count[i]
  }
  return(nft)
}


#part 1
wt <- ft
for(i in 1:10) wt <- advance_polymer_table(wt)

wt %>% mutate(fl = str_sub(key, 1, 1)) %>%
  group_by(fl) %>%
  summarise(count = sum(count)) %>%
  mutate(count = count + if_else(fl == str_sub(polymer, -1, -1), 1, 0)) %>%
  summarise(my_max = max(count), my_min = min(count)) %>%
  rowwise() %>%
  mutate(my_ans = my_max - my_min) %>%
  pull(my_ans)


#part 2
wt <- ft
for(i in 1:40) wt <- advance_polymer_table(wt)

wt %>% mutate(fl = str_sub(key, 1, 1)) %>%
  group_by(fl) %>%
  summarise(count = sum(count)) %>%
  mutate(count = count + if_else(fl == str_sub(polymer, -1, -1), 1, 0)) %>%
  summarise(my_max = max(count), my_min = min(count)) %>%
  rowwise() %>%
  mutate(my_ans = my_max - my_min) %>%
  pull(my_ans)

