library(tidyverse)
library(magrittr)

my_dat <- tibble(raw=read_lines("2021/dat/day08.txt"))

#part 1
md <- str_split(my_dat$raw, "\\|") %>% sapply("[[",2) %>% str_trim() %>%
  str_split("\\s") %>% sapply(c) %>% t() %>% data.frame() %>% tibble() %>%
  rename(l1 = 1, l2 = 2, l3 = 3, l4 = 4)


my_nums <- str_split(my_dat$raw, "\\|") %>% sapply("[[",2) %>% str_trim() %>% 
  str_c(collapse = " ") %>% str_split("\\s") %>% unlist() %>% str_length()

sapply(c(2, 3, 4,7), function(x)sum(my_nums == x)) %>% sum()

### part 2
inputs <- str_split(my_dat$raw, "\\|") %>% sapply("[[",1) %>% str_trim()
outputs <- str_split(my_dat$raw, "\\|") %>% sapply("[[",2) %>% str_trim()

my_decode <- function(my_input){
  my_nums <- str_split(my_input, "\\s") %>% unlist()
  
  v1 <- my_nums[which(str_length(my_nums) == 2)] 
  v7 <- my_nums[which(str_length(my_nums) == 3)] 
  v4 <- my_nums[which(str_length(my_nums) == 4)] 
  v8 <- my_nums[which(str_length(my_nums) == 7)]
  
  my_nums <- my_nums[-which(my_nums == v1)]
  my_nums <- my_nums[-which(my_nums == v7)]
  my_nums <- my_nums[-which(my_nums == v4)]
  my_nums <- my_nums[-which(my_nums == v8)]
  
  v1 %<>% str_extract_all(".{1}") %>% unlist()
  v7 %<>% str_extract_all(".{1}") %>% unlist()
  v4 %<>% str_extract_all(".{1}") %>% unlist()
  v8 %<>% str_extract_all(".{1}") %>% unlist()
  
  top <- v7[!(v7 %in% v1)]
  
  cand9 <- my_nums[which(str_length(my_nums) == 6)] %>% str_extract_all(".{1}")
  find4 <- lapply(cand9, function(x)sum(v4 %in% x)) %>% unlist()
  v9 <- my_nums[which(str_length(my_nums) == 6)][find4 == 4]
  my_nums <- my_nums[-which(my_nums == v9)]
  v9 %<>% str_extract_all(".{1}") %>% unlist()
  ll <- v8[!(v8 %in% v9)]
  
  cand0 <- my_nums[which(str_length(my_nums) == 6)] %>% str_extract_all(".{1}")
  find1 <- lapply(cand0, function(x)sum(v1 %in% x)) %>% unlist()
  v0 <- my_nums[which(str_length(my_nums) == 6)][find1 == 2]
  my_nums <- my_nums[-which(my_nums == v0)]
  v0 %<>% str_extract_all(".{1}") %>% unlist()
  middle <- v8[!(v8 %in% v0)]
  
  v6 <- my_nums[which(str_length(my_nums) == 6)]
  my_nums <- my_nums[-which(my_nums == v6)]
  v6 %<>% str_extract_all(".{1}") %>% unlist()
  ur <- v8[!(v8 %in% v6)]
  
  lr <- v1[!(v1 %in% ur)]
  
  ul <- v4[!(v4 %in% c(middle, v1))]
  bottom <- v8[!(v8 %in% c(v4, top, ll))]
  
  my_code = list(top = top, ul = ul, ur = ur, middle = middle, ll = ll, lr = lr, bottom = bottom)
  return(my_code)
}

my_encoder <- function(my_output, my_code){
  my_output %<>% str_split("\\s") %>% unlist()
  
  d1 <- compute_number(my_output[1], my_code)
  d2 <- compute_number(my_output[2], my_code)
  d3 <- compute_number(my_output[3], my_code)
  d4 <- compute_number(my_output[4], my_code)
  
  my_code <- str_c(d1, d2, d3, d4) %>% as.numeric()
  return(my_code)
}

compute_number <- function(my_wires, my_code){
  if(str_length(my_wires) == 2) return(1)
  if(str_length(my_wires) == 3) return(7)
  if(str_length(my_wires) == 4) return(4)
  if(str_length(my_wires) == 7) return(8)
  
  my_wires %<>% str_extract_all(".{1}") %>% unlist()
  
  if(length(my_wires) == 6){
    if(!(my_code$middle %in% my_wires)) return(0)
    if(!(my_code$ll %in% my_wires)) return(9)
    if(!(my_code$ur %in% my_wires)) return(6)
  }
  
  if(!(my_code$ul %in% my_wires) & !(my_code$lr %in% my_wires)) return(2)
  if(!(my_code$ul %in% my_wires) & !(my_code$ll %in% my_wires)) return(3)
  if(!(my_code$ur %in% my_wires) & !(my_code$ll %in% my_wires)) return(5)
}

my_ans <- 0
for(i in 1:length(inputs)){
  nc <- my_decode(inputs[i])
  my_ans <- my_ans + my_encoder(outputs[i], nc)
}

