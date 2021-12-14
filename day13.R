library(tidyverse)
library(magrittr)

my_raw <- read_lines("2021/dat/day13.txt")
my_points <- my_raw[str_detect(my_raw, "^\\d+")]

my_dat <- tibble(raw = my_points) %>%
  separate(raw, into = c("x", "y"), sep = ",") %>%
  mutate(x = as.numeric(x), y = as.numeric(y))

my_folds <- tibble(raw = my_raw[str_detect(my_raw, "^fold")]) %>%
  mutate(axis = str_extract(raw, "[xy]{1}"),
         line = str_extract(raw, "\\d+$") %>% as.numeric()) 


fold_along_x <- function(my_paper, my_along){
  above <- my_paper %>% filter(x <= my_along)
  below <- my_paper %>% filter(x > my_along)
  
  below %<>%
    mutate(nx = 2*my_along - x) %>%
    select(nx, y) %>%
    rename(x = nx)
  
  nd <- above %>%
    bind_rows(below) %>%
    group_by(x, y) %>%
    summarise() %>% ungroup()
  
  return(nd)
}

fold_along_y <- function(my_paper, my_along){
  left <- my_paper %>% filter(y <= my_along)
  right <- my_paper %>% filter(y > my_along)
  
  right %<>%
    mutate(ny = 2*my_along - y) %>%
    select(x, ny) %>%
    rename(y = ny)
  
  nd <- left %>%
    bind_rows(right) %>%
    group_by(x, y) %>%
    summarise() %>% ungroup()
  
  return(nd)
}

#part 1
s1 <- fold_along_x(my_dat, 655)
print(nrow(s1))

#part 2
p2d <- my_dat

for(i in 1:nrow(my_folds)){
  if(my_folds$axis[i] == "x") p2d %<>% fold_along_x(my_folds$line[i])
  if(my_folds$axis[i] == "y") p2d %<>% fold_along_y(my_folds$line[i])
}


plot(x = p2d$x, y = 7-p2d$y, pch = 17)