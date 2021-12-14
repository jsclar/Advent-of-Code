library(tidyverse)
library(magrittr)

my_dat <- tibble(raw = read_lines("2021/dat/day02.txt")) %>%
  mutate(dir = str_extract(raw, "^[a-z]+"),
         steps = str_extract(raw, "\\d+$") %>% as.numeric())


#part 1
my_dat %>%
  group_by(dir) %>%
  summarise(count = sum(steps)) -> movement

vert <- movement %>% filter(dir == "down") %>% pull(count) - movement %>% filter(dir == "up") %>% pull(count)
horiz <- movement %>% filter(dir == 'forward') %>% pull(count)

vert * horiz 


### part 2

my_dat %<>%
  mutate(ac = case_when(
           dir == "down" ~ steps,
           dir == "up" ~ -1 * steps,
           dir == "forward" ~ 0
         ),
         aim = cumsum(ac))

my_dat %>% 
  filter(dir == "forward") %>%
  mutate(dc = steps * aim) %>%
  summarise(cd = sum(dc)) %>% pull() -> vert2


vert2 * horiz