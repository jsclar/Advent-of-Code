library(tidyverse)
library(magrittr)

my_dat <- tibble(raw = read_lines("2021/dat/day05.txt"))

my_dat %<>%
  mutate(x1 = str_extract(raw, "^\\d+") %>% as.numeric(),
         y1 = str_extract(raw, "(?<=\\d,)\\d+(?=[:space:]->)") %>% as.numeric(),
         x2 = str_extract(raw, "(?<=->[:space:])\\d+") %>% as.numeric(),
         y2 = str_extract(raw, "\\d+$") %>% as.numeric())


#part1

my_dat %>%
  filter(x1 == x2 | y1 == y2) -> d1

vert <- my_dat %>% filter(x1 == x2)
horiz <- my_dat %>% filter(y1 == y2)

points <- tibble()

for(i in 1:nrow(vert)){
  x <- vert$x1[i]
  y <- seq(from = min(vert$y1[i], vert$y2[i]), to = max(vert$y1[i], vert$y2[i]), by = 1)
  points %<>% bind_rows(tibble(x = x, y = y))
}

for(i in 1:nrow(horiz)){
  y <- horiz$y1[i]
  x <- seq(from = min(horiz$x1[i], horiz$x2[i]), to = max(horiz$x1[i], horiz$x2[i]), by = 1)
  points %<>% bind_rows(tibble(x = x, y = y))
}

points %>%
  group_by(x, y) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() -> overlap

nrow(overlap)

##### part 2
diag <- my_dat %>%
  filter(x1 != x2 & y1 != y2)

for(i in 1:nrow(diag)){
  x = seq(from = diag$x1[i], to = diag$x2[i])
  y = seq(from = diag$y1[i], to = diag$y2[i])
  points %<>% bind_rows(tibble(x = x, y = y))
}

points %>%
  group_by(x, y) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() -> overlap2

nrow(overlap2)
