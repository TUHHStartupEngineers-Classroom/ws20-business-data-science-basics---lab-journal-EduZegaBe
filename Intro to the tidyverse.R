class(cars)
## "data.frame"

cars_tbl <- as_tibble(cars)
class(cars_tbl)

vehicles <- as_tibble(cars[1:5,])

vehicles %>% .$dist

vehicles %>% .[['speed']]

vehicles %>% .[[2]]

library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")



pivot_longer(diamonds2, cols      = c("2008", "2009"), 
              names_to  = 'year', 
              values_to = 'price')


diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 5)

diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n =6 )



diamonds4 <- readRDS("diamonds4.rds")

diamonds4

diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

diamonds5 <- readRDS("diamonds5.rds")

diamonds5

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')