library(fpp3)
library(tidyverse)
employed <- us_employment %>% 
  select(Month, Employed)
employed <- as_tsibble(employed)
duplicates(employed)

#employed %>% 
ACF(employed) %>% 
  filter(Series_ID=='CEU0800000001') %>% 
  autoplot()

PBS %>% 
  filter(ATC2 == "H02") %>% 
  summarise(Cost = sum(Cost)/1e6) %>% 
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` = difference(difference(log(Cost), 12), 1)
  ) %>% 
  pivot_longer(-Month, names_to = "Type", values_to = "Sales") %>% 
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) %>% 
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

PBS

us_employment %>% 
  filter(Series_ID=='CEU0800000001') %>% 
  summarise(Employed = sum(Employed)) %>% 
  transmute(
    `num_employed` = Employed,
    `Log num_employed` = log(Employed),
    `Annual change in log employed` = difference(log(Employed), 12),
    `Doubly differenced log employed` = difference(difference(log(num_employed), 12), 1),
    `Triply differenced log employed` = difference(difference(difference(log(num_employed), 12), 1)),
    `Quad differenced log employed` = difference(difference(difference(difference(log(num_employed), 12), 1)))
  ) %>% 
  pivot_longer(-Month, names_to = "Type", values_to = "Employed") %>% 
  mutate(
    Type = factor(Type, levels = c(
      "num_employed",
      "Log num_employed",
      "Annual change in log employed",
      "Doubly differenced log employed",
      "Triply differenced log employed",
      "Quad differenced log employed"))
  )  %>% 
  ACF(Employed) %>% 
  autoplot()
  
  
  
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)
  )

us_employment %>% 
  filter(Series_ID=='CEU0800000001') %>% 
  ACF(Employed) %>% 
  autoplot()
