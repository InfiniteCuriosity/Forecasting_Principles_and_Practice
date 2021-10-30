library(tidyverse)
library(fpp3)
library(fable)

vic_elec %>% 
  select(Date, Demand) %>% 
  autoplot(Demand)

vic_elec %>% 
  select(Date, Temperature) %>% 
  autoplot(Temperature)

vic_elec %>% 
  ggplot(aes(x = Date, y = Demand)) +
  geom_point()

vic_elec %>% 
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  geom_smooth(method = 'gam')

vic_elec %>%
  select(Date, Demand, Holiday) %>% 
  filter(Holiday == "FALSE") %>% 
  autoplot(Demand)

t2 <- vic_elec %>% 
  model(stl = STL(Demand))

components(t2) %>% 
  as_tsibble() %>% 
  autoplot(Demand)

components(t2) %>% autoplot()

components(t2) %>% 
  as_tsibble() %>% 
  autoplot(Demand) +
  geom_line(aes(y = season_adjust), colour = "0072B2")

t3 <- vic_elec %>% 
  mutate(
    `5-MA` = slider::slide_dbl(Demand, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

t3 %>% 
  autoplot(Demand) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00")

vic_elec %>% 
  model(
    classical_decomposition(Demand, type = "additive")
  ) %>% 
  components() %>% 
  autoplot()

vic_elec %>% 
  model(
    STL(Demand ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()

vic_elec %>% 
  features(Demand, feat_stl)

vic_elec %>%
  group_by(Holiday) %>% 
  autoplot(Demand) +
  labs(y = "Put something here", title = "Another boring title")

fit <- vic_elec %>% 
  model(trend_model = TSLM(Demand ~ trend()))
fit %>% forecast(h = "3 years")

fit %>% 
  forecast(h = "3 years") %>% 
  autoplot()

augment(fit)
aug <- vic_elec %>% 
  model(SNAIVE(Demand)) %>% 
  augment()
autoplot(aug, .innov)

aug %>% 
  ggplot(aes(x = .innov)) +
  geom_histogram()

aug %>% 
  ACF(.innov) %>% 
  autoplot()

demand_fit <- vic_elec %>% 
  model(
    Mean = MEAN(Demand),
    `Naive` = NAIVE(Demand),
    `Seasonal naive` = SNAIVE(Demand),
    Drift = RW(Demand ~ drift())
  )

demand_forecast <- demand_fit %>% 
  forecast(h = 10)

demand_forecast %>% 
  autoplot() +
  facet_wrap(~.model)

vic_elec %>% 
  pivot_longer(c(Demand, Temperature), names_to = "Series") %>% 
  autoplot(name ~., scales = "free_y")
vic_elec %>% 
  pivot_longer(c(Demand, Temperature))
  ggplot(aes(Quarter, color = name)) +
  geom_line() +
  facet_grid(name~., scales = "free_y")
