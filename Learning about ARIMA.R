library(tidyverse)
library(fpp3)

aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  autoplot(Takings)

aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  ACF(Takings) %>% 
  autoplot()

aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  features(Takings, unitroot_kpss)

aus_accommodation %>% 
  mutate(diff = difference(Takings)) %>% 
  features(diff, unitroot_kpss)

aus_accommodation %>% 
  features(Takings, unitroot_ndiffs)

fit <- aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  model(ARIMA(Takings))

report(fit)

fit %>% forecast(h = 4) %>% 
  autoplot(aus_accommodation) %>% 
  labs(y = 'Put something here', title = "This is a title")

aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  gg_tsdisplay(y = Takings, plot_type = 'partial')

aus_accommodation %>% 
  filter(State == 'Australian Capital Territory') %>% 
  mutate(diff = difference(Takings)) %>% 
  gg_tsdisplay(y = diff, plot_type = 'partial')

##################

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  autoplot()

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  ACF(Employed) %>% 
  autoplot()

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  features(Employed, unitroot_kpss)

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  features(Employed, unitroot_ndiffs)

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  mutate(diff = difference(Employed)) %>% 
  gg_tsdisplay(y = diff, plot_type = 'partial')

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  mutate(diff2 = difference(difference(Employed))) %>% 
  gg_tsdisplay(y = diff2, plot_type = 'partial')

fit <- us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  model(ARIMA(Employed))
report(fit)

###################

us_gasoline %>% 
  autoplot()

us_gasoline %>% 
  ACF(Barrels) %>% 
  autoplot()

us_gasoline %>% 
  ACF(Barrels) %>% 
  autoplot()

us_gasoline %>% 
  features(Barrels, unitroot_kpss)

us_gasoline %>% 
  features(Barrels, unitroot_ndiffs)

us_gasoline %>% 
  mutate(diff = difference(Barrels)) %>% 
  features(diff, unitroot_kpss)

us_gasoline %>% 
  mutate(diff = difference(Barrels)) %>% 
  gg_tsdisplay(y = diff, plot_type = 'partial')

us_gasoline %>% 
  mutate(diff2 = difference(difference(Barrels))) %>% 
  features(Barrels, unitroot_kpss)

us_gasoline %>% 
  mutate(diff2 = difference(difference(Barrels))) %>% 
  gg_tsdisplay(y = diff2, plot_type = 'partial')

us_gas_fit <- us_gasoline %>% 
  model(
    stepwise = ARIMA(Barrels),
    search = ARIMA(Barrels, stepwise = FALSE))
us_gas_fit

us_gas_fit %>% pivot_longer(names_to = "Model name", values_to = "Barrels")
glance(us_gas_fit) %>% arrange(AICc) %>% select(.model:BIC)

us_gas_fit %>% 
  select(search) %>% 
  gg_tsresiduals()

global_economy %>%
  filter(Code == "CAF")

augment(us_gas_fit) %>% 
  features(.innov, ljung_box, lag = 12, dof = 3)

us_gas_fit %>% 
  forecast(h = 5)
