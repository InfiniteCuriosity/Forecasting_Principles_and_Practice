library(tidyverse)
library(fpp3)
us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>% 
  autoplot(Employed)

us_employment %>% 
  filter(Series_ID == 'CEU0500000001') %>%
  gg_tsdisplay(difference(Employed), plot_type = 'partial')

us_change %>% 
  gg_tsdisplay(difference(difference(Consumption)), plot_type = 'partial')

aus_airpassengers %>% 
  gg_tsdisplay(difference(Passengers), plot_type = 'partial')

aus_airpassengers %>% 
  gg_tsdisplay(plot_type = 'partial')

aus_airpassengers %>% 
  mutate(diff = difference(difference(Passengers))) %>% 
  features(diff, ljung_box, lag = 12)

aus_airpassengers %>% 
  features(Passengers, unitroot_kpss)

aus_airpassengers %>% 
  mutate(diff_passengers = difference(Passengers)) %>% 
  features(diff_passengers, unitroot_kpss)

aus_airpassengers %>% 
  mutate(2diff_passengers = difference(difference(Passengers))) %>% 
  features(2diff_passengers, unitroot_kpss)

aus_airpassengers %>% 
  features(Passengers, unitroot_ndiffs)

fit <- aus_airpassengers %>% 
  model(ARIMA(Passengers))
report(fit)



test1 <- aus_airpassengers %>% 
  model(
    stepwise = ARIMA(Passengers),
    search = ARIMA(Passengers)
  )

test1

test1 %>% pivot_longer(names_to = "Model name", values_to = "Passengers")

glance(test1) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)

test1 %>% 
  select(stepwise) %>% 
  gg_tsresiduals()

google_2015 %>% 
  filter(Close>575)

google_2015[140:nrow(google_2015),]

google_2015 %>% 
  mutate(diff = difference(Close)) %>% 
  gg_tsresiduals()
  
autoplot(diff)

google_2015

google_2015 %>% 
  mutate(diff2 = difference(difference(Close))) %>% 
  autoplot(diff2)

google_2015 %>% 
  mutate(diff4 = difference(difference(difference(difference(Close))))) %>% 
  autoplot(diff4)


sunspots.ar <- ar(sunspot.year)
sunspots.ar

PBS %>% 
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
      `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) %>% 
  features(`Doubly differenced log sales`, unitroot_kpss)

fit <- PBS %>% 
  filter(ATC2 == "H02" & Concession == 'General' & Type == 'Safety net') %>% 
  model(ARIMA(Cost))
report(fit)

PBS %>% 
  filter(ATC2 == "H02" & Concession == 'General' & Type == 'Safety net') %>%
  gg_tsdisplay(difference(difference(Cost)), plot_type = 'partial')


  aus_economy <- global_economy %>%
    filter(Code == "AUS") %>%
    mutate(Population = Population/1e6)
  aus_economy %>%
    model(ETS(Population)) %>%
    forecast(h = "5 years")

library(clock)
flight1 <- nycflights13::flights %>% 
  select(year, month, day) %>% 
  count(year, month, day) %>% 
  as_tsibble(index = day, key = n) %>% 
  duplicates()
  mutate(
    date = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  )
  count(month) %>% 
  as_tsibble(index = day)

duplicates(flight1, index = day)
rlang::last_error()
rlang::last_trace()
nycflights13::flights$time_hour

nycflights13::flights %>% 
  gather(year, month, day) %>% 
  group_by(year, month, day) 
  as_tsibble(index = day, key = n)

Flights1 <- nycflights13::flights %>% 
  summarise(Flights = sum(day)) %>% 
  as_tsibble(key = Flights, index = date) %>% 
  mutate(date <- as.numeric(date))

Flights2 <- Flights1[,4:5]
Flights2 %>% 
  autoplot(Flights)


Flights1 <- Flights1 %>% 
  arrange(date)

Flights1




Flights2 <- Flights1[,4:5]
Flights2 %>% 
  mutate(day = year) %>% 
  as_tsibble(index = day)

?yearmonth

Flights2 %>% 
  arrange(date) %>% 
  autoplot(Flights)


dcmp <- Flights2 %>% 
  model(stl = STL(Flights2$Flights))
components(dcmp)

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment 


str(Flights1)

flight1 %>% 
  as_tsibble(key = day, index = date)



%>% 
  as_tsibble(index = day, key = Flights)


flights <- nycflights13::flights %>%
  mutate(
    date = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  )
flights
flight2 <- flights %>% 
  count(date) %>% 
  as_tsibble(key = n, index = date) %>% 
  arrange(date) %>% 
  autoplot(n)
n <- flight2$n
n
date <- seq.Date(from = as.Date("2013/01/01"), to = as.Date("2013/12/31"), by = "day")
date

test1 <- as_tsibble(x =n, key = n, index = date)
test1 %>% arrange(date) %>% 
  autoplot(n)

##########

flight1 <- nycflights13::flights %>% 
  mutate(
    day = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  ) %>%
  count(day) %>% 
  mutate(flights = n) %>% 
  select(day, flights) %>% 
  print(n = 200)

flight1

flight1 <- flight1 %>% 
  as_tsibble(index = day) %>% 
  autoplot(flights) %>% 

dcmp <- flight1 %>% 
  model(stl = STL(flights))
components(dcmp)

components(dcmp) %>% autoplot(flights)

components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(n, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "red") +
  labs(y = "Flights per day", title = "Seasonally adjusted number of flights per day")

flight1 %>% 
  mutate(
    `5-MA` = slider::slide_index_dbl(flights, .f = mean)
      )

flight1$day <- as.numeric(flight1$day)
flight1$flights <- as.double(flight1$flights)
flight1

flight1 %>%
  as_tsibble(index = day) %>% 
  features(flights, list(mean = mean)) %>% 
  arrange(mean)

flight1 %>% 
  as_tsibble(index = day) %>% 
  features(flights, quantile)

flight1 %>% 
  as_tsibble(index = day) %>% 
  features(flights, feat_stl)

######### modelings, take 1 ##############3

flight1$day <- as.Date(flight1$day)
?as.Date

flight1


flight1 <- flight1 %>% 
  as_tsibble(index = day)
TSLM(flight1$flights~trend())
fit <- flight1 %>% model(trend_model = TSLM(flights~trend()))

fit %>% forecast(h = "1 month")

flight1
library(tidyverse)
library(fpp3)

fit1 <- flight1 %>% 
  as_tsibble(index = day) %>% 
  model(tslm = TSLM(flights))
report(fit1)

augment(fit1) %>% 
  ggplot(aes(x = day, y = flights)) +
  geom_point() +
  geom_abline(intercept = 922.674, slope = 20, color = "red")

flight1 <- nycflights13::flights %>% 
  mutate(
    day = date_build(year = year, month = month,day = day) %>%  
    .keep = "unused", 
    .before = 1
  ) %>%
  count(day) %>% 
  mutate(flights = n) %>% 
  select(day, flights)
