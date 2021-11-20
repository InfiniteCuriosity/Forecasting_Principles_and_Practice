library(tidyverse)
library(fpp3)
flight4 <- nycflights13::flights %>% 
  mutate(
    day = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  ) %>%
  select(day, dep_time, air_time, arr_time)
flight4 %>%
  as_tsibble(index = day) %>% 
  autoplot(day)
  
flight7 %>% 
  as_tsibble(key = c(dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, arr_delay, carrier, flight, tailnum, origin, dest, air_time, distance, hour, time_hour), index = minute) %>%
  autoplot(day)
  
dup2 <- duplicates(data = flight7, key = c(dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, arr_delay, carrier, flight, tailnum, origin, dest, air_time, distance, hour, minute, time_hour), index = minute)
  
  select(day, dep_time, air_time, arr_time) %>% 
  group_by(day) %>% 
  summarise(flights = count(day))
  print(n = 20)

flight1

flight1 <- flight1 %>% 
  as_tsibble(index = day) %>% 
  autoplot(flights)

flight1
  
dcmp <- flight1 %>% 
  model(stl = STL(flights))
components(dcmp)

components(dcmp) %>% autoplot(flights)

components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(n, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "red") +
  labs(y = "Flights per day", title = "Seasonally adjusted number of flights per day")



flight1_ma <- flight1 %>% 
  mutate(
    `5-MA` = slider::slide_index_dbl(flights, .f = mean, .i = day,
                                     .before = 1, .after = 1, .complete = TRUE)
  )

flight1_ma

flight1_ma %>% 
  as_tsibble() %>% 
  autoplot(flights) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  guides(colour = guide_legend(title = "series"))


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

flight1 %>% 
  ggplot(aes(x = day, y = flights)) +
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)

flight1 <- nycflights13::flights %>% 
  mutate(
    day = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  ) %>%
  count(day) %>% 
  mutate(flights = n) %>% 
  select(day, flights)

model1 <- flight1 %>% 
  as_tsibble() %>% 
  model(tslm = TSLM(flights~day)) %>% 
  report

model1 %>% gg_tsresiduals()

model2 <- flight1 %>% 
  as_tsibble(index = day) %>% 
  model(TSLM(flights~trend() + season()))
report(model2)

model2 %>% gg_tsresiduals()

forecast1 <- forecast(model2, h = 30)
forecast1

flight3 <- nycflights13::flights %>% 
  mutate(
    day = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  )
flight3 %>% 
  filter(dest=='ORD') %>% 
  arrange(air_time) %>% 
  select(day, dep_time, arr_time, air_time, carrier) %>% 
  ggplot(aes(x = air_time))+
  facet_grid(~carrier) +
  geom_bar()

fourier_flights <- flight2 %>% 
  model(TSLM(n ~ trend() + fourier(k = 2)))
report(fourier_flights)

glance(model1) %>% 
  select(adj_r_squared, CV, AIC, AICc, BIC)

glance(model2) %>% 
  select(adj_r_squared, CV, AIC, AICc, BIC)







library(tidyverse)
library(fpp3)

rm(fc)

flight2 <- flight2 %>% 
  as_tsibble()
fit_flights <- flight2 %>% 
  model(
    lm = TSLM(n ~ day + trend())
  )

future_scenarios <- scenarios(
  Increase = new_data(.data = flight2, n = 4) %>% 
    mutate(n = 100),
  Decrease = new_data(.data  = flight2, 4) %>% 
    mutate(n = -100),
  names_to = "Scenario")

us_change
fc <- forecast(fit_flights, new_data = future_scenarios)

fc



############
flight2
fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)
fc
flight2
fit <- flight2 %>% 
  model(ETS(n~error("A") + trend("N") + season("N")))
forecast1 <- fit %>% 
  forecast(h = 7)
forecast1
flight2
flight1 %>% 
  features(flights, unitroot_kpss)

flight1 %>% 
  mutate(diff1 = difference(flights)) %>% 
  features(diff1, unitroot_kpss)

flight6 <- flight1 %>% 
  mutate(diff1 = difference(flights))
flight6 %>% gg_tsresiduals()

model3 <- flight2 %>% 
  model(tslm = TSLM(n ~ day + trend() + season()))
report(model3)

flight2 %>% gg_tsresiduals()

aug <- flight1 %>% 
  model(SNAIVE(flights)) %>% 
  augment()
aug

autoplot(aug, .innov)

aug %>% 
  ACF(.innov) %>% 
  autoplot()

us_change %>%
  select(-Consumption, -Income) %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(Quarter, value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")
fit_consMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)
fit_consMR %>% gg_tsresiduals()

dup1 <- nycflights13::flights %>% 
  as_tsibble(index = day, key = c(dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, arr_delay, carrier, 
                                  flight, tailnum, origin, dest, air_time, distance))
flight8 <- nycflights13::flights %>% 
  as.numeric(count(month, day)) %>% 
  drop_na() %>% 
  distinct() %>% 
  arrange(month)

flight8 %>% 
  group_by(day) %>% 
  mutate(flights = n)


flight10 <- nycflights13::flights %>% 
  group_by(month, day) %>% 
  summarise(flights = count(month, day))
sum(flight10$flights)
sum(flight1$flights)

flight11 <- nycflights13::flights %>% 
  group_by(year, month, day) %>% 
  tally()
flights <- flight11$n
x2<-seq(as.Date("2013-01-01"),by="day",length.out=365)
x2
flight12 <- tibble(x2, flights)
flight12 %>% 
  as_tsibble() %>% 
  gg_tsresiduals()

decomposition <- flight12 %>% 
  mutate(diff1 = difference(flights)) %>% 
  as_tsibble(index = x2, key = c(flights, diff1)) %>% 
  arrange(x2) %>% 
  gg_tsresiduals()
decomposition
date
flights
flights.df <- data.frame(x2, flights)
flight13 <- flights.df %>% as_tsibble()
flight13 %>% 
  mutate(diff1 = difference(flight)) %>% 
  model(tslm = TSLM(diff1~x2)) %>% 
  gg_tsresiduals()
flight13 %>% 
  model(ARIMA(flights)) %>% 
  report

library(tidyverse)
library(fpp3)
flight %>% 
  mutate(day1=yearmonth(month))

flight1 %>% 
  mutate(day1 <- ymd(day))
olympic_running  
PBS %>% 
  filter(ATC2 =='A10') %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarise(totalcost = sum(Cost)) %>% 
  as_tsibble() %>% 
  gg_season(totalcost)

flight1 %>% 
  gg_subseries(flights)

nycflights13::flights %>% 
  group_by(year, month, day) %>% 
  summarise(flights = sum(month))

flight6 %>% 
  features(flights, feat_acf)

flight6 %>% 
  features(flights, feat_stl) %>% 
  ggplot(aes(x = trend_strength, y = seasonal_strength_week)) +
  geom_point()



tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()

library(tidyverse)
library(fpp3)


weather <- nycflights13::weather %>% 
  select(temp, year, month, day, hour)
weather <- as.data.frame(weather)

weather <- weather %>% 
  unique() %>% 
  distinct()

weather %>% 
  as_tsibble(key = c(temp, year, month), index = day) %>% 
  model(ARIMA(temp)) %>% 
  gg_tsresiduals()

weather <- data.frame(temp = weather1$temp, time_hour = weather1$time_hour)
weather <- weather %>% 
  filter(time_hour<'2013-02-01 01:00:00')
weather <- weather %>% 
  arrange(time_hour)
weather

weather %>% 
  arrange(time_hour) %>% 
  ggplot(aes(x = time_hour, y = temp)) +
           geom_line()

weather <- weather %>% 
  as_tsibble(index = time_hour, key = temp)

library(tidyverse)
library(fpp3)
time1 <- nycflights13::weather$time_hour
temp1 <- nycflights13::weather$temp
weather <- data.frame(time1, temp1)
weather <-  as_tsibble(x = weather, key = temp1, index = time1)%>% 
  unique()

weather <- weather[1:500,]
head(weather)
weather %>% 
  arrange(time1) %>% 
  ggplot(aes(x = time1, y = temp1)) +
  geom_line() +
  labs(title = "Temperature vs time, January, 2013 in New York City")

weather <- weather %>% 
  as_tsibble(index = hour, key = temp)



weather %>%
  arrange(time_hour) %>% 
  model(tslm = TSLM(temp ~ time_hour)) %>% 
  report()
weather

us_change %>% 
  autoplot(Consumption)

weather

library(clock)
weather <- weather %>% 
  distinct(date, avg_temp)
dates <- seq(as.Date('2013-01-01'), as.Date('2013-12-31'), "days")
class(dates)
weather$avg_temp

date <- as.numeric(dates)

weather <- weather %>% 
  unique() %>% 
  distinct()
weather

weather1 <- build_tsibble(x = weather, index = day, key = c(temp, year, month, hour))
weather1 %>% 
  autoplot(temp)

str(aus_accommodation)
