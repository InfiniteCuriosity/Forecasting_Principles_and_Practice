library(tidyverse)
library(fpp3)
flights <- nycflights13::flights
num_flights <- flights %>% 
  group_by(year, month, day) %>% 
  count() %>% 
  arrange(month, day)

num_flights <- num_flights %>% 
  mutate(year_month_day = make_date(year = year, month = month, day = day)) %>% 
  ungroup()

num_flights <- tsibble(flights = num_flights$n, date = num_flights$year_month_day)
num_flights %>% 
  autoplot(flights)

dcmp <- num_flights %>% 
  model(stl = STL(flights))
components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(flights, colour = "gray") +
  geom_line(aes(y = trend, colour = "#D55E00")) +
  labs(y = "Number of flights per day",
       title = "Number of flights per day")

components(dcmp) %>% autoplot()

##### -- 5-day moving average smoothing --- #####
num_flights_moving_average_5 <- num_flights %>% 
  mutate(
    `5-MA` = slider::slide_dbl(flights, mean,
                                .before = 2, .after = 2, .complete = TRUE)
  )

num_flights_moving_average_5 %>% 
  autoplot(flights) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = '5-day moving average', title = "5-day moving average")
              
##### -- 9-day moving average smoothing --- #####
num_flights_moving_average_9 <- num_flights %>% 
  mutate(
    `9-MA` = slider::slide_dbl(flights, mean,
                               .before = 7, .after = 7, .complete = TRUE)
  )

num_flights_moving_average_9 %>% 
  autoplot(flights) +
  geom_line(aes(y = `9-MA`), colour = "#D55E00") +
  labs(y = '9-day moving average', title = "9-day moving average")

##### --- Moving averages of moving averages --- #####

num_flights_moving_average <- num_flights %>% 
  mutate(
    `4-MA` = slider::slide_dbl(flights, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )
num_flights_moving_average

num_flights_moving_average_3 <- num_flights %>% 
  mutate(
    `12-MA` = slider::slide_dbl(flights, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
num_flights_moving_average_3 %>% 
  autoplot(flights, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00")

##### --- Classical decomposition --- #####

num_flights %>% 
  model(
    classical_decomposition(flights, type = "additive")
  ) %>% 
  components %>% 
  autoplot() +
  labs(title = "Classical additive decomposition of number of flights out of New York City")

num_flights %>% 
  model(
    classical_decomposition(flights, type = "multiplicative")
  ) %>% 
  components %>% 
  autoplot() +
  labs(title = "Classical multiplicative decomposition of number of flights out of New York City")

##### --- STL Decomposition --- #####

num_flights %>% 
  model(
    STL(flights~ trend(window = 13) + season(window = "periodic"),
        robust = TRUE)) %>% 
      components() %>% 
      autoplot()

##### --- Time Series Features --- #####

num_flights %>% 
  features(flights, list(mean = mean)) %>% 
  arrange(mean)

num_flights %>% 
  features(flights, quantile)

num_flights %>% 
  features(flights, feat_acf)

num_flights %>% 
  features(flights, feat_stl)

num_flights %>% 
  features(flights, feat_stl) %>% 
  ggplot(aes(x = trend_strength, y = seasonal_strength_week)) +
           geom_bar()

##### -- working with models --- #####

fit <- num_flights %>% 
  model(trend_model = TSLM(flights ~ trend()))
report(fit)

# Produce forecasts
fit %>% forecast(h = "30 days")

#Apply simple forecasting methods
flights_fit <- num_flights %>% 
  model(
    Mean = MEAN(flights),
    `Naïve` = NAIVE(flights),
    Seasonal_naive = SNAIVE(flights),
    Drift = NAIVE(flights~drift())
      )
report(flights_fit)
flights_forecast <- flights_fit %>% forecast(h = 5)
flights_forecast %>%
  autoplot(num_flights, colour = "black") +
  facet_grid(~.model)

# A great table that shows accuracy statistics of the four models
accuracy(flights_fit)

#plot all four prediction methods on the same plot
flights_forecast %>%
  autoplot(num_flights, colour = "black")

# Plot all four prediction methods on separate graphs
flights_forecast %>%
  autoplot(num_flights, colour = "black") +
  facet_grid(~.model)

# run the report with Seasonal Naive (since it has the highest accuracy)
flights_fit1 <- num_flights %>% 
  model(
    Seasonal_naive = SNAIVE(flights)
    )
report(flights_fit1)

flights_fit1 %>% forecast(h = 30) %>% 
  print(n = 30)

flights_fit1 %>% 
  gg_tsresiduals()


# working with residuals
aug <- num_flights %>% 
  model(SNAIVE(flights)) %>% 
  augment()
autoplot(aug, .innov) +
  labs(y = "Number of flights",
       title = "residuals from the SNAIVE method")

mean(aug$.resid,na.rm = TRUE)

# Residual diagnostics
num_flights %>% 
  model(SNAIVE(flights)) %>% 
  gg_tsresiduals()

#Portmanteau tests for autocorrelation
aug %>% features(.innov, box_pierce, lag = 7, dof = 0)
aug %>% features(.innov, ljung_box, lag = 7, dof = 0)

#80% and 95% prediction intervals
num_flights %>% 
  model(SNAIVE(flights)) %>% 
  forecast(h = 30) %>% 
  hilo()

# plot of prediction intervals
num_flights %>% 
  model(SNAIVE(flights)) %>% 
  forecast(h = 30)

#### --- using bootstrapped residuals --- #####

fit <- 
  num_flights %>% 
  model(SNAIVE(flights))
sim <- fit %>% generate(h = 30, times = 5000, bootstrap = TRUE)
sim %>% 
  print(n = 100)

sim[nrow(sim)-50:nrow(sim),] %>% 
  print(n = 50)

num_flights %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y =flights)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  facet_grid(~.rep)

# bootstrap is built into the forecast tool - yay!! :)
forecast_01 <- fit %>% forecast(h = 30, bootstrap = TRUE, times = 10000)
forecast_01

# forecasting using transformations: bias-adjustment
num_flights %>% 
  model(RW(log(flights) ~ drift())) %>% 
  forecast(h = 5)

# Forecasting with decomposition
dcmp <- num_flights %>% 
  model(STL(flights ~ trend(window = 7), robust = TRUE)) %>% 
  components() %>% 
  select(-.model)
dcmp

# plot of forecasting with decomposition
dcmp1 <- dcmp %>% 
  model(SNAIVE(season_adjust)) %>% 
  forecast()
dcmp %>% 
  ggplot(aes(x = date, y = trend), colour = "red") +
  geom_line(aes(x = date, y = trend), colour = "green") +
  geom_line(aes(x = date, y = flights), color = "lightblue") +
  geom_line(aes(x = date, y = season_adjust), color = "orange")


# Time series cross-validation
num_flights_tr <- num_flights %>% 
  stretch_tsibble(.init = 3, .step = 1) %>% 
  relocate(flights, .id) %>% 
  autoplot(flights)
num_flights_tr

# Accuracy of the cross-validated models

num_flights %>% 
  model(SNAIVE(flights ~ drift() + seasonal())) %>% 
          accuracy()

#### --- regression models --- #####

fit_01 <- num_flights %>% 
  mutate(diff1 = difference(flights)) %>% 
  model(TSLM(diff1 ~ trend() + season()), 
        )
report(fit_01)
fit_01 %>% gg_tsresiduals()

fit_02 <- num_flights %>%
  mutate(diff2 = difference(difference(flights))) %>% 
  model(TSLM(diff2 ~ trend() + season()))
report(fit_02)
gg_tsresiduals(fit_02)

report(fit_01)

augment(fit_01) %>% 
  ggplot(aes(x = flights, y = .fitted)) +
  geom_point()

# Selecting predictors
glance(fit_01) %>% 
  select(adj_r_squared, CV, AIC, AICc, BIC)

fit_flights <- num_flights %>% 
  model(
    `Seasonal Naïve` = SNAIVE(flights),
    exponential = TSLM(log(flights) ~ trend() + season()),
    piecewise = TSLM(flights~trend() + season()))
report(fit_flights)
fit_flights

fit <- num_flights %>% 
  model(
    AAN = ETS(flights ~ error("M") + trend("Ad") + season("M"))
  )
forecast <- fit %>% forecast(h = 10)
forecast

fit %>% gg_tsresiduals()

fit <- num_flights %>% 
  model(ARIMA(flights))
report(fit)

fit_01 <- num_flights %>% 
  mutate(diff2 = difference(difference(flights))) %>% 
  model(TSLM(diff2 ~ trend() + season()), 
  )
report(fit_01)
gg_tsresiduals(fit_01)

num_flights_01 <- num_flights %>% 
  mutate(row_num = row_number())

train <- num_flights[1:300,]
test <- num_flights[301:nrow(num_flights),]
test
train

fit_arima <- train %>% model(ARIMA(flights))
report(fit_arima)
fit_arima %>% gg_tsresiduals()

fit_arima2 <- test %>% model(ARIMA(flights))
gg_tsresiduals(fit_arima2)


augment(fit_arima) %>% 
  features(.innov, ljung_box)

fit_ets <- train %>% model(ETS(flights))
report(fit_ets)
gg_tsresiduals(fit_ets)

bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 7) %>% accuracy(test),
  fit_ets %>% forecast(h = 7) %>%  accuracy(test))
fit_arima %>% forecast(h = 7) %>% accuracy(flights)

num_flights %>% 
  model(ARIMA(flights)) %>% 
  forecast(h = "30 days") %>% 
  print(n = 30)

num_flights
