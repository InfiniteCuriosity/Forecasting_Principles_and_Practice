library(tidyverse)
library(fpp3)

####### Quits ##########

quits <- read.csv('/Users/russellconte/quits.csv',header = TRUE, sep = ',')
quits <- quits %>% pivot_longer(cols = 2:13) %>% 
  mutate(index = seq(as.Date("2011/1/1"), by = "month", length.out = 132))
quits <- quits[1:128, 3:4]
quits$index = yearmonth(quits$index)
quits <- as_tsibble(x = quits, index = index)
quits <- rename(quits, num_quits = value)
num_quits <- quits$num_quits
index <- quits$index

autoplot(object = quits) +
  labs(y = "Number of quits", title = "Number of quits per month")

ACF(quits, lag_max = 120) %>% 
  autoplot() +
  labs(title = "Autocorrelation of quits")


# the way the US Census Bureau does their reporting (X11)
x11_dcmp <- quits %>%
  model(x11 = X_13ARIMA_SEATS(num_quits ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of quits using X-11.")

x11_dcmp %>% 
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_quits, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Quit rate",
       title = "Quit rate")

###


TSLM(quits ~ trend())
fit <- quits %>%
  model(trend_model = TSLM(num_quits ~ trend()))
report(fit)

augment(fit) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_quits, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

fit %>% gg_tsresiduals()

glance(fit) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)


fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(quits)

dcmp <- quits %>% 
  model(stl = STL(num_quits))
components(dcmp) %>% autoplot()

gg_season(data = quits, y = num_quits)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(num_quits, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

### --- Forecasts --- ###

quits

# Set training data
train <- quits %>%
  filter_index("2011 Jan" ~ "2021 Sep")
# Fit the models
quit_fit <- train %>%
  model(
    Mean = MEAN(num_quits),
    `Naïve` = NAIVE(num_quits),
    `Seasonal naïve` = SNAIVE(num_quits)
  )
# Generate forecasts for 14 quarters
quits_forecast <- quit_fit %>% forecast(h = 14)
# Plot forecasts against actual values
quits_forecast %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(quits, "2021 Oct" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Rate of quits",
    title = "Forecasts for rate of quits"
  ) +
  guides(colour = guide_legend(title = "Forecast"))



#################### Openings #########################

openings <- read.csv('/Users/russellconte/Job_openings.csv',header = TRUE, sep = ',')
openings <- openings %>% pivot_longer(cols = 2:13) %>% 
  mutate(index = seq(as.Date("2011/1/1"), by = "month", length.out = 132))
openings <- openings[1:128, 3:4]
openings$index = yearmonth(openings$index)
openings <- as_tsibble(x = openings, index = index)
openings <- rename(openings, num_openings = value)
num_openings <- openings$num_openings

autoplot(object = openings) +
  labs(y = "Number of openings", title = "Number of openings per month")

ACF(openings, lag_max = 120) %>% 
  autoplot() +
  labs(title = "Autocorrelation of openings")

# the way the US Census Bureau does their reporting (X11)
x11_dcmp <- openings %>%
  model(x11 = X_13ARIMA_SEATS(num_openings ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of openings using X-11.")

x11_dcmp %>% 
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_openings, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Openings rate",
       title = "Openings rate")

TSLM(openings ~ trend())
fit <- openings %>%
  model(trend_model = TSLM(num_openings ~ trend()))
report(fit)

augment(fit) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_openings, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in openings"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

fit %>% gg_tsresiduals()

glance(fit) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(openings)

dcmp <- openings %>% 
  model(stl = STL(num_openings))
components(dcmp) %>% autoplot()

gg_season(data = openings, y = num_openings)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(num_openings, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

# Set training data
train <- openings %>%
  filter_index("2011 Jan" ~ "2021 Sep")
# Fit the models
openings_fit <- train %>%
  model(
    Mean = MEAN(num_openings),
    `Naïve` = NAIVE(num_openings),
    `Seasonal naïve` = SNAIVE(num_openings)
  )
# Generate forecasts for 14 quarters
openings_forecast <- openings_fit %>% forecast(h = 14)
# Plot forecasts against actual values
openings_forecast %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(quits, "2021 Oct" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Rate of openings",
    title = "Forecasts for rate of openings"
  ) +
  guides(colour = guide_legend(title = "Forecast"))



############ Seasonally adjusted hires ############

hires <- read.csv('/Users/russellconte/Hires.csv',header = TRUE, sep = ',')
hires <- hires %>% pivot_longer(cols = 2:13) %>% 
  mutate(index = seq(as.Date("2011/1/1"), by = "month", length.out = 132))

hires <- hires[1:128, 3:4]
hires$index = yearmonth(hires$index)
hires <- as_tsibble(x = hires, index = index)
hires$num_hired = hires$value
hires <- hires[,2:3]
num_hires <- hires$num_hired

autoplot(hires) +
  labs(y = "Number of hires", title = "Number of hires per month")

x11_dcmp <- hires %>%
  model(x11 = X_13ARIMA_SEATS(num_hires ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of hires using X-11.")

x11_dcmp %>% 
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_hires, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Hire rate",
       title = "Hire rate")

ACF(hires, lag_max = 120) %>% 
  autoplot() +
  labs(title = "Autocorrelation of hires")

TSLM(hires ~ trend())
fit <- hires %>%
  model(trend_model = TSLM(num_hired ~ trend()))
report(fit)

augment(fit) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_hires, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = "Number of hires ('000)",
       title = "Number of hires",
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

fit %>% gg_tsresiduals()

glance(fit) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(hires)

dcmp <- hires %>% 
  model(stl = STL(num_hired))
components(dcmp) %>% autoplot()

gg_season(data = hires, y = num_hires)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(num_hired, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

train <- hires %>%
  filter_index("2011 Jan" ~ "2021 Sep")
# Fit the models
hires_fit <- train %>%
  model(
    Mean = MEAN(num_hires),
    `Naïve` = NAIVE(num_hires),
    `Seasonal naïve` = SNAIVE(num_hires)
  )
# Generate forecasts for 14 quarters
hires_forecast <- hires_fit %>% forecast(h = 14)
# Plot forecasts against actual values
hires_forecast %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(hires, "2021 Oct" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Rate of hires",
    title = "Forecasts for rate of hires"
  ) +
  guides(colour = guide_legend(title = "Forecast"))



############ Layoffs and Discharges ###################

layoffs_and_discharges <- read.csv('/Users/russellconte/Layoffs_and_discharges.csv',header = TRUE, sep = ',')
layoffs_and_discharges <- layoffs_and_discharges %>% pivot_longer(cols = 2:13) %>% 
  mutate(index = seq(as.Date("2011/1/1"), by = "month", length.out = 132))

layoffs_and_discharges <- layoffs_and_discharges[1:128, 3:4]
layoffs_and_discharges$index = yearmonth(layoffs_and_discharges$index)
layoffs_and_discharges <- as_tsibble(x = layoffs_and_discharges, index = index)
layoffs_and_discharges <- rename(layoffs_and_discharges, num_layoffs = value)
autoplot(layoffs_and_discharges) +
  labs(y = "Number of layoffs and discharges", title = "Number of layoffs and discharges per month")

x11_dcmp <- layoffs_and_discharges %>%
  model(x11 = X_13ARIMA_SEATS(num_layoffs ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of layoffs and discharges using X-11.")

x11_dcmp %>% 
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_layoffs, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Layoffss rate",
       title = "Layoffs rate")

ACF(layoffs_and_discharges, lag_max = 120) %>% 
  autoplot() +
  labs(title = "Autocorrelation of layoffs and discharges")

num_layoffs <- layoffs_and_discharges$num_layoffs

TSLM(hires ~ trend())
fit <- layoffs_and_discharges %>%
  model(trend_model = TSLM(num_layoffs ~ trend()))
report(fit)

augment(fit) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_layoffs, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = "Number of layoffs ('000)",
       title = "Number of layoffs",
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

fit %>% gg_tsresiduals()

glance(fit) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(layoffs_and_discharges)

dcmp <- layoffs_and_discharges %>% 
  model(stl = STL(num_layoffs))
components(dcmp) %>% autoplot()

gg_season(data = layoffs_and_discharges, y = num_layoffs)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(num_layoffs, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

train <- layoffs_and_discharges %>%
  filter_index("2011 Jan" ~ "2021 Sep")
# Fit the models
layoffs_fit <- train %>%
  model(
    Mean = MEAN(num_layoffs),
    `Naïve` = NAIVE(num_layoffs),
    `Seasonal naïve` = SNAIVE(num_layoffs)
  )
# Generate forecasts for 14 quarters
layoffs_forecast <- layoffs_fit %>% forecast(h = 14)
# Plot forecasts against actual values
layoffs_forecast %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(layoffs_and_discharges, "2021 Oct" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Rate of layoffs and discharges",
    title = "Forecasts for rate of layoffs and discharges"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

############ Seasonally Adjusted Separations###################

separations <- read.csv('/Users/russellconte/Separations.csv',header = TRUE, sep = ',')
separations <- separations %>% pivot_longer(cols = 2:13) %>% 
  mutate(index = seq(as.Date("2011/1/1"), by = "month", length.out = 132))
separations <- separations[1:128, 3:4]
separations$index = yearmonth(separations$index)
separations <- as_tsibble(x = separations, index = index)
separations <- rename(separations, num_separations = value)
num_separations <- separations$num_separations
autoplot(separations) +
  labs(y = "Number of seprations", title = "Number of separations per month")

x11_dcmp <- separations %>%
  model(x11 = X_13ARIMA_SEATS(num_separations ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of separations using X-11.")

x11_dcmp %>% 
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_separations, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "separations rate",
       title = "Separations rate")

ACF(separations, lag_max = 120) %>% 
  autoplot() +
  labs(title = "Autocorrelation of separations")

TSLM(separations ~ trend())
fit <- separations %>%
  model(trend_model = TSLM(num_separations ~ trend()))
report(fit)

augment(fit) %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = num_separations, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = "Number of separations ('000)",
       title = "Number of separations",
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

fit %>% gg_tsresiduals()

glance(fit) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(separations)

dcmp <- separations %>% 
  model(stl = STL(num_separations))
components(dcmp) %>% autoplot()

gg_season(data = separations, y = num_separations)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(num_separations, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2")

train <- separations %>%
  filter_index("2011 Jan" ~ "2021 Sep")
# Fit the models
separations_fit <- train %>%
  model(
    Mean = MEAN(num_separations),
    `Naïve` = NAIVE(num_separations),
    `Seasonal naïve` = SNAIVE(num_separations)
  )
# Generate forecasts for 14 quarters
separations_forecast <- separations_fit %>% forecast(h = 14)
# Plot forecasts against actual values
separations_forecast %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(separations, "2021 Oct" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Rate of separations",
    title = "Forecasts for rate of separations"
  ) +
  guides(colour = guide_legend(title = "Forecast"))




############ Print the results ###################

labor_market_data <- tsibble(index = index, num_openings, num_quits, num_hires, num_layoffs, num_separations)

index1 <- as.character(index)
labor_market_data <- data.frame(index, num_hires, num_layoffs, num_openings, num_quits, num_separations)
labor_market_data <- as_tsibble(labor_market_data)
labor_market_data


labor_market_data %>%
  pivot_longer(-index) %>% 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")


fit_labor_market_data <-  labor_market_data %>% 
  model(tslm = TSLM(num_hires ~ num_openings + num_quits + num_layoffs + num_separations))
report(fit_labor_market_data)

fit_labor_market_data %>% gg_tsresiduals()

# Correlations
labor_market_data %>% 
  GGally::ggpairs(columns = 2:6) +
  labs(title = "Correlation of columns in Labor Market Data")

##### ----- scenario based forecasting ----- #####

fit <- labor_market_data %>%
  model(
    lm = TSLM(num_hires ~ num_layoffs + num_openings + num_quits + num_separations)
  )

future_scenarios <- scenarios(
  Increase = new_data(labor_market_data, 12) %>%
    mutate(num_openings = -1, num_layoffs = 1, num_openings = 1, num_quits = 1, num_separations = 1),
  Decrease = new_data(labor_market_data, 12) %>%
    mutate(num_openings = -10, num_layoffs = 10, num_openings = 10, num_quits = 10, num_separations = 10),
  names_to = "Scenario")

forecast1 <- forecast(fit, new_data = future_scenarios)

quits %>%
  autoplot(num_quits) +
  autolayer(forecast1)
  labs(title = "US consumption", y = "% change")

