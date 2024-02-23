# Times Series Homework
# Alec Wall and Anirudh Ravishankar
# February 2024


# Libraries ---------------------------------------------------------------

library(fredr)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(forecast)
library(moments)

# Forbid scientific notation
options(scipen = 999)

# Problem 1 ---------------------------------------------------------------

# # Search FRED for series
# fredr_series_search_text("FPCPITOTLZGUSA")
# fredr_series("FEDFUNDS")$title

# FRED Data
fredr_set_key("eb56ad2180a43dce3526dc9cd086b33b")


## Consumer Price Index for All Urban Consumers: All Items in US City Average ----
cpi_df <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2024-01-01")
)

# First difference
cpi_df$diff <- c(NA, diff(cpi_df$value, 1))

# Descriptive statistics
summary(cpi_df$date)
summary(cpi_df$value)
sd(cpi_df$value)
summary(cpi_df$diff)
sd(cpi_df$diff, na.rm = T)

# Plot
plot(cpi_df$date, cpi_df$value, type = "l", xlab = "Date", ylab = "CPI")
plot(cpi_df$date, cpi_df$diff, type = "l", xlab = "Date", ylab = "CPI FD")

# ACF
acf(cpi_df$value, main = "")
acf(cpi_df$diff, main = "", na.action = na.pass)

# PACF
pacf(cpi_df$value, main = "")
pacf(cpi_df$diff, main = "", na.action = na.pass)

# Fit AR model
ar_fit <- ar(cpi_df$value)
theforecast <- forecast(object = ar_fit, h = 20) # twenty period forecast
plot(theforecast)

## Inflation, consumer prices for the US ----
inflation_df <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("1960-07-01"),
  observation_end = as.Date("2022-01-01")
)

# First difference
inflation_df$diff <- c(NA, diff(inflation_df$value, 1))

# Descriptive statistics
summary(inflation_df$date)
summary(inflation_df$value)
sd(inflation_df$value)
summary(inflation_df$diff)
sd(inflation_df$diff, na.rm = T)

# Plot
plot(inflation_df$date, inflation_df$value, type = "l", xlab = "Date", ylab = "Inflation")
plot(inflation_df$date, inflation_df$diff, type = "l", xlab = "Date", ylab = "Inflation FD")

# ACF
acf(inflation_df$value, main = "")
acf(inflation_df$diff, main = "", na.action = na.pass)

# PACF
pacf(inflation_df$value, main = "")
pacf(inflation_df$diff, main = "", na.action = na.pass)


## Gross Domestic Product ----
gdp_df <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2023-10-01")
)

# First difference
gdp_df$diff <- c(NA, diff(gdp_df$value, 1))

# Descriptive statistics
summary(gdp_df$date)
summary(gdp_df$value)
sd(gdp_df$value)
summary(gdp_df$diff)
sd(gdp_df$diff, na.rm = T)

# Plot
plot(gdp_df$date, gdp_df$value, type = "l", xlab = "Date", ylab = "GDP")
plot(gdp_df$date, gdp_df$diff, type = "l", xlab = "Date", ylab = "GDP FD")

# ACF
acf(gdp_df$value, main = "")
acf(gdp_df$diff, main = "", na.action = na.pass)

# PACF
pacf(gdp_df$value, main = "")
pacf(gdp_df$diff, main = "", na.action = na.pass)


## Federal Funds Effective Rate ----
fedfunds_df <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1954-07-01"),
  observation_end = as.Date("2024-01-01")
)

# First difference
fedfunds_df$diff <- c(NA, diff(fedfunds_df$value, 1))

# Descriptive statistics
summary(fedfunds_df$date)
summary(fedfunds_df$value)
sd(fedfunds_df$value)
summary(fedfunds_df$diff)
sd(fedfunds_df$diff, na.rm = T)

# Plot
plot(fedfunds_df$date, fedfunds_df$value, type = "l", xlab = "Date", ylab = "Fed Funds Rate")
plot(fedfunds_df$date, fedfunds_df$diff, type = "l", xlab = "Date", ylab = "Fed Funds Rate FD")

# ACF
acf(fedfunds_df$value, main = "")
acf(fedfunds_df$diff, main = "", na.action = na.pass)

# PACF
pacf(fedfunds_df$value, main = "")
pacf(fedfunds_df$diff, main = "", na.action = na.pass)


## Total electricity consumption USA: monthly data ----
electricity_df <- read.csv("Electricity consumption - United States.csv", skip = 2) %>% 
  rename("value" = "Electricity.consumption",
         "date" = "X")

# First difference
electricity_df$diff <- c(NA, diff(electricity_df$value, 1))

# Descriptive statistics
summary(electricity_df$date)
summary(electricity_df$value)
sd(electricity_df$value)
summary(electricity_df$diff)
sd(electricity_df$diff, na.rm = T)

# Plot
plot(electricity_df$date, electricity_df$value, type = "l", xlab = "Date", ylab = "Electricity Consumption USA")
plot(electricity_df$date, electricity_df$diff, type = "l", xlab = "Date", ylab = "Electricity Consumption USA FD")

# ACF
acf(electricity_df$value, main = "")
acf(electricity_df$diff, main = "", na.action = na.pass)

# PACF
pacf(electricity_df$value, main = "")
pacf(electricity_df$diff, main = "", na.action = na.pass)


# Problem 2 ---------------------------------------------------------------

stock_df <- read.csv("apple_stock_prices_data.csv")

# Returns and z
stock_df <- stock_df %>% 
  mutate(return = diff(Adj.Close) / lag(Adj.Close),
         log_return = log(return + 1),
         z = case_when(log_return > 0 ~ 1, T ~ 0))

# Descriptive statistics
summary(stock_df$log_return) # Mean
sd(stock_df$log_return, na.rm = T) # Standard deviation
skewness(stock_df$log_return, na.rm = T) # Skewness
kurtosis(stock_df$log_return, na.rm = T) # Kurtosis

summary(stock_df$z) # Mean
sd(stock_df$z, na.rm = T) # Standard deviation
skewness(stock_df$z, na.rm = T) # Skewness
kurtosis(stock_df$z, na.rm = T) # Kurtosis

# ACF
acf(stock_df$log_return, main = "", na.action = na.pass)
acf(stock_df$z, main = "", na.action = na.pass)

# PACF
pacf(stock_df$log_return, main = "", na.action = na.pass)
pacf(stock_df$z, main = "", na.action = na.pass)

# Problem 3 ---------------------------------------------------------------

## Question 3 ---

df <- read_excel("ie_data.xls") # Doesn't work
