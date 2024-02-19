
# Libraries ---------------------------------------------------------------

library(fredr)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(ggplot2)


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

# ACF
acf(cpi_df$value, main = "")

# PACF
pacf(cpi_df$value, main = "")

# Plot
plot(cpi_df$date, cpi_df$value, type = "l", xlab = "Date", ylab = "CPI")

# Descriptive statistics
summary(cpi_df$date)
summary(cpi_df$value)
sd(cpi_df$value)


## Inflation, consumer prices for the US ----
inflation_df <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("1960-07-01"),
  observation_end = as.Date("2022-01-01")
)

# ACF
acf(inflation_df$value, main = "")

# PACF
pacf(inflation_df$value, main = "")

# Plot
plot(inflation_df$date, inflation_df$value, type = "l", xlab = "Date", ylab = "Inflation")

# Descriptive statistics
summary(inflation_df$date)
summary(inflation_df$value)
sd(inflation_df$value)


## Gross Domestic Product ----
gdp_df <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2023-10-01")
)

# ACF
acf(gdp_df$value, main = "")

# PACF
pacf(gdp_df$value, main = "")

# Plot
plot(gdp_df$date, gdp_df$value, type = "l", xlab = "Date", ylab = "GDP")

# Descriptive statistics
summary(gdp_df$date)
summary(gdp_df$value)
sd(gdp_df$value)


## Federal Funds Effective Rate ----
fedfunds_df <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1954-07-01"),
  observation_end = as.Date("2024-01-01")
)

# ACF
acf(fedfunds_df$value, main = "")

# PACF
pacf(fedfunds_df$value, main = "")

# Plot
plot(fedfunds_df$date, fedfunds_df$value, type = "l", xlab = "Date", ylab = "Fed Funds Rate")

# Descriptive statistics
summary(fedfunds_df$date)
summary(fedfunds_df$value)
sd(fedfunds_df$value)


## Electricity consumption: EIA monthly data ----


# ACF
acf(electricity_df$value, main = "")

# PACF
pacf(electricity_df$value, main = "")

# Plot
plot(electricity_df$date, electricity_df$value, type = "l", xlab = "Date", ylab = "Electricity Consumption")

# Descriptive statistics
summary(electricity_df$date)
summary(electricity_df$value)
sd(electricity_df$value)


# Problem 2 ---------------------------------------------------------------

stock_df <- read.csv("apple_stock_prices_data.csv")


# Problem 3 ---------------------------------------------------------------

## Question 3 ---

df <- read_excel("ie_data.xls") # Doesn't work
