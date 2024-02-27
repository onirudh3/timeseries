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
library(MASS)
library(tseries)

# Forbid scientific notation
# options(scipen = 999)


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

# ADF test
adf.test(cpi_df$value) # Non-stationary
adf.test(cpi_df$diff[!is.na(cpi_df$diff)]) # Stationary

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
ar(cpi_df$value)
ar(cpi_df$diff, na.action = na.pass) # For FD

# Forecast
frcst <- forecast(object = ar(cpi_df$value), h = 50) # fifty period forecast
frcst # see first three values
plot(frcst)

frcst <- forecast(object = ar(cpi_df$diff, na.action = na.pass), h = 50)
frcst
plot(frcst)


## Inflation, consumer prices for the US ----
inflation_df <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("1960-07-01"),
  observation_end = as.Date("2022-01-01")
)

# First difference
inflation_df$diff <- c(NA, diff(inflation_df$value, 1))

# ADF test
adf.test(inflation_df$value) # Non-stationary
adf.test(inflation_df$diff[!is.na(inflation_df$diff)]) # Stationary

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

# Fit AR model
ar(inflation_df$value)
ar(inflation_df$diff, na.action = na.pass) # For FD

# Forecast
frcst <- forecast(object = ar(inflation_df$value), h = 50)
frcst
plot(frcst)

frcst <- forecast(object = ar(inflation_df$diff, na.action = na.pass), h = 50)
frcst
plot(frcst)


## Gross Domestic Product ----
gdp_df <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2023-10-01")
)

# First difference
gdp_df$diff <- c(NA, diff(gdp_df$value, 1))

# ADF test
adf.test(gdp_df$value) # Non-stationary
adf.test(gdp_df$diff[!is.na(gdp_df$diff)]) # Stationary

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

# Fit AR model
ar(gdp_df$value)
ar(gdp_df$diff, na.action = na.pass) # For FD

# Forecast
frcst <- forecast(object = ar(gdp_df$value), h = 3)
frcst
plot(frcst)

frcst <- forecast(object = ar(gdp_df$diff, na.action = na.pass), h = 3)
frcst
plot(frcst)


## Federal Funds Effective Rate ----
fedfunds_df <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1954-07-01"),
  observation_end = as.Date("2024-01-01")
)

# First difference
fedfunds_df$diff <- c(NA, diff(fedfunds_df$value, 1))

# ADF test
adf.test(fedfunds_df$value) # Non-stationary
adf.test(fedfunds_df$diff[!is.na(fedfunds_df$diff)]) # Stationary

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

# Fit AR model
ar(fedfunds_df$value)
ar(fedfunds_df$diff, na.action = na.pass) # For FD

# Forecast
frcst <- forecast(object = ar(fedfunds_df$value), h = 3)
frcst
plot(frcst)

frcst <- forecast(object = ar(fedfunds_df$diff, na.action = na.pass), h = 3)
frcst
plot(frcst)


## Total electricity consumption USA: monthly data ----
electricity_df <- read_excel("Retail_sales_of_electricity.xlsx") %>% 
  gather(date, value) %>%
  mutate(date = excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"))

# First difference
electricity_df$diff <- c(NA, diff(electricity_df$value, 1))

# ADF test
adf.test(electricity_df$value) # Non-stationary
adf.test(electricity_df$diff[!is.na(electricity_df$diff)]) # Stationary

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

# Fit AR model
ar(electricity_df$value)
ar(electricity_df$diff, na.action = na.pass) # For FD

# Forecast
frcst <- forecast(object = ar(electricity_df$value), h = 3)
frcst
plot(frcst)

frcst <- forecast(object = ar(electricity_df$diff, na.action = na.pass), h = 3)
frcst
plot(frcst)


# Problem 2 ---------------------------------------------------------------

stock_df <- read.csv("apple_stock_prices_data.csv")

# Returns and z
stock_df <- stock_df %>% 
  mutate(return = diff(Adj.Close) / lag(Adj.Close),
         log_return = c(NA, diff(log(Adj.Close), lag = 1)),
         z = case_when(log_return > 0 ~ 1, T ~ 0))

# Plot
stock_df %>% 
  ggplot() +
  geom_line(aes(Date, log_return, group = 1))

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

# Fit AR model
ar(stock_df$log_return, na.action = na.pass)
ar(stock_df$z)

# Diagnostics
checkresiduals(ar(stock_df$log_return, na.action = na.pass))


# Problem 3 ---------------------------------------------------------------

## 1. ----

model_beta <- 0

set.seed(seed = 1232020)

sim <- function(beta, T, rho, sigma_u, sigma_v, sigma_uv, S) {
  
  Sigma <- matrix(c(sigma_u, sigma_uv, sigma_uv, sigma_v), 2, 2)
  
  uv <- data.frame(mvrnorm(n = S * T,
                           mu = c(0, 0),
                           Sigma = Sigma))
  
  for (s in 1:S){
    
    # Based on his email maybe we shouldn't use x0 = 0?
    # Confirmed changing x0 to 1 or 10 didn't substantively impact the estimated beta
    x_t <- 0 + uv[(s - 1) * T + 1, 2]
    y_t <- 0 + uv[(s - 1) * T + 1, 1]
    
    for(i in 2:T){
      x_t[i] <- rho * x_t[i - 1] + uv[(s - 1) * T + i, 2]
      y_t[i] <- beta * x_t[i - 1] + uv[(s - 1) * T + i, 1]
    }
    
    data <- data.frame(y_t, x_t)
    
    model <- lm(y_t ~ lag(x_t), data = data)
    model_beta[s] <- model$coefficients[2]
    
    rm(x_t, y_t, data)
    
  }
  
  return(model_beta)
  
}

betas_og <- sim(beta = 0.21, T = 840, rho = 0.972, sigma_u = 30.05 / 10 ^ 4, 
                sigma_v = 0.108 / 10 ^ 4, sigma_uv = -1.621 / 10 ^ 4, S = 1000)
summary(betas_og)
sd(betas_og)

betas_rho5 <- sim(beta = 0.21, T = 840, rho = 0.5, sigma_u = 30.05 / 10 ^ 4, 
                  sigma_v = 0.108 / 10 ^ 4, sigma_uv = -1.621 / 10 ^ 4, S = 1000)
summary(betas_rho5)
sd(betas_rho5)

betas_rho0 <- sim(beta = 0.21, T = 840, rho = 0, sigma_u = 30.05 / 10 ^ 4, 
                  sigma_v = 0.108 / 10 ^ 4, sigma_uv = -1.621 / 10 ^ 4, S = 1000)
summary(betas_rho0)
sd(betas_rho0)

betas_sigma0 <- sim(beta = 0.21, T = 840, rho = 0.972, sigma_u = 30.05 / 10 ^ 4, 
                    sigma_v = 0.108 / 10 ^ 4, sigma_uv = 0, S = 1000)
summary(betas_sigma0)
sd(betas_sigma0)

betas_T1680 <- sim(beta = 0.21, T = 1680, rho = 0, sigma_u = 30.05 / 10 ^ 4, 
                   sigma_v = 0.108 / 10 ^ 4, sigma_uv = -1.621 / 10 ^ 4, S = 1000)
summary(betas_T1680)
sd(betas_T1680)


## 3. ---

df <- read_excel("ie_data.xls")




