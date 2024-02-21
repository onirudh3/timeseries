

rm(list=ls())

# Libraries ---------------------------------------------------------------

library(easypackages)
packages("fredr", "dplyr", "tidyr", "janitor", "readxl", "forecast", "stats",
         "labelled", "ggplot2", "zoo")


# Problem 1 ---------------------------------------------------------------

# Get data from FRED
fredr_set_key("eb56ad2180a43dce3526dc9cd086b33b")

## Consumer Price Index for All Urban Consumers: All Items in US City Average ----
cpi_df <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2024-01-01")
)

var_label(cpi_df$value) <- "Consumer Price Index"

## Inflation, consumer prices for the US ----
inflation_df <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("1960-07-01"),
  observation_end = as.Date("2022-01-01")
)

var_label(inflation_df$value) <- "Inflation"

## Gross Domestic Product ----
gdp_df <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2023-10-01")
)

var_label(gdp_df$value) <- "Gross Domestic Product"

## Federal Funds Effective Rate ----
fedfunds_df <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1954-07-01"),
  observation_end = as.Date("2024-01-01")
)

var_label(fedfunds_df$value) <- "Short-term Interest Rate"

firstdiff <- function(df) {
  diff <- df %>% 
    mutate(value = c(NA, diff(value, lag=1)))
}

# Q1: Plots and descriptive stats

all_plots <- function(df) {
 
  label <- var_label(df$value)
  simple <- plot(df$date, df$value, type="l", xlab="Date", main=label)
  acf <- acf(df$value, main =label, na.action=na.pass)
  pacf <- pacf(df$value, main=label, na.action=na.pass)
  summary(df)
  
}

diff_cpi_df <- firstdiff(cpi_df)
diff_inflation_df <- firstdiff(inflation_df)
diff_gdp_df <- firstdiff(gdp_df)
diff_fedfunds_df <- firstdiff(fedfunds_df)

var_label(diff_cpi_df$value) <- "Short-term Interest Rate (1st Difference)"
var_label(diff_inflation_df$value) <- "Short-term Interest Rate (1st Difference)"
var_label(diff_gdp_df$value) <- "Short-term Interest Rate (1st Difference)"
var_label(diff_fedfunds_df$value) <- "Short-term Interest Rate (1st Difference)"

cpi_plots <- function(){all_plots(cpi_df)}
diff_cpi_plots <- function(){all_plots(diff_cpi_df)}
inflation_plots <- function(){all_plots(inflation_df)}
diff_inflation_plots <- function(){all_plots(diff_inflation_df)}
gdp_plots <- function(){all_plots(gdp_df)}
diff_gdp_plots <- function(){all_plots(diff_gdp_df)}
fedfunds_plots <- function(){all_plots(fedfunds_df)}
diff_fedfunds_plots <- function(){all_plots(diff_fedfunds_df)}

# Q2: AR(p) 
# Convert to ts objects

cpi_ts <- ts(cpi_df$value, frequency=12, start=c(1947,1))
inflation_ts <- ts(inflation_df$value, frequency=1, start=c(1960,1))
gdp_ts <- ts(gdp_df$value, frequency=4,start=c(1947,1))
fedfunds_ts <- ts(fedfunds_df$value, frequency = 12, start=c(1954,7))

cpi_ar <- ar(cpi_ts)
# AR(1) model with beta = 0.9965
inflation_ar <- ar(inflation_ts)
# AR(3) model with betas = 1.0402, -0.5186, 0.2855
gdp_ar <- ar(gdp_ts)
# AR(1) model with beta = 0.9853
fedfunds_ar <- ar(fedfunds_ts)
# AR(17) model with many betas

# Q3: 1 and 3 step ahead forecasts - running into some errors here. Maybe should work with FD data?

cpi_forecast1 <- predict(cpi_ar, n.ahead=1, se.fit=TRUE)
cpi_forecast3 <- predict(cpi_ar, n.ahead=3, se.fit=TRUE)

infl_forecast1 <- predict(inflation_ar, n.ahead=1, se.fit=TRUE)
infl_forecast3 <- predict(inflation_ar, n.ahead=3, se.fit=TRUE)

gdp_forecast1 <- predict(inflation_ar, n.ahead=1, se.fit=TRUE)
gdp_forecast3 <- predict(inflation_ar, n.ahead=3, se.fit=TRUE)

ff_forecast1 <- predict(fedfunds_ar, n.ahead-1, se.fit=TRUE)
ff_forecast3 <- predict(fedfunds_ar, n.ahead=3, se.fit=TRUE)


# Problem 2 ---------------------------------------------------------------

# Problem 3 ---------------------------------------------------------------

beta = 0.21
T = 840
rho = 0.972
sigma_u = 30.05*10^4
sigma_v = 0.108*10^4
sigma_uv = -1.621*10^4

Sigma <- matrix(c(sigma_u, sigma_uv, sigma_uv, sigma_v),2,2)

library(MASS)
set.seed(seed = 1232020)

S = 1000
model_beta <- 0

uv <- data.frame(mvrnorm(n=S*T,
                            mu=c(0,0),
                            Sigma=Sigma))

for (s in 1:S){

  # Based on his email maybe we shouldn't use x0 = 0?
  x_t <- 0 + uv[(s-1)*T+1,2]
  y_t <- 0 + uv[(s-1)*T+1,1]
  
  for(i in 2:T){
    x_t[i] <- rho*x_t[i-1]+uv[(s-1)*T+i,2]
    y_t[i] <- beta*x_t[i-1]+uv[(s-1)*T+i,1]
  }
  
  data <- data.frame(y_t,x_t)
  
  model <- lm(y_t ~ lag(x_t), data=data)
  model_beta[s] <- model$coefficients[2]
  
  rm(x_t, y_t, data)

}



