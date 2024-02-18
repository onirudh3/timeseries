
# Libraries ---------------------------------------------------------------

library(fredr)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)


# Problem 1 ---------------------------------------------------------------

# Search FRED
fredr_series_search_text("FPCPITOTLZGUSA")
fredr_series("FPCPITOTLZGUSA")$title

# FRED Data
fredr_set_key("eb56ad2180a43dce3526dc9cd086b33b")

## Consumer price index: Consumer Price Index for All Urban Consumers: All Items in U.S. City Average ----
cpi_df <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2024-01-01")
)

## Inflation ----
inflation_df <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("1960-07-01"),
  observation_end = as.Date("2022-01-01")
)

## GDP ----
gdp_df <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2023-10-01")
)

## Short term interest rate: federal funds effective rate
fedfunds_df <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1954-07-01"),
  observation_end = as.Date("2024-01-01")
)

## Electricity consumption: EIA monthly data ----

electricity_df <- read_excel("Retail_sales_of_electricity.xlsx") %>% 
  gather(date, value) %>%
  mutate(date = excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"))



# Problem 2 ---------------------------------------------------------------

stock_df <- read.csv("apple_stock_prices_data.csv")



# Problem 3 ---------------------------------------------------------------


