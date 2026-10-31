# Loading in the raw data

library(tidyverse)
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

# Classifying "observation_date" as <date>

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# Reshaping data from a wide to a long format

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), 
               names_to = "series_id", 
               values_to = "sales")

head(retail_raw_long)

# Creating a tsibble

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)

# Canadian_gas: tsibble of monthly natural gas production in Canada.
# Measured in billions of cubic meters, from Jan 1960 to Feb 2005.

head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

# Working with dates in a tsibble: filtering by date

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# Working with dates in a tsibble: extracting parts of a date

can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# Calendar month with the highest average production: December (17.1)
# Calendar month with the lowest average production: June (14.9)

# Working with dates in a tsibble: data arithmetic

can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# Working with dates in a tsibble: changing the frequency of a series

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# We sum the monthly values for gas production rather than take their mean because we want the total amount of gas production within a quarter, not the mean gas production in a quarter. Taking the mean might also smooth out seasonality that we want to take note of.
# The quarterly series is still seasonal, peaking in Q1 and Q4 and dropping in Q2 and Q3.

# Plotting with autoplot

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# There are consistent spikes and drops in the NSA series, with huge spikes occurring at the end of a year and huge drops occurring at the beginning of a year.
# The SA series does not have this same pattern. Its levels waver up and down the y-axis, but those small changes don't seem consistent or to correspond to seasonal changes.

#The two series do follow the same path in the long run. 
# Both the NSA and SA series are trending up at the same approximate rate over time, and larger peaks and valleys that are caused by recessions or high economic growth are evident in both series.

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Natural Gas Production in Canada",
    subtitle = "Monthly amounts, from January 1960 to February 2005",
    x = NULL, y = "Billions of cubic meters", color = NULL
  ) +
  theme_minimal()

# Adding a ggplot2 layer to the plot

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  scale_y_continuous(labels = scales::label_comma(scale =  1))
  theme_minimal()

# I chose to use scale_y_continuous() to clean up the y-axis.
# Previously, the units of the y-axis were in scientific notation, which is confusing and didn't quite match the y-axis label of units being in millions of dollars. To match the axis label, the display units on the y-axis needed to match the scale of units in the data set. So, I changed their scale so the data set, y-axis labels, and y-axis units all match up.


