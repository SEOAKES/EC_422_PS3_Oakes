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

## Calendar month with the highest average production: December (17.1)
## Calendar month with the lowest average production: June (14.9)

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

## We sum the monthly values for gas production rather than take their mean because we want the total amount of gas production within a quarter, not the mean gas production in a quarter. Taking the mean might also smooth out seasonality that we want to take note of.
## The quarterly series is still seasonal, peaking in Q1 and Q4 and dropping in Q2 and Q3.

# Plotting with autoplot

library(ggplot2)

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## There are consistent spikes and drops in the NSA series, with huge spikes occurring at the end of a year and huge drops occurring at the beginning of a year.
## The SA series does not have this same pattern. Its levels waver up and down the y-axis, but those small changes don't seem consistent or to correspond to seasonal changes.

## The two series do follow the same path in the long run. 
## Both the NSA and SA series are trending up at the same approximate rate over time, and larger peaks and valleys that are caused by recessions or high economic growth are evident in both series.

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

## I chose to use scale_y_continuous() to clean up the y-axis.
## Previously, the units of the y-axis were in scientific notation, which is confusing and didn't quite match the y-axis label of units being in millions of dollars. To match the axis label, the display units on the y-axis needed to match the scale of units in the data set. So, I changed their scale so the data set, y-axis labels, and y-axis units all match up.

## The seasonal swings in can_gas do seem to get larger are the series grows, but this is not true over the whole same. It seems to generally be the case up until around 1990, where the seasonal swings become less pronounced despite the series continuing to trend up over time.
## I'm not quite sure how to describe the way the seasonal swings are changing over time, but it isn't completely related to the series' level trending up over time.
  
# Should we log can_gas?
  
can_gas <-  can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Natural Gas Production in Canada",
    subtitle = "Monthly amounts, from January 1960 to February 2005",
    x = NULL, y = "Log of billions of cubic meters", color = NULL
  ) +
  theme_minimal()

## By logging Volume, some of the swings in the series have become less extreme, making me think that some of these swings were seasonal and increased as a result of the series' level trending upward over time.
## The trend line of the series also changed, from a positive linear trend to a trend with a shrinking positive rate of change over time.
## Since the abnormally large but consistent swings in certain parts of the data did not disappear and become uniform with other swings when Volume was logged, I don't think it's worth logging can_gas. While there is likely some seasonality present in the data contributing to the abnormal swings, there is clearly a larger factor at work causing them that is currently unaccounted for. Taking the log of the series doesn't contribute much to clarity in that department, so I'd say it's not very useful.

# Should we log RSAFSNA?

retail_ts |>
  mutate(log_sales = log(sales)) |>
  filter(series_id == "RSAFSNA") |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Logged, not seasonally adjusted",
    x = NULL, y = "log_sales", color = NULL
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  scale_y_continuous(labels = scales::label_comma(scale =  1))
  theme_minimal()

## The case for logging retail_ts is much stronger than the case for logging can)gas. 
## Unlike can_gas, by logging the non-seasonally adjusted data in retail_ts, the seasonal swings have become mostly consistent in size over time and the trend has become more linear, both of which are the main goals of logging a variable.
  
# Growth rates

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )
can_gas |>
  autoplot(growth_direct)

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))
can_gas |>
  autoplot(growth_gap)

## In this case, I don't think the log approximation is very good.
## The growth levels (both positive and negative) that can_gas experiences, particularly in the 1960s and the 1980s, are so extreme that the approximation that the first difference of the logged series provides is just not accurate enough. If the series weren't as volatile, perhaps the difference of the logged series would be acceptable.

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title =  "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## The month-over-month growth rate in the NSA series is much noisier than the month-over-month growth rate of the SA series.
## As I noted before in Part 2.7b, the NSA series is definitely affected by seasonality, and this shows in the month-over-month growth rate. When you remove seasonal factors, the growth rate (in this case, the change in retail sales between months) actually doesn't change that much.

