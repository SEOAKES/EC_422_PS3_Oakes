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

# Seeing, decomposing, and modeling seasonality

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

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) + 
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  scale_y_continuous(labels = scales::label_comma(scale =  1))
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) + 
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  scale_y_continuous(labels = scales::label_comma(scale =  1))
  theme_minimal()

## The NSA plot has many more peaks and valleys between months than the SA plot.
## These peaks and valleys are virtually non-existent in the SA plot, meaning they can be clearly attributed to seasonal effects on retail sales; by removing their effects from the plot, we can more clearly see the steady positive trend of retail sales over time (aside from some outlier, of course, such as the COVID-19 pandemic).

can_gas |>
  gg_season(log_volume) + 
    labs(
      title = "Monthly Natural Gas Production in Canada",
      subtitle = "Plotted to emphasize seasonality",
      x = NULL, y = "Log of billions of cubic meters", color = NULL
    ) +
      theme_minimal()

## As we enter colder winter months, it appears that natural gas production increases, and throughout the warmer summer months, natural gas production slows down.
## This makes sense, as one would assume that in a colder country like Canada, more natural gas production in the colder months is necessary to meet the demand of people who want to use the natural gas to heat their homes.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

library(seasonal)

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

### Seasonal factor of 1.10, for example, means that month is typically 10% above the trend.

## The seasonal factor appears to be the highest in December, with sales ranging from roughly 10-20% higher than the trend (sales start at above 20% higher than the trend at the beginning of the data set, but decrease to about 10% by the most recent years.)
## The size of the seasonal swings is changing over time, reaching lower positive extremes and roughly consistent negative extremes.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * log(mine) - log(census))

sa_compare |>
  ggplot(aes(x = Month, y= pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y =  "Difference (%)"
  ) + 
  theme_minimal()

## Considering the difference in percent begins in the near 1200% range, I'd say that my adjustment does not match the official one very well.
## While my adjustment seems to match the official adjustment a little better during major recessions (the difference % drops significantly during both the 2008 financial crisis and the beginning of the COVID-19 pandemic), my seasonal adjustment seems to differ more and more from the census bureau's as the years in the data set climb higher.

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))
report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

## The estimated average monthly growth rate of gas production is 0.004, or 0.4%.
## The month with the most significant seasonal effect is June, with a seasonal effect estimate of -0.251.

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

## The fitted line does not do a very good job of tracking the actual series, in my opinion.
## What the fitted line does do is well is track the series between 1990 and 2000, as the series is experiencing some relatively linear growth there. However, even between those year, since the fitted line exhibits constant seasonal variation over the years, it doesn't capture the reduction in seasonal swings that begin to occur past 1990.
## The fitted line does a particularly bad job at capturing the temporary spike in Canadian gas production that begins around 1970, nor does it capture the increased seasonal variation in gas production  during that spike.
## Overall, due to its nature as a linear fitted model and its inability to respond to changes in variations of seasonality over time, the linear trend model struggles to capture some of the important features of the Canadian gas production data.

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

## The residuals do not look like white noise. The plot of the ACF clearly shows persistence over time in the residuals, which means there is some factor unaccounted for in the linear trend model that would model the effect of these residuals and take care of them.
## At the 5% level, the Ljung-Box text concludes that we reject the null hypothesis that the first 24 lags are uncorrelated.

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

## First forecast that is evaluated: January 1980
## Last forecast that is evaluated: March 2005
## MAE of snaive: 0.562
## MAE of tslm: 4.01
## RMSE of snaive: 0.702
## RMSE of tslm: 4.36

## It appears that the SNAIVE model forecasts much more accurately, and both the MAE and RMSE metrics agree on this. 

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |>
              select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

## DM statistic: -31.278
## p-value: < 2.2e-16

## With how large the p-value is, it's clear that we must reject the null hypothesis of the DM test that the SNAIVE model and TSLM model have equal forecast accuracy. In this case, SNAIVE is clearly more effective at forecasting.

gas_cv_fc_rolling <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_rolling |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

## MAE of snaive with rolling window: 0.562
## MAE of tslm with rolling window: 2.09
## RMSE of snaive with rolling window: 0.702
## RMSE of tslm with rolling window: 2.42

## The model whose accuracy changed more with the rolling window was the TSLM model.
## I imagine this is the case because the TSLM model struggles to make adjust to extreme changes over time in the data, both in its levels and seasonality.
## By changing the forecast generating method to a rolling window, the TSLM model doesn't have to take into account old observations that don't correspond to the current trends of the series. Its ability to respond to changes in how the data series generates is much more enhanced.