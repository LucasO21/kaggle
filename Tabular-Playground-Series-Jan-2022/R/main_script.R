# TABULAR PLAYGROUND SERIES - JAN 2022

# SOURCE - "https://www.kaggle.com/c/tabular-playground-series-jan-2022/overview"

# 1.0 SETUP ----

# * 1.1 Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)

# * 1.2 Load Data ----
train_raw_tbl <- read.csv("../Data/train.csv") %>% as_tibble() %>% clean_names()

test_raw_tbl <- read.csv("../Data/test.csv") %>% as_tibble() %>% clean_names()


# * 1.3 Data Inspection ----
dim(train_raw_tbl)

dim(test_raw_tbl)

train_raw_tbl %>% sapply(function(x) sum(is.na(x)))
train_raw_tbl %>% sapply(function(x) sum(x == ""))

test_raw_tbl %>% sapply(function(x) sum(is.na(x)))
test_raw_tbl %>% sapply(function(x) sum(x == ""))

train_raw_tbl %>% count(country)
train_raw_tbl %>% count(store)
train_raw_tbl %>% count(product)

# 1.4 Initial Data Cleaning/Formatting ----
train_clean_tbl <- train_raw_tbl %>% 
    mutate(date = ymd(date)) 

test_clean_tbl <- test_raw_tbl %>% 
    mutate(date = ymd(date)) 


# 2.0 EXPLORATORY DATA ANALYSIS ----

# * 2.1 Plot Time Series ----
train_clean_tbl %>% 
    summarise_by_time(
        .date_var     = date,
        .by           = "day",
        num_sold      = sum(num_sold)
    ) %>% 
    plot_time_series(
        .date_var = date,
        .value    = num_sold 
    )

train_clean_tbl %>% 
    group_by(product) %>% 
    summarise_by_time(
        .date_var     = date,
        .by           = "day",
        num_sold      = sum(num_sold)
    ) %>% 
    plot_time_series(
        .date_var = date,
        .value    = log(num_sold) 
    )

# * 2.2 ACF/PACF ----
train_clean_tbl %>% 
    filter(product == "Kaggle Sticker") %>% 
    summarise_by_time(
        .date_var     = date,
        .by           = "day",
        num_sold      = sum(num_sold)
    ) %>% 
    plot_acf_diagnostics(.date_var = date, .value = num_sold, .lags = 300)

# * 2.3 Anomaly Diagnostics ----
train_clean_tbl %>% 
    filter(product == "Kaggle Sticker") %>% 
    summarise_by_time(
        .date_var     = date,
        .by           = "day",
        num_sold      = sum(num_sold)
    ) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = num_sold)

# * 2.4 Seasonal Diagnostics -----
train_clean_tbl %>% 
    filter(product == "Kaggle Sticker") %>% 
    summarise_by_time(
        .date_var     = date,
        .by           = "day",
        num_sold      = sum(num_sold)
    ) %>% 
    plot_seasonal_diagnostics(.date_var = date, .value = log(num_sold))


# 3.0 DATA PREPARATION ----

# * 3.1 * Aggregate Data ----
full_data_tbl <- train_clean_tbl %>% 
    group_by(country, store, product) %>% 
    summarise_by_time(.date_var = date, .by = "day", num_sold = sum(num_sold)) %>% 
    ungroup() %>% 
    
    # global transformations
    mutate(num_sold = log(num_sold)) %>% 
    
    # extend into future 56 days
    group_by(country, store, product) %>% 
    future_frame(.date_var = date, .length_out = 56, .bind_data = TRUE) %>% 
    ungroup() %>% 
    
    # lags, rolling features & fourier features
    mutate_if(is.character, as.factor) %>% 
    group_by(country, store, product) %>% 
    group_split() %>% 
    map(.f = function(df) {
        df %>% 
            arrange(date) %>% 
            tk_augment_fourier(date, .periods = c(7, 14, 21, 28)) %>% 
            tk_augment_lags(num_sold, .lags = 56) %>% 
            tk_augment_slidify(
                num_sold_lag56,
                .f       = ~ mean(.x, na.rm = TRUE),
                .period  = c(7, 28, 56),
                .partial = TRUE,
                .align   = "center"
            )
    }) %>% 
    bind_rows() %>% 
    rowid_to_column(var = "rowid")

full_data_tbl

# * 3.2 Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(!is.na(num_sold)) %>% 
    drop_na()

# * 3.3 Future Data ----
future_tbl <- full_data_tbl %>% 
    filter(is.na(num_sold))

future_tbl %>% sapply(function(x) sum(is.na(x)))

# * 3.4 Time Split ----
data_prepared_tbl %>% 
    time_series_split(date, assess = 56, cumulative = TRUE)

