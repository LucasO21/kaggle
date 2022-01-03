# TABULAR PLAYGROUND SERIES - JAN 2022

# SOURCE - "https://www.kaggle.com/c/tabular-playground-series-jan-2022/overview"

# 1.0 SETUP ----

# * 1.1 Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)

# * 1.2 Load Data ----
data_raw_tbl <- read.csv("../Data/train.csv") %>% as_tibble() %>% clean_names()

test_raw_tbl <- read.csv("../Data/test.csv") %>% as_tibble() %>% clean_names()


# * 1.3 Data Inspection ----