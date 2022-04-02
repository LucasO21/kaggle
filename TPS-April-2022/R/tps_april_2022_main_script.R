# KAGGLE TPS - APRIL 2022 ----
# *** ----

# 1.0 SETUP ----

# * 1.1 Set Working Dir ----
setwd("~/Desktop/School/2022_Projects/Kaggle/TPS-April-2022/R")

# * 1.2 Libraries ----
library(tidyverse)
library(janitor)

# 1.3 Load Data ----
train_raw_tbl <- read.csv("../Data/train-2.csv") %>% clean_names()

test_raw_tbl <- read.csv("../Data/test-3.csv") %>% clean_names()

train_labels_tbl <- read.csv("../Data/train_labels.csv")

# * 1.4 Data Inspection ----
dim(train_raw_tbl)
dim(test_raw_tbl)
dim(train_labels_tbl)

train_raw_tbl %>% glimpse()
train_raw_tbl %>% distinct(sequence) %>% count()

# * 1.5 Check For Nulls ----
train_raw_tbl %>% sapply(function(x) sum(is.na(x)))
test_raw_tbl %>% sapply(function(x) sum(is.na(x)))

# COMBINE DATA ----

# * Add Train Labels to Train Data ----

