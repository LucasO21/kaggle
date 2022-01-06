# TABULAR PLAYGROUND SERIES - JAN 2022

# SOURCE - "https://www.kaggle.com/c/tabular-playground-series-jan-2022/overview"

# 1.0 SETUP ----
setwd("C:/Users/lokwudishu/Desktop/Projects/Kaggle/Tabular-Playground-Series-Jan-2022/R")

# * 1.1 Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)

library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
library(kernlab)
library(rules)
library(earth)

library(tictoc)
library(future)
library(doFuture)
library(parallel)

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
    filter(date >= as.Date("2016-01-01")) %>% 
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

full_data_tbl %>% glimpse()

# * 3.2 Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(!is.na(num_sold)) %>% 
    drop_na()

data_prepared_tbl %>% glimpse()

# * 3.3 Future Data ----
future_tbl <- full_data_tbl %>% 
    filter(is.na(num_sold))

future_tbl %>% glimpse()

future_tbl %>% sapply(function(x) sum(is.na(x)))

# * 3.4 Time Split ----
split_obj <- data_prepared_tbl %>% 
    time_series_split(date, assess = 56, cumulative = TRUE)

split_obj %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, num_sold)

#  4.0 RECIPES/DATA PREPROCESSING ----

# * 4.1 Clean Data - Remove Outliers ----
train_cleaned <- training(split_obj) %>% 
    group_by(country, store, product) %>% 
    mutate(num_sold = ts_clean_vec(num_sold, period = 7))


training(split_obj) %>% 
    group_by(country, store, product) %>% 
    plot_time_series(
        .date_var   = date,
        .value      = num_sold,
        .facet_ncol = 4,
    )

train_cleaned %>% 
    group_by(country, store, product) %>% 
    plot_time_series(
        .date_var   = date,
        .value      = num_sold,
        .facet_ncol = 4,
    )

# * 4.2 Recipe Specification ----
recipe_spec <- recipe(num_sold ~ ., data = train_cleaned) %>% 
    update_role(rowid, new_role = "indicator") %>% 
    step_timeseries_signature(date) %>% 
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>% 
    step_normalize(date_index.num, date_year) %>% 
    step_other(country, store, product) %>% 
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()

# 5.0 MODELING ----

# * 5.1 Prophet ----
wflw_fit_prophet <- workflow() %>% 
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_cleaned)

# * 5.2 Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(
        spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * 5.3 Prophet Boost ----
wflw_fit_prophet_boost <-  workflow() %>% 
    add_model(
        spec = prophet_boost(
            seasonality_daily  = FALSE,
            seasonality_weekly = FALSE,
            seasonality_yearly = FALSE
        ) %>% set_engine("prophet_xgboost")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_cleaned)

# * 5.4 SVM RBF ----
wflw_fit_svm <- workflow() %>% 
    add_model(
        spec = svm_rbf() %>% set_mode("regression") %>% set_engine("kernlab")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * Random Forest ----
wflw_fit_rf <- workflow() %>% 
    add_model(
        spec = rand_forest() %>% set_mode("regression") %>% set_engine("ranger")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * Neural Net ----
wflw_fit_nnet <- workflow() %>% 
    add_model(
        spec = mlp() %>% set_mode("regression") %>% set_engine("nnet")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * MARS ----
wflw_fit_mars <- workflow() %>% 
    add_model(
        spec = mars() %>% set_mode("regression") %>% set_engine("earth")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * Cubist ----
wflw_fit_cubist <- workflow() %>% 
    add_model(
        spec = cubist_rules() %>% set_mode("regression") %>% set_engine("Cubist")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_cleaned)

# * 5.5 Accuracy Check ----
sub_models_fit_tbl <- modeltime_table(
    wflw_fit_prophet,
    wflw_fit_xgboost,
    wflw_fit_prophet_boost,
    wflw_fit_svm,
    wflw_fit_rf,
    wflw_fit_nnet,
    wflw_fit_mars,
    wflw_fit_cubist
)

sub_models_fit_tbl %>% 
    modeltime_accuracy(testing(split_obj)) %>% 
    arrange(smape)

# 6.0 HYPERPARAMETER TUNINT ----

# * 6.1 Setting Up Parallel Processing ----
registerDoFuture()
n_cores <- 4
plan(strategy = cluster, workers = makeCluster(n_cores))

plan(strategy = sequential)

# * 6.2 Resamples Spec ----
set.seed(123)
resamples_kfold <- train_cleaned %>% vfold_cv(v = 5)

resamples_kfold %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(.date_var = date, .value = num_sold, .facet_ncol = 2)


# * 6.3 Model Tuning ----

# ** Cubist Tune ----
model_spec_cubist_tune <- cubist_rules(
    mode = "regression",
    committees = tune(),
    neighbors  = tune()
) %>% 
    set_engine("Cubist")

wflw_spec_cubist_tune <- workflow() %>% 
    add_model(model_spec_cubist_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

set.seed(123)
tic()
tune_results_cubist <- wflw_spec_cubist_tune %>% 
    tune_grid(
        resamples = resamples_kfold,
        grid      = 10,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_fit_cubist_tuned <- wflw_spec_cubist_tune %>% 
    finalize_workflow(select_best(tune_results_cubist, "rmse")) %>% 
    fit(train_cleaned)

wflw_fit_cubist_tuned %>% write_rds("../Artifacts/cubist_tuned.rds")


# ** Ranger Tune ----
model_spec_ranger_tune <- rand_forest(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune()
) %>% 
    set_engine("ranger")

wflw_spec_ranger_tune <- workflow() %>% 
    add_model(model_spec_ranger_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))



set.seed(223)
tic()
tune_results_ranger <- wflw_spec_ranger_tune %>%
    tune_grid(
        resamples  = resamples_kfold,
        grid       = 10,
        control    = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_fit_ranger_tuned <- wflw_spec_ranger_tune %>% 
    finalize_workflow(select_best(tune_results_ranger, "rmse")) %>% 
    fit(train_cleaned)

wflw_fit_ranger_tuned %>% write_rds("../Artifacts/ranger_tuned.rds")

# ** Xgboost Tune ----
model_spec_xgboost_tune <- boost_tree(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
) %>% 
    set_engine("xgboost")

wflw_spec_xgboost_tune <- workflow() %>% 
    add_model(model_spec_xgboost_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

tic()
set.seed(123)
tune_results_xgboost <- wflw_spec_xgboost_tune %>% 
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_xgboost_tune) %>% 
            update(learn_rate = learn_rate(c(0.001, 0.400), trans = NULL)),
        grid       = 10,
        control    = control_grid(verbose = TRUE, allow_par = TRUE)
        
    )
toc()

wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>% 
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>% 
    fit(train_cleaned)

wflw_fit_xgboost_tuned %>% write_rds("../Artifacts/xgboost_tuned.rds")

# ** SVM RBF ----
model_spec_svm_rbf_tune <- svm_rbf(
    mode      = "regression",
    cost      = tune(),
    rbf_sigma = tune(),
    margin    = tune()
) %>% 
    set_engine("kernlab")

wflw_spec_svm_rbf_tune <- workflow() %>% 
    add_model(model_spec_svm_rbf_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

tic()
set.seed(123)
tune_results_svm_rbf <- wflw_spec_svm_rbf_tune %>% 
    tune_grid(
        resamples = resamples_kfold,
        grid      = 10,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_fit_svm_rbf_tuned <- wflw_spec_svm_rbf_tune %>% 
    finalize_workflow(select_best(tune_results_svm_rbf, "rmse")) %>% 
    fit(train_cleaned)

wflw_fit_svm_rbf_tuned %>% write_rds("../Artifacts/svm_rbf_tuned.rds")

# * 6.4 Model Evaluation ----

# ** Modeltime Table ----
sub_models_fit_2_tbl <- modeltime_table(
    wflw_fit_cubist_tuned,
    wflw_fit_ranger_tuned,
    wflw_fit_xgboost_tuned,
    wflw_fit_svm_rbf_tuned
) %>% 
    update_model_description(1, "CUBIST - Tuned") %>% 
    update_model_description(2, "RANGER - Tuned") %>% 
    update_model_description(3, "XGBOOST - Tuned") %>% 
    update_model_description(4, "KERNLAB - Tuned") %>% 
    combine_modeltime_tables(sub_models_fit_tbl)

# ** Calibration Table ----
calibration_tbl <- sub_models_fit_2_tbl %>% 
    modeltime_calibrate(testing(split_obj))

# ** Accuracy Check ----
calibration_tbl %>% 
    modeltime_accuracy() %>% 
    arrange(smape)

# ** Visualize ----
forecast_tbl <- calibration_tbl %>% 
    modeltime_forecast(
        new_data    = testing(split_obj),
        actual_data = data_prepared_tbl,
        keep_data   = TRUE 
    ) 

forecast_tbl %>% 
    filter(country == "Sweden") %>% 
    filter(date >= as.Date("2018-01-01")) %>% 
    filter(product == "Kaggle Hat") %>% 
    group_by(store, product) %>% 
    plot_modeltime_forecast(
        .facet_ncol         = 4,
        .conf_interval_show = FALSE
    )


# 7.0 RESAMPLING ----

# - Assess the stability of our models over time

# * 7.1 Time Series CV ----
set.seed(123)
resamples_tscv <- train_cleaned %>% 
    time_series_cv(
        assess      = 56,
        skip        = 56,
        cumulative  = TRUE,
        slice_limit = 4 
    )

resamples_tscv %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, num_sold)

# * 7.2 Fitting Resamples ----
model_tbl_tuned_resamples <- sub_models_fit_2_tbl %>% 
    filter(.model_desc != "NNET") %>% 
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control = control_resamples(verbose = TRUE, allow_par = TRUE)
    )

# * 7.3 Resampling Accuracy Table ----
model_tbl_tuned_resamples_accuracy <- model_tbl_tuned_resamples %>% 
    modeltime_resample_accuracy(
        metric_set = metric_set(rmse, smape),
        summary_fns = list(mean = mean, sd = sd)
    ) %>% 
    arrange(smape_mean)

# * 7.3 Visualize Tuned Resamples ----
model_tbl_tuned_resamples %>% 
    plot_modeltime_resamples(
        .metric_set  = metric_set(smape),
        .point_size  = 4,
        .point_alpha = 0.8,
        .facet_ncol  = 1
    )

# 8.0 ENSEMBLING ----

# * 8.1 Average Ensemble ----
sub_models_2_ids_to_keep <- c(3, 2, 1)

ensemble_fit_1 <- sub_models_fit_2_tbl %>% 
    filter(.model_id %in% sub_models_2_ids_to_keep) %>% 
    ensemble_average()

ensemble_fit_2 <- sub_models_fit_2_tbl %>% 
    filter(.model_id %in% sub_models_2_ids_to_keep[1:2]) %>% 
    ensemble_average()

ensemble_fit_3 <- sub_models_fit_2_tbl %>% 
    filter(.model_id %in% sub_models_2_ids_to_keep[c(1, 3)]) %>% 
    ensemble_average()

ensemble_fit_4 <- sub_models_fit_2_tbl %>% 
    filter(.model_id %in% sub_models_2_ids_to_keep[2:3]) %>% 
    ensemble_average()

model_ensemble_tbl <- modeltime_table(
    ensemble_fit_1, 
    ensemble_fit_2,
    ensemble_fit_3,
    ensemble_fit_4
) %>% 
    update_model_description(1, 'ENSEMBLE (MEAN) - Xgboost, Ranger, Cubist') %>% 
    update_model_description(2, "ENSEMBLE (MEAN) - Ranger, Xgboost") %>% 
    update_model_description(3, "ENSEMBLE (MEAN) - Cubist, Xgboost") %>% 
    update_model_description(4, "ENSEMBLE (MEAN) - Ranger, Cubist")

# * 8.2 Ensemble Accuracy ----
model_ensemble_calibrate_tbl <- model_ensemble_tbl %>% 
    modeltime_calibrate(testing(split_obj))

model_ensemble_calibrate_tbl %>% 
    modeltime_accuracy() %>% 
    arrange(smape)

# 9.0 REFIT ----

# Here we refit the best ensemble model on the entire dataset

data_prepared_tbl_cleaned <- data_prepared_tbl %>% 
    group_by(country, store, product) %>% 
    mutate(num_sold = ts_clean_vec(num_sold, period = 7)) %>% 
    ungroup()

model_ensemble_refit_tbl <- model_ensemble_tbl %>% 
    filter(.model_id == 1) %>% 
    modeltime_refit(
        data = data_prepared_tbl_cleaned
    )


model_ensemble_refit_tbl %>% 
    modeltime_forecast(
        test_clean_tbl %>% mutate(num_sold = NA)
    )


# SAVING ARTIFACTS ----
feature_engineering_artifacts_list_2 <- list(
    
    # Recipe
    recipes = list(recipe = recipe_spec),
    
    # Models Tuned
    models = list(
        sub_models_tuned = sub_models_fit_2_tbl,
        ensemble_models  = model_ensemble_tbl
    )
    
)

feature_engineering_artifacts_list_2 %>% 
    write_rds("../Artifacts/feat_engineering_artifacts_list_1.rds")
