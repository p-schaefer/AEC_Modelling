library(tidyverse)
library(tidymodels)
library(ranger)
library(doParallel)

model_data<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds"))

model_data<-ingredients::select_sample(model_data,1000,seed=1234)


# Define Cross-Validation -------------------------------------------------

cros_v<-group_vfold_cv(model_data,
                       "gen_ProvSegmentID",
                       5)

# Define Recipe -----------------------------------------------------------

recip_main<-recipe(x=model_data) %>% 
  update_role(
    starts_with(c("tx_",
                  "hb_",
                  "nr_")),
    new_role = "predictor"
  ) %>% 
  step_nzv(all_predictors()) %>% 
  step_unorder(all_factor_predictors())


# Define Model ------------------------------------------------------------

r_mod<-boost_tree(mode="regression",
                  learn_rate=tune(),
                  mtry=tune(),
                  trees=tune(),
                  min_n=tune(),
                  tree_depth=tune(),
                  loss_reduction=tune()) %>% 
  set_engine("lightgbm",
             weight_column = "name:case_weight",
             objective = "quantile",
             alpha = 0.5,
             boosting = "gbdt",
             num_threads = parallel::detectCores(logical = FALSE),
             num_leaves = tune(),
             bagging_fraction = tune(),
             bagging_freq = 1,
             early_stopping_round = 50,
             max_cat_to_onehot= tune(),
             min_data_per_group = tune(),
             max_cat_threshold = tune(),
             cat_l2 = tune(),
             cat_smooth = tune()
  )


# Set Tuning Range --------------------------------------------------------

tune_param <- r_mod %>% 
  hardhat::extract_parameter_set_dials() %>% 
  update(
    mtry = sample_prop(c(0.2,0.8)),
    trees = trees(c(1000,10000)), 
    min_n = min_n(c(5,50)),
    tree_depth = tree_depth(c(100,10000)),
    learn_rate=learn_rate(c(-6,1)),
    loss_reduction=tube()
    
    num.random.splits = num_random_splits(c(3,50)),
    regularization.factor=regularization_factor(range = c(0, 1), trans = NULL),
    regularization.usedepth=regularize_depth(values = c(TRUE, FALSE))
  ) 

# Tune Models -------------------------------------------------------------

resp<-model_data %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[grepl("Perc",resp)]
resp<-resp[1]

for (ep in resp) {
  
  wflow <- workflow() %>% 
    add_model(r_mod) %>% 
    add_recipe(recip_main %>% 
                 update_role(
                   any_of(!!ep),
                   new_role = "outcome"
                 )%>% 
                 step_naomit(any_of(!!ep)) %>% 
                 step_naomit(c(everything(),-any_of("case_weight")))
    ) %>% 
    add_case_weights(case_weight)
  
  # all_cores <- parallel::detectCores(logical = FALSE)
  # cl <- makePSOCKcluster(all_cores)
  # registerDoParallel(cl)
  
  tune_out<-tune_bayes(
    object=wflow,
    resamples=cros_v,
    param_info=tune_param,
    metrics=yardstick::metric_set(mae,rmse,rsq),
    initial= 25,
    iter = 50,
    control=control_bayes(
      verbose = T,
      verbose_iter = T,
      no_improve = 15L,
      uncertain = 5,
      save_pred = FALSE,
      time_limit = 60,
      parallel_over = "everything"
    )
  )
  
  # stopCluster(cl)
  
  saveRDS(tune_out,file.path("models",paste0("ModelTune_",ep,".csv")))
  
}


