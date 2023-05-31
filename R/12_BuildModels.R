library(tidyverse)
library(tidymodels)
library(ranger)
library(doParallel)

model_data<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds"))

model_data<-ingredients::select_sample(model_data,10000,seed=1234)


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

r_mod<-rand_forest(mode="regression",
                   mtry=tune(),
                   trees=tune(),
                   min_n=tune()) %>% 
  set_engine("ranger",
             replace=FALSE,
             max.depth = tune(),
             splitrule = "extratrees",
             num.random.splits = tune(),
             regularization.factor = tune(),
             regularization.usedepth = tune(),
             always.split.variables = model_data %>% select(starts_with("tx_")) %>% colnames(),
             quantreg = TRUE,
             keep.inbag = TRUE,
             oob.error = FALSE,
             num.threads=parallel::detectCores(logical = FALSE),
             respect.unordered.factors="partition"
  )


# Set Tuning Range --------------------------------------------------------

tune_param <- r_mod %>% 
  hardhat::extract_parameter_set_dials() %>% 
  update(
    mtry = sample_prop(c(0.1,0.9)),
    trees = trees(c(100000,1000000)), 
    min_n = min_n(c(20,50)),
    max.depth = tree_depth(c(100,100000)),
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
    initial= 10,
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


