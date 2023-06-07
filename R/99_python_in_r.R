library(reticulate)
library(tidyverse)
library(tidymodels)


if (F) {
  # Setup python environment
  install_miniconda()
  conda_create("AEC_Model", python_version="3.11")
  
  miniconda_update()
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/XGBoostLSS.git",pip=T)
  conda_install("AEC_Model","git+https://github.com/dsgibbons/shap.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/LightGBMLSS.git",pip=T)
  
}

use_condaenv("AEC_Model")
lss.model <- import("lightgbmlss.model")
distr<-import("lightgbmlss.distributions.Expectile")

# Prepare Data ------------------------------------------------------------

model_data<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds"))

resp<-model_data %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[grepl("Perc",resp)]
ep<-resp[[1]]

recip_main<-recipe(x=model_data %>%
                     select(-starts_with("gen_")) %>% 
                     select(starts_with(c("tx_",
                                          "hb_",
                                          "nr_")),
                            any_of(ep),
                            any_of("case_weight"))
) %>% 
  update_role(
    starts_with(c("tx_",
                  "hb_",
                  "nr_")),
    new_role = "predictor"
  ) %>% 
  step_nzv(all_predictors()) %>% 
  step_unorder(all_factor_predictors()) %>% 
  update_role(
    any_of(!!ep),
    new_role = "outcome"
  )%>% 
  step_naomit(any_of(!!ep)) %>% 
  step_mutate(across(starts_with(c("tx_")),~as.integer(.))) %>% 
  step_mutate(across(starts_with(c("case_weight")),~as.numeric(.)))

final_data<-prep(recip_main,model_data) %>% juice()

train_py<-lss.model$lgb$Dataset(
  data = final_data %>% select(-any_of(ep),-any_of("case_weight"))  %>% r_to_py(),
  label = final_data %>% select(any_of(ep))  %>% r_to_py(),
  weight = final_data %>% select(any_of("case_weight"))  %>% r_to_py(),
  #feature_name = r_to_py(colnames(final_data %>% select(-any_of(ep)))),
  categorical_feature = r_to_py(which(colnames(final_data) %in% colnames(final_data %>% select(starts_with(c("tx_")))))),
  free_raw_data=r_to_py(FALSE)
)


# Prepare Parameters ------------------------------------------------------

params = list(#boosting= list("categorical",list("gbdt")),
              eta= list("float",list(low= 1e-5, high=1, log=TRUE)), 
              max_depth= list("int",list(low= 10L, high=30L, log=FALSE)),
              num_leaves= list("int",list(low= 124L, high=512L, log=FALSE)),           
              min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
              # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
              # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
              min_gain_to_split= list("float",list(low= 1e-5, high=30, log=TRUE)),
              min_sum_hessian_in_leaf=list("float",list(low= 1e-5, high=300, log=TRUE)),
              subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
              feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
              feature_fraction_by_node = list("float",list(low= 0.2, high=0.8, log=FALSE)),
              bagging_freq = list("int",list(low= 1L, high=10L, log=FALSE)),
              bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
              max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
              max_cat_threshold = list("int",list(low= 124L, high=512L, log=FALSE)),
              min_data_per_group = list("int",list(low= 5L, high=256L, log=FALSE)),
              cat_smooth = list("int",list(low= 0L, high=30L, log=FALSE)),
              cat_l2 = list("int",list(low= 0L, high=30L, log=FALSE))#,
              #device_type = "cpu",
              #categorical_feature = paste0("name:",colnames(final_data %>% select(where(is.factor))))
)

# Define Expectiles ------------------------------------------------------------

lgblss = lss.model$LightGBMLSS(
  distr$Expectile(
    stabilization=r_to_py("None"),              # Options are "None", "MAD", "L2".
    expectiles = r_to_py(c(0.05,0.25, 0.5, 0.75, 0.95)),         # List of expectiles to be estimated, in increasing order.
    penalize_crossing = r_to_py(TRUE)           # Whether to include a penalty term to discourage crossing of expectiles.
  )
)


# Models ------------------------------------------------------------------


opt_param = lgblss$hyper_opt(hp_dict=r_to_py(params),
                             train_set=train_py,
                             num_boost_round=r_to_py(100L),        # Number of boosting iterations.
                             nfold=r_to_py(5L),                    # Number of cv-folds.
                             early_stopping_rounds=r_to_py(20L),   # Number of early-stopping rounds
                             max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                             n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
                             silence=r_to_py(FALSE),              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
                             seed=r_to_py(123L),                   # Seed used to generate cv-folds.
                             hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
)






