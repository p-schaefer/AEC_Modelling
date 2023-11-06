library(reticulate)
library(tidyverse)
library(tidymodels)

adj<-0.005

inv.logit <- function(f,a) {
  a <- (1-2*a)
  zapsmall((a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f))))
}

if (F) {
  # Setup python environment
  install_miniconda()
  conda_create("AEC_Model", python_version="3.11")
  use_condaenv("AEC_Model")
  
  miniconda_update()
  
  conda_install("AEC_Model","IPython",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/XGBoostLSS.git",pip=T)
  conda_install("AEC_Model","git+https://github.com/dsgibbons/shap.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/LightGBMLSS.git",pip=T)
  
}

use_condaenv("AEC_Model")

# multiprocessing <- import("multiprocessing")
# 
# xgb_data <- import("xgboostlss.datasets.data_loader")
# xgb_model <- import("xgboostlss.model")
# xgb_distr<-import("xgboostlss.distributions.Expectile")

lss.model <- import("lightgbmlss.model")
distr<-import("lightgbmlss.distributions")


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
  #step_mutate(across(starts_with(c("tx_")),~as.integer(.))) %>% 
  step_mutate(across(starts_with(c("case_weight")),~as.numeric(.))) #%>% 
#step_mutate(across(starts_with("resp_Perc_"),~inv.logit(.,adj)))

final_data<-prep(recip_main,model_data) %>% juice()

weight<-final_data %>% select(any_of("case_weight")) %>% unlist()
# weight<-round(weight/sum(weight)*10000000000,1)
# weight<-tibble(wt=weight) %>% 
#   group_by(wt) %>% 
#   mutate(wt2=paste0(wt,row_number())) %>% 
#   mutate(wt2=as.numeric(as.character(wt2)) )%>% 
#   mutate(wt2=floor(wt2)) %>% 
#   pull(wt2)
#weight[]<-1

train_py<-lss.model$lgb$Dataset(
  data = final_data %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
  label = final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
  weight =  weight %>% as.matrix() %>% r_to_py()#,
  #feature_name = r_to_py(colnames(final_data %>% select(-any_of(ep)))),
  #categorical_feature = r_to_py(which(colnames(final_data) %in% colnames(final_data %>% select(starts_with(c("tx_"))))))
  #free_raw_data=r_to_py(FALSE)
)

# train_py_xgb<-xgb_model$xgb$DMatrix(
#   final_data %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame()  %>% r_to_py(),
#   label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
#   #weight=final_data %>% select(any_of("case_weight")) %>% as.matrix()  %>% r_to_py(),
#   enable_categorical = r_to_py(TRUE),
#   nthread=4
# )


# Prepare Parameters ------------------------------------------------------

params_lightgbm = list(boosting= list("categorical",list("gbdt")),
                       feature_pre_filter= list("categorical",list("false")),
                       eta= list("float",list(low= 1e-7, high=1, log=TRUE)), 
                       max_depth= list("int",list(low= 10L, high=1000L, log=FALSE)),
                       num_leaves= list("int",list(low= 124L, high=1024L, log=FALSE)),           
                       min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
                       # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
                       # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
                       min_gain_to_split= list("float",list(low= 1e-7, high=30, log=TRUE)),
                       min_sum_hessian_in_leaf=list("float",list(low= 1e-7, high=300, log=TRUE)),
                       subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       feature_fraction_by_node = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       bagging_freq = list("int",list(low= 1L, high=10L, log=FALSE)),
                       bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
                       max_cat_threshold = list("int",list(low= 124L, high=512L, log=FALSE)),
                       min_data_per_group = list("int",list(low= 5L, high=256L, log=FALSE)),
                       cat_smooth = list("float",list(low= 0L, high=50L, log=FALSE)),
                       cat_l2 = list("float",list(low= 0L, high=50L, log=FALSE)),
                       histogram_pool_size = list("int",list(low= 4000L, high=4000L, log=FALSE))
                       #device_type = "cpu",
                       #categorical_feature = paste0("name:",colnames(final_data %>% select(where(is.factor))))
)

# params_xgboost = list(#booster = list("categorical",list("gbtree")),
#                       #tree_method = list("categorical",list("hist")),
#                       #grow_policy = list("categorical",list("lossguide")),
#                       eta= list("float",list(low= 1e-5, high=1, log=TRUE))#, 
#                       #max_depth = list("int",list(low= 10L, high=30L, log=FALSE)),
#                       #max_leaves = list("int",list(low= 124L, high=512L, log=FALSE)),           
#                       #gamma = list("float",list(low= 1e-5, high=30, log=TRUE)),
#                       #min_child_weight =list("float",list(low= 1e-5, high=300, log=TRUE)),
#                       #subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
#                       #colsample_bytree  = list("float",list(low= 0.2, high=0.8, log=FALSE)),
#                       #colsample_bynode  = list("float",list(low= 0.2, high=0.8, log=FALSE)),
#                       #subsample  = list("float",list(low= 0.2, high=0.8, log=FALSE)),
#                       #max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
#                       #max_cat_threshold = list("int",list(low= 124L, high=512L, log=FALSE))
# )

# Define Expectiles ------------------------------------------------------------

lgblss = lss.model$LightGBMLSS(
  distr$Expectile$Expectile(
    stabilization=r_to_py("MAD"),              # Options are "None", "MAD", "L2".
    expectiles = r_to_py(c( 0.05, 0.5, 0.95)),         # List of expectiles to be estimated, in increasing order.
    penalize_crossing = r_to_py(TRUE)           # Whether to include a penalty term to discourage crossing of expectiles.
  )
)

# lgblss2 = lss.model$LightGBMLSS(
#   distr$Beta$Beta(
#     stabilization=r_to_py("MAD"),              # Options are "None", "MAD", "L2".
#     response_fn = r_to_py("exp")           # Whether to include a penalty term to discourage crossing of expectiles.
#   )
# )

# xgblss = xgb_model$XGBoostLSS(
#   xgb_distr$Expectile(
#     stabilization=r_to_py("None"),              # Options are "None", "MAD", "L2".
#     expectiles = r_to_py(c(0.25, 0.75)),         # List of expectiles to be estimated, in increasing order.
#     penalize_crossing = r_to_py(TRUE)           # Whether to include a penalty term to discourage crossing of expectiles.
#   )
# )

# Models ------------------------------------------------------------------


opt_param = lgblss$hyper_opt(hp_dict=r_to_py(params_lightgbm),
                             train_set=train_py,
                             num_boost_round=r_to_py(10000L),        # Number of boosting iterations.
                             nfold=r_to_py(5L),                    # Number of cv-folds.
                             early_stopping_rounds=r_to_py(15L),   # Number of early-stopping rounds
                             max_minutes=r_to_py(60*11L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                             #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
                             silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
                             #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
                             #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
)

# opt_param = lgblss2$hyper_opt(hp_dict=r_to_py(params_lightgbm),
#                              train_set=train_py,
#                              num_boost_round=r_to_py(100L),        # Number of boosting iterations.
#                              nfold=r_to_py(5L),                    # Number of cv-folds.
#                              early_stopping_rounds=r_to_py(30L),   # Number of early-stopping rounds
#                              max_minutes=r_to_py(60*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
#                              #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
#                              silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
#                              #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
#                              #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
# )


#saveRDS(opt_param,file.path("data","final",paste0("best_params_",ep,".rds")))
saveRDS(opt_param,paste0("best_params_",ep,".rds"))


# opt_param = xgblss$hyper_opt(hp_dict=r_to_py(params_xgboost),
#                              dtrain=train_py_xgb,
#                              num_boost_round=r_to_py(100L),        # Number of boosting iterations.
#                              nfold=r_to_py(5L),                    # Number of cv-folds.
#                              early_stopping_rounds=r_to_py(20L),   # Number of early-stopping rounds
#                              max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
#                              silence=r_to_py(FALSE)                # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
# )




