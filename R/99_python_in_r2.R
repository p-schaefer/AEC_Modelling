library(reticulate)
library(tidyverse)
library(tidymodels)

adj<-0.005

inv.logit <- function(f,a) {
  a <- (1-2*a)
  out<-zapsmall((a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f))))
  out[out<0]<-0
  out[out>1]<-1#-1e-14
  out
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
xgb.model <- import("xgboostlss.model")
distr.lgb<-import("lightgbmlss.distributions")
distr.xgb<-import("xgboostlss.distributions")


# Prepare Data ------------------------------------------------------------

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(year(as.Date(gen_SampleDate))>1994) # %>% 
  # mutate(across(contains("_Perc_"),~inv.logit(.,adj))) %>% 
  # mutate(across(contains("_Comm_"),~expm1(.))) 

resp<-model_data0 %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[!grepl("Perc",resp)]
#ep<-resp[[1]]


for (ep in resp){
  
  model_data <- model_data0
  
  recip_main<-recipe(x=model_data %>%
                       select(-starts_with("gen_")) %>% 
                       select(starts_with(c("tx_",
                                            "hb_",
                                            "nr_",
                                            "LDI_")),
                              any_of(ep),
                              any_of(unique(model_data0$tx_Taxa)),
                              any_of("case_weight"))
  ) %>% 
    update_role(
      starts_with(c("tx_",
                    "hb_",
                    "nr_",
                    "LDI_")),
      new_role = "predictor"
    ) %>% 
    step_nzv(all_predictors()) %>% 
    step_unorder(all_factor_predictors()) %>% 
    update_role(
      any_of(!!ep),
      new_role = "outcome"
    ) %>% 
    step_mutate(across(starts_with(c("case_weight")),~as.numeric(.)))
  
  final_data<-prep(recip_main,
                   model_data ) %>%
    juice()
  
  
  
  weight<-final_data %>% select(any_of("case_weight")) %>% unlist()
  
  train_py = xgb.model$DMatrix(data = final_data %>% select(-any_of("case_weight"),-starts_with("resp_")) %>% as.data.frame() %>%   r_to_py(), 
                               label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                               weight =weight %>% as.matrix() %>% r_to_py(),
                               enable_categorical=r_to_py(TRUE),
                               nthread=8L)
  
  
  # Prepare Parameters ------------------------------------------------------
  
  params_xgb = list(booster = list("categorical",list("gbtree")),
                    eta= list("float",list(low= 1e-6, high=2.5, log=TRUE)), 
                    max_depth= list("int",list(low= 100L, high=1000L, log=FALSE)),
                    lambda = list("float",list(low= 1e-12, high=2.5, log=TRUE)),
                    alpha = list("float",list(low= 1e-12, high=2.5, log=TRUE)),
                    gamma = list("float",list(low= 1e-6, high=2.5, log=TRUE)),
                    min_child_weight =list("float",list(low= 1e-6, high=2.5, log=TRUE)),
                    subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
                    colsample_bynode = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                    max_cat_to_onehot = list("int",list(low= 2L, high=20L, log=FALSE)),
                    max_cat_threshold = list("int",list(low= 2L, high=512L, log=FALSE))
  )
  
  # Define Expectiles ------------------------------------------------------------
  
  xgb = xgb.model$XGBoostLSS(
    distr.xgb$ZALN$ZALN(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  
  # Models ------------------------------------------------------------------

  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_xgb),
                            dtrain=train_py,
                            num_boost_round=r_to_py(5000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(25L),   # Number of early-stopping rounds
                            #max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            max_minutes=r_to_py(60L*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
                            silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
                            #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
                            #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
  )
  
  
  
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_",ep,".rds")))
  
  
  
  # validate CV -------------------------------------------------------------
  
  # Define Cross-Validation -------------------------------------------------
  
  if (T){
    opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_",ep,".rds")))
    
    cros_v<-group_vfold_cv(model_data,
                           "gen_ProvSegmentID",
                           20)
    
    out_res<-list()
    for (i in 1:nrow(cros_v)){
      
      
      xgb = xgb.model$XGBoostLSS(
        distr.xgb$ZALN$ZALN(
          stabilization = "None",
          response_fn = "exp",
          loss_fn="nll"
        )
      )
      
      trn<-training(cros_v$splits[[i]])
      
      final_prep<-prep(recip_main,trn)
      
      tst<-testing(cros_v$splits[[i]]) %>% 
        bake(object=final_prep)
      
      trn<-juice(final_prep)
      
      train_py = xgb.model$DMatrix(data = trn %>% select(-any_of("case_weight"),-starts_with("resp_")) %>% as.data.frame() %>%   r_to_py(), 
                                   label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                   weight =trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                   enable_categorical=r_to_py(TRUE),
                                   nthread=8L)
      
      test_py = xgb.model$DMatrix(data = tst %>% select(-any_of("case_weight"),-starts_with("resp_")) %>% as.data.frame() %>%   r_to_py(), 
                                  label=tst %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                  weight =tst %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                  enable_categorical=r_to_py(TRUE),
                                  nthread=8L)
      
      
      xgb$train(
        r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
        train_py,
        num_boost_round=opt_param$opt_rounds
      )
      
      # Calculate quantiles from predicted distribution
      pred_quantiles = xgb$predict(test_py,
                                   pred_type="quantiles",
                                   n_samples=2000L,
                                   quantiles=c(0.05,0.25,0.5,0.75,0.95))
      
      out<-pred_quantiles %>% 
        bind_cols(tst %>%
                    select(any_of(ep),tx_Taxa,everything()) %>%
                    rename_with(.cols=any_of(ep),~paste0("Pred")) %>% 
                    mutate(endpoint=ep,
                           cv_fold=i)
                  )
      
      out_res[[i]]<-out
      
    }
    
    out_res<-bind_rows(out_res)
    
    saveRDS(out_res,file.path("data","models","LSS",paste0("best_params_OOB_Pred_",ep,".rds")))
    
    
  }
  
}

