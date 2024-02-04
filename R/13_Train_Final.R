library(reticulate)
library(tidyverse)
library(tidymodels)

use_condaenv("AEC_Model")


# Load Python Modules -----------------------------------------------------

lss.model <- import("lightgbmlss.model")
distr.lgb<-import("lightgbmlss.distributions")
shap<-import("shap")

# Load Data ---------------------------------------------------------------

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(year(as.Date(gen_SampleDate))>1994) %>% 
  mutate(across(starts_with("resp_"),
                ~case_when(
                  .x < quantile(.x[.x>0],0.01) ~ "0 - 1",
                  .x < quantile(.x[.x>0],0.2) ~ "1 - 20",
                  .x < quantile(.x[.x>0],0.4) ~ "20 - 40",
                  .x < quantile(.x[.x>0],0.6) ~ "40 - 60",
                  .x < quantile(.x[.x>0],0.8) ~ "60 - 80",
                  .x >= quantile(.x[.x>0],0.8) ~ "80 - 100"
                ),
                .names = "cat_{.col}")) %>% 
  mutate(across(starts_with("cat_resp_"),~factor(.x)))

resp<-model_data0 %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[!grepl("Perc|cat_",resp)]
#resp<-resp[[1]]

booster<-"dart"

for (ep in resp){
  
  # Prepare datasets --------------------------------------------------------
  recip<-readRDS(file.path("data","models","LSS",paste0("Final_Recipe_",ep,".rds")))
  
  recip_main<-recip$recip_main
  final_prep<-recip$final_prep
  
  train_data<- final_prep %>%
    juice()
  
  train_py = lss.model$Dataset(
    data=train_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
    label=train_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py()
  )
  
  # Define Model ------------------------------------------------------------
  xgb = lss.model$LightGBMLSS(
    distr.lgb$ZAGamma$ZAGamma(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  
  # Load optimal hyperparameters --------------------------------------------
  opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_",booster,".rds")))
  
  xgb$train(
    r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
    train_py,
    num_boost_round=opt_param$opt_rounds
  )
  
  # Save Model --------------------------------------------------------
  
  xgb$save_model(r_to_py(file.path("data","models","LSS",paste0("Final_Model_",ep,"_",booster,".txt"))))
  
  if (T){
    # OOS Predictions -------------------------------------------------------------
    
    cros_v<-group_vfold_cv(model_data0,
                           "gen_ProvReachID",
                           50)
    
    out_res<-list()
    for (i in 1:nrow(cros_v)){
      xgb = lss.model$LightGBMLSS(
        distr.lgb$ZAGamma$ZAGamma(
          stabilization = "None",
          response_fn = "exp",
          loss_fn="nll"
        )
      )
      
      trn<-training(cros_v$splits[[i]]) %>% 
        mutate(across(starts_with(c("case_weight")),~as.numeric(.))) %>% 
        group_by(gen_ProvReachID,gen_link_id,gen_StreamName,across(starts_with("tx_"))) %>% 
        summarise(across(where(is.numeric),~median(.x,na.rm=T)),
                  across(!where(is.numeric),~tail(.x,1)),
                  .groups="drop")
      
      final_prep<-prep(recip_main,trn)
      
      tst<-testing(cros_v$splits[[i]]) %>% 
        bake(object=final_prep) 
      
      trn<-trn %>% 
        bake(object=final_prep)
      
      train_py = lss.model$Dataset(
        data=trn %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
        label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
        #weight = trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
      )
      
      xgb$train(
        r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
        train_py,
        num_boost_round=opt_param$opt_rounds
      )
      
      
      # Calculate quantiles from predicted distribution
      pred_quantiles = xgb$predict(tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
                                   pred_type="quantiles",
                                   n_samples=2000L,
                                   quantiles=c(0.05,0.25,0.5,0.75,0.95))
      
      pred_samples = xgb$predict(tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
                                 pred_type="samples",
                                 n_samples=1000L) %>% 
        rowMeans() %>% 
        unlist()
      
      out<-pred_quantiles %>% 
        mutate(predicted=pred_samples) %>% 
        bind_cols(testing(cros_v$splits[[i]]) %>%
                    select(any_of(ep),tx_Taxa,everything()) %>%
                    rename_with(.cols=any_of(ep),~paste0("observed")) %>% 
                    mutate(endpoint=ep,
                           cv_fold=i)
        ) %>% 
        mutate(gen_ProvReachID=testing(cros_v$splits[[i]])$gen_ProvReachID)
      
      out_res[[i]]<-out
    }
    
    out_res<-bind_rows(out_res)
    
    saveRDS(out_res,file.path("data","models","LSS",paste0("OOB_Pred_",ep,"_",booster,".rds")))
  }
}