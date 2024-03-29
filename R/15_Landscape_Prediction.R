library(reticulate)
library(tidyverse)
library(tidymodels)

use_condaenv("AEC_Model")
booster<-"dart"

# Load Python Modules -----------------------------------------------------

lss.model <- import("lightgbmlss.model")
distr.lgb<-import("lightgbmlss.distributions")
shap<-import("shap")

# Load Data ---------------------------------------------------------------

pred_data_fin<-read_rds(file.path("data","final","Prediction_finaltaxa_data.rds"))

pred_refdata_fin<-read_rds(file.path("data","final","Prediction_ref_finaltaxa_data.rds")) 


out_landscape<-list()
out_Reflandscape<-list()
for (ep in c("resp_Comm_Biomass","resp_Comm_Abundance")){ #
  
  # Prepare datasets --------------------------------------------------------
  recip<-readRDS(file.path("data","models","LSS",paste0("Final_Recipe_",ep,".rds")))
  
  recip_main<-recip$recip_main
  final_prep<-recip$final_prep
  
  pred_data<- bake(final_prep,
                   pred_data_fin)
  
  pred_refdata<- bake(final_prep,
                      pred_refdata_fin)
  
  pred_data_final<-pred_data %>% 
    select(-starts_with(c("case_weight","resp_","cat_resp_","gen_Region"))) %>% 
    as.data.frame() 
  
  pred_refdata_final<-pred_refdata %>% 
    select(-starts_with(c("case_weight","resp_","cat_resp_","gen_Region"))) %>% 
    as.data.frame()
  
  # Define Model ------------------------------------------------------------
  xgb = lss.model$LightGBMLSS(
    distr.lgb$ZAGamma$ZAGamma(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  
  # Load Model --------------------------------------------------------
  xgb<-xgb$load_model(r_to_py(file.path("data","models","LSS",paste0("Final_Model_",ep,"_",booster,".txt"))))
  
  # Get Predictions ---------------------------------------------------------
  pred_quantiles = xgb$predict(pred_data_final %>% r_to_py(),
                               pred_type="quantiles",
                               n_samples=1000L,
                               quantiles=c(0.05,0.16,0.25,0.33,0.5,0.66,0.75,0.84,0.95))
  
  pred_params = xgb$predict(pred_data_final %>% r_to_py(),
                            pred_type="parameters")
  
  
  pred_samples = xgb$predict(pred_data_final %>% r_to_py(),
                               pred_type="samples",
                               n_samples=1000L) %>% 
    rowMeans() %>% 
    unlist()
  
  fin_data<-pred_quantiles %>% 
    mutate(predicted=pred_samples) %>% 
    bind_cols(pred_params) %>% 
    rename_with(~paste0(ep,"_",.x))
  
  out_landscape[[ep]]<-fin_data
  
  # Get Ref Predictions ---------------------------------------------------------
  pred_quantiles = xgb$predict(pred_refdata_final %>%   r_to_py(),
                               pred_type="quantiles",
                               n_samples=1000L,
                               quantiles=c(0.05,0.16,0.25,0.33,0.5,0.66,0.75,0.84,0.95))
  
  pred_params = xgb$predict(pred_refdata_final %>%   r_to_py(),
                            pred_type="parameters")
  
  
  pred_samples = xgb$predict(pred_refdata_final %>%   r_to_py(),
                             pred_type="samples",
                             n_samples=1000L) %>% 
    rowMeans() %>% 
    unlist()
  
  fin_refdata<-pred_quantiles %>% 
    mutate(predicted=pred_samples) %>% 
    bind_cols(pred_params) %>% 
    rename_with(~paste0(ep,"_",.x))
  
  out_Reflandscape[[ep]]<-fin_refdata
}

out_final<-bind_cols(out_landscape) %>% 
  bind_cols(pred_data_fin) %>% 
  select(-starts_with("cat_resp")) %>% 
  mutate(pred_type="observed") %>% 
  select(pred_type,everything())

saveRDS(out_final,file.path("data","models","LSS",paste0("Landscape_Predictions_",booster,".rds")))

out_Reffinal<-bind_cols(out_Reflandscape) %>% 
  bind_cols(pred_refdata_fin) %>% 
  select(-starts_with("cat_resp"))%>% 
  mutate(pred_type="reference")%>% 
  select(pred_type,everything())

saveRDS(out_Reffinal,file.path("data","models","LSS",paste0("Landscape_RefPredictions_",booster,".rds")))
