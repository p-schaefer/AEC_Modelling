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

model_data0_fin<-read_rds(file.path("data","final","Prediction_finaltaxa_data.rds"))
model_refdata0_fin<-read_rds(file.path("data","final","Prediction_ref_finaltaxa_data.rds")) 


# resp<-model_data0 %>% select(starts_with("resp_")) %>% colnames()
# resp<-resp[!grepl("Perc|cat_",resp)]
# #ep<-resp[[1]]

for (dt in c("Current","Reference")){
  
  if (dt == "Current"){
    model_data0 <- model_data0_fin
  } else {
    model_data0 <- model_refdata0_fin
  }
  
  for (ep in c("resp_Comm_Biomass","resp_Comm_Abundance")){ #,"resp_Comm_Abundance"
    
    # Prepare datasets --------------------------------------------------------
    recip<-readRDS(file.path("data","models","LSS",paste0("Final_Recipe_",ep,".rds")))
    
    recip_main<-recip$recip_main
    final_prep<-recip$final_prep
    
    train_final<- model_data0 %>% 
      bake(object=final_prep)
    
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
    
    # xgb$plot(r_to_py(train_data %>%
    #                    select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>%
    #                    #filter(tx_Taxa=="Brook (speckled) Trout") %>%
    #                    as.data.frame()),
    #          parameter="concentration",
    #          feature="LDI_HAiFLO_mean",
    #          plot_type="Partial_Dependence")
    # 
    # xgb$plot(r_to_py(train_data %>%
    #                    select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>%
    #                    filter(tx_Taxa=="Brook (speckled) Trout") %>% 
    #                    as.data.frame()),
    #          parameter="rate",#concentration
    #          feature="LDI_HAiFLO_mean",
    #          plot_type="Partial_Dependence")
    
    # Calculate SHAP ---------------------------------------------------------
    # shap$initjs()
    # explainer = shap$TreeExplainer(xgb$booster)
    # shap_values = explainer(train_data %>% 
    #                           select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% 
    #                           as.data.frame() %>%
    #                           r_to_py())
    
    shap_pred<-xgb$booster$predict(train_final%>%
                                     select(-contains(c("resp","case"))) %>% 
                                     r_to_py(),
                                   start_iteration = -1L,
                                   pred_contrib = T)
    
    arg_nms<-xgb$dist$distribution_arg_names
    pred_nms<-colnames(train_final %>%
                         select(-contains(c("resp","case"))))
    
    col_index<-split(seq(1:((length(pred_nms)+1)*3)),rep(1:3, each=length(pred_nms)+1))
    names(col_index)<-arg_nms
    
    shap_pred_fin<-lapply(col_index,function(x) shap_pred[,x])
    shap_pred_fin<-lapply(shap_pred_fin,function(x) {colnames(x)<-c(pred_nms,"BIAS");x})
    
    shap_pred_fin$raw_data<-model_data0
    
    # shap_values_r = py_to_r(shap_values)
    # 
    # shap_values_r[,1][,0]
    # 
    saveRDS(shap_pred_fin,
            file.path("data","models","LSS",paste0("Shap_",ep,"_",booster,"_",dt,".rds")))
    
    #shap_values[,"tx_Taxa"]
    
  }
}
