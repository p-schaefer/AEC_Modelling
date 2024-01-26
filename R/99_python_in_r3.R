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
  
  conda_install("AEC_Model","shap",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/dsgibbons/shap.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/XGBoostLSS.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/LightGBMLSS.git",pip=T)
  
}

use_condaenv("AEC_Model")

# multiprocessing <- import("multiprocessing")
# 
# xgb_data <- import("xgboostlss.datasets.data_loader")
# xgb_model <- import("xgboostlss.model")
# xgb_distr<-import("xgboostlss.distributions.Expectile")

lss.model <- import("lightgbmlss.model")
#xgb.model <- import("xgboostlss.model")
distr.lgb<-import("lightgbmlss.distributions")
#distr.xgb<-import("xgboostlss.distributions")
shap<-import("shap")

# Prepare Data ------------------------------------------------------------

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(year(as.Date(gen_SampleDate))>1994)   %>% 
  # mutate(across(contains("_Perc_"),~inv.logit(.,adj))) %>%
  # mutate(across(contains("_Comm_"),~expm1(.))) %>% 
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
#ep<-resp[[1]]


for (ep in resp){
  
  model_data <- model_data0
  
  recip_main<-recipe(x=model_data %>%
                       select(-starts_with("gen_")) %>% 
                       select(starts_with(c("tx_",
                                            "hb_",
                                            "nr_",
                                            "LDI_")),
                              -starts_with("LDI_Natural"),
                              -contains("lumped"),
                              -contains("_iFL"),
                              any_of(ep),
                              any_of(paste0("cat_",ep)),
                              any_of("case_weight")) %>% 
                       mutate(across(any_of("case_weight"),~as.numeric(.x)))
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
    #step_normalize(all_numeric_predictors()) %>% 
    step_mutate(across(starts_with(c("case_weight")),~as.numeric(.))) %>% 
    themis::step_upsample(!!paste0("cat_",ep),over_ratio  = 0.25)
  
  final_prep<-prep(recip_main,
                   model_data )
  
  final_data<- final_prep %>%
    juice()
  
  
  train_py = lss.model$Dataset(
    data=final_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
    label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
    weight =final_data %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
  )
  
  
  # Prepare Parameters ------------------------------------------------------
  
  params_lightgbm = list(boosting= list("categorical",list("gbdt")),
                         feature_pre_filter= list("categorical",list("false")),
                         learning_rate = list("float",list(low= 1e-12, high=30, log=TRUE)), 
                         max_depth= list("int",list(low= 100L, high=5000L, log=FALSE)),
                         num_leaves= list("int",list(low= 100L, high=25000L, log=FALSE)),           
                         min_data_in_leaf= list("int",list(low= 1L, high=100L, log=FALSE)),       
                         min_gain_to_split= list("float",list(low= 1e-12, high=30, log=TRUE)),
                         subsample= list("float",list(low= 0.3, high=0.7, log=FALSE)),
                         feature_fraction_bynode = list("float",list(low= 0.3, high=0.7, log=FALSE)),
                         baging_freq=list("none",list(1)),
                         bagging_fraction = list("float",list(low= 0.3, high=0.7, log=FALSE)),
                         max_cat_to_onehot = list("int",list(low= 2L, high=20L, log=FALSE)),
                         max_cat_threshold = list("int",list(low= 124L, high=2500L, log=FALSE)),
                         min_data_per_group = list("int",list(low= 2L, high=256L, log=FALSE)),
                         cat_smooth = list("float",list(low= 0L, high=20L, log=FALSE)),
                         cat_l2 = list("float",list(low= 0L, high=20L, log=FALSE)),
                         histogram_pool_size = list("none",list(210000L))
  )
  
  # Define Expectiles ------------------------------------------------------------
  
  xgb = lss.model$LightGBMLSS(
    distr.lgb$ZAGamma$ZAGamma(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  

  # Models ------------------------------------------------------------------
  
  # Tune hyperparameters
  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_lightgbm),
                            train_set=train_py,
                            num_boost_round=r_to_py(2000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(10L),   # Number of early-stopping rounds
                            max_minutes=r_to_py(60L*24L*1L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            silence=r_to_py(FALSE)
  )
  
  
  
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_gbtree.rds")))
  
  
  params_xgb = list(
    #booster = list("categorical",list("gbtree")),
    booster = list("categorical",list("dart")),
    sample_type = list("categorical",list("weighted")),
    rate_drop= list("float",list(low= 0, high=0.3, log=FALSE)),
    skip_drop= list("float",list(low= 0, high=0.3, log=FALSE)),
    boosting= list("categorical",list("gbdt")),
    feature_pre_filter= list("categorical",list("false")),
    learning_rate = list("none",list(opt_param$learning_rate)), 
    max_depth= list("none",list(opt_param$max_depth)),
    num_leaves= list("none",list(opt_param$num_leaves)),           
    min_data_in_leaf= list("none",list(opt_param$min_data_in_leaf)),       
    min_gain_to_split= list("none",list(opt_param$min_gain_to_split)),
    subsample= list("none",list(opt_param$subsample)),
    feature_fraction_bynode = list("none",list(opt_param$feature_fraction_bynode)),
    baging_freq=list("none",list(1)),
    bagging_fraction = list("none",list(opt_param$bagging_fraction)),
    max_cat_to_onehot = list("none",list(opt_param$max_cat_to_onehot)),
    max_cat_threshold = list("none",list(opt_param$max_cat_threshold)),
    min_data_per_group = list("none",list(opt_param$min_data_per_group)),
    cat_smooth = list("none",list(opt_param$cat_smooth)),
    cat_l2 = list("none",list(opt_param$cat_l2)),
    histogram_pool_size = list("none",list(210000L))
  )
  
  # Tune hyperparameters
  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_lightgbm),
                            train_set=train_py,
                            num_boost_round=r_to_py(opt_param$opt_rounds + 2000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(10L),   # Number of early-stopping rounds
                            max_minutes=r_to_py(60L*24L*0.5L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            silence=r_to_py(FALSE)
  )
  
  
  
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart.rds"))) 
  
  # validate CV -------------------------------------------------------------
  
  # Define Cross-Validation -------------------------------------------------
  
  if (T){
    
    for (ii in c("gbtree","dart")){
      opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_",ii,".rds")))
      
      cros_v<-group_vfold_cv(model_data,
                             "gen_ProvSegmentID",
                             20)
      
      out_res<-list()
      for (i in 1:nrow(cros_v)){
        xgb = lss.model$LightGBMLSS(
          distr.lgb$ZAGamma$ZAGamma(
            stabilization = "None",
            response_fn = "exp",
            loss_fn="nll"
          )
        )
        
        trn<-training(cros_v$splits[[i]])
        
        final_prep<-prep(recip_main,trn)
        
        tst<-testing(cros_v$splits[[i]]) %>% 
          bake(object=final_prep) #%>% 
        #head(1)
        
        trn<-training(cros_v$splits[[i]]) %>% 
          bake(object=final_prep)
        
        train_py = lss.model$Dataset(
          data=trn %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
          label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
          weight = trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
        )
        
        test_py = lss.model$Dataset(
          data=tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
          label=tst %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
          weight = tst %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
        )
        
        
        xgb$train(
          r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
          train_py,
          num_boost_round=opt_param$opt_rounds
        )
        
        # xgb$plot(test_py,
        #          parameter="concentration",
        #          feature="LDI_HAiFLO_mean",
        #          plot_type="Partial_Dependence"
        #          
        #          )
        
        # Calculate quantiles from predicted distribution
        pred_quantiles = xgb$predict(tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
                                     pred_type="quantiles",
                                     n_samples=2000L,
                                     quantiles=c(0.05,0.25,0.5,0.75,0.95))
        
        # xgb$predict(test_py, pred_type="parameters")
        # pred_quantiles = xgb$predict(test_py, pred_type="expectiles")
        
        out<-pred_quantiles %>% 
          rename_with(.cols=any_of(c("quant_0.5","expectile_0.5")),~paste0("Predicted")) %>% 
          bind_cols(testing(cros_v$splits[[i]]) %>%
                      select(any_of(ep),tx_Taxa,everything()) %>%
                      rename_with(.cols=any_of(ep),~paste0("Observed")) %>% 
                      mutate(endpoint=ep,
                             cv_fold=i)
          ) %>% 
          mutate(gen_ProvSegmentID=testing(cros_v$splits[[i]])$gen_ProvSegmentID)
        
        if (F){
          
          
          #explainer = shap$TreeExplainer(xgb$booster)
          
          # xgb$plot(
          #   #train_py,
          #   tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
          #   parameter="concentration",
          #   plot_type="Feature_Importance"
          # )
          
          out %>% 
            mutate(in_90=Observed<=quant_0.95 & Observed>=quant_0.05) %>% 
            select(in_90) %>% 
            summarise(in_90=sum(in_90)/length(in_90))
          
          ggplot(out,aes(x=Observed,y=Predicted,colour=tx_Taxa))+
            geom_point()+
            geom_abline(slope=1,intercept=0)+
            geom_smooth(aes(x=Observed,y=Predicted),se=F,method="gam",colour="black")+
            geom_smooth(aes(x=Observed,y=quant_0.95),se=F,method="gam",colour="blue")+
            geom_smooth(aes(x=Observed,y=quant_0.05),se=F,method="gam",colour="blue")
          
          rsq_vec(out$Observed,out$Predicted)
          rmse_vec(out$Observed,out$Predicted)
          
          # sumry<-out %>% 
          #   group_by(tx_Taxa,gen_ProvSegmentID) %>% 
          #   summarise(Observed=median(Observed),
          #             Predicted=median(Predicted))
          # 
          # m1<-sumry %>% 
          #   filter(tx_Taxa=="Brook (speckled) Trout") %>% 
          #   left_join(bind_rows(stm_lns),
          #             by=c("gen_ProvSegmentID"="ProvSegmentID")) %>% 
          #   sf::st_as_sf()
          # 
          # mapview::mapview(m1,zcol="Predicted")+mapview::mapview(m1,zcol="Observed")
          
        }
        
        out_res[[i]]<-out
        
      }
      
      out_res<-bind_rows(out_res)
      
      saveRDS(out_res,file.path("data","models","LSS",paste0("best_params_OOB_Pred_",ep,"_",ii,".rds")))
      
      xgb = lss.model$LightGBMLSS(
        distr.lgb$ZAGamma$ZAGamma(
          stabilization = "None",
          response_fn = "exp",
          loss_fn="nll"
        )
      )
      
      train_py = lss.model$Dataset(
        data=final_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
        label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
        weight = final_data %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
      )
      
      xgb$train(
        r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
        train_py,
        num_boost_round=opt_param$opt_rounds
      )
      
      shap$initjs()
      explainer = shap$TreeExplainer(xgb$booster)
      shap_values = explainer(final_data %>% 
                                select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% 
                                as.data.frame() %>%
                                r_to_py())
      
      #shap_values$base_values[,1]+shap_values$values[,,1]
      #shap$plots$scatter(shap_values[,32,1],color=shap_values[,32,1])
      
      saveRDS(shap_values,file.path("data","models","LSS",paste0("best_params_SHAP_",ep,"_",ii,".rds")))
      
      shap_values2 = xgb$booster$predict(final_data %>% 
                                           select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% 
                                           as.data.frame() %>%
                                           r_to_py(),
                                         num_iteration =-1L,
                                         pred_contrib =T
                                         )
      
      saveRDS(shap_values2,file.path("data","models","LSS",paste0("best_params_SHAP2_",ep,"_",ii,".rds")))

      shap_values_interact<-explainer$shap_interaction_values(final_data %>% 
                                          select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% 
                                          as.data.frame() %>%
                                          r_to_py()
                                          )
      saveRDS(shap_values_interact,file.path("data","models","LSS",paste0("best_params_SHAPinteraction_",ep,"_",ii,".rds")))

    }
  }
}

