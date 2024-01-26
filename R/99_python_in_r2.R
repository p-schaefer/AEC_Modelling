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

#lss.model <- import("lightgbmlss.model")
xgb.model <- import("xgboostlss.model")
#distr.lgb<-import("lightgbmlss.distributions")
distr.xgb<-import("xgboostlss.distributions")
#shap<-import("shap")

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
                              #-contains("lumped"),
                              #-contains("_iFL"),
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
  
  
  weight<-final_data %>% select(any_of("case_weight")) %>% unlist()
  
  train_py = xgb.model$DMatrix(data = final_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(), 
                               label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                               weight =weight %>% as.matrix() %>% r_to_py(),
                               enable_categorical=r_to_py(TRUE),
                               nthread=8L)
  
  
  if (F) { # distribution selection
    xgblss_dist_class =distr.xgb$distribution_utils$DistributionClass()
    candidate_distributions = list(distr.xgb$ZALN, distr.xgb$ZAGamma, distr.xgb$Gaussian,distr.xgb$Expectile)
    
    dist_nll = xgblss_dist_class$dist_select(target=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                             candidate_distributions=r_to_py(candidate_distributions),
                                             max_iter=100L, 
                                             plot=T, 
                                             figure_size=c(8, 5))
    dist_nll
    
  }
  # Prepare Parameters ------------------------------------------------------
  
  params_xgb = list(
    booster = list("categorical",list("gbtree")),
    # booster = list("categorical",list("dart")),
    # sample_type = list("categorical",list("weighted")),
    # rate_drop= list("float",list(low= 0, high=0.3, log=FALSE)), 
    # skip_drop= list("float",list(low= 0, high=0.3, log=FALSE)), 
    eta= list("float",list(low= 1e-12, high=1, log=TRUE)), 
    max_depth= list("int",list(low= 50L, high=2000L, log=FALSE)),
    # lambda = list("float",list(low= 1e-12, high=2.5, log=TRUE)), #these don't work for some reason
    # alpha = list("float",list(low= 1e-12, high=2.5, log=TRUE)),
    gamma = list("float",list(low= 1e-12, high=1, log=TRUE)),
    min_child_weight =list("float",list(low= 1e-12, high=100, log=TRUE)),
    subsample= list("float",list(low= 0.3, high=0.7, log=FALSE)),
    colsample_bynode = list("float",list(low= 0.3, high=0.7, log=FALSE)),
    max_cat_to_onehot = list("int",list(low= 2L, high=8L, log=FALSE)),
    max_cat_threshold = list("int",list(low= 200L, high=2000L, log=FALSE))
  )
  
  # Define Expectiles ------------------------------------------------------------
  
  xgb = xgb.model$XGBoostLSS(
    distr.xgb$ZAGamma$ZAGamma(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  
  # Models ------------------------------------------------------------------
  
  # Tune hyperparameters
  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_xgb),
                            dtrain=train_py,
                            num_boost_round=r_to_py(2000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(10L),   # Number of early-stopping rounds
                            max_minutes=r_to_py(60L*24L*2L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            silence=r_to_py(FALSE)
  )
  
  
  
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_",ep,"_gbtree.rds")))
  
  
  params_xgb = list(
    #booster = list("categorical",list("gbtree")),
    booster = list("categorical",list("dart")),
    sample_type = list("categorical",list("weighted")),
    rate_drop= list("float",list(low= 0, high=0.3, log=FALSE)),
    skip_drop= list("float",list(low= 0, high=0.3, log=FALSE)),
    eta= list("float",list(low= opt_param$eta*0.9,high=opt_param$eta*1.1, log=TRUE)), 
    max_depth= list("int",list(low= as.integer(opt_param$max_depth*0.9),high=as.integer(opt_param$max_depth*1.1), log=FALSE)),
    # lambda = list("float",list(low= 1e-12, high=2.5, log=TRUE)), #these don't work for some reason
    # alpha = list("float",list(low= 1e-12, high=2.5, log=TRUE)),
    gamma = list("float",list(low= opt_param$gamma*0.9,high=opt_param$gamma*1.1, log=TRUE)),
    min_child_weight =list("float",list(low= opt_param$min_child_weight*0.9,high=opt_param$min_child_weight*1.1, log=TRUE)),
    subsample= list("float",list(low= opt_param$subsample*0.9,high=opt_param$subsample*1.1, log=FALSE)),
    colsample_bynode = list("float",list(low= opt_param$colsample_bynode*0.9,high=opt_param$colsample_bynode*1.1, log=FALSE)),
    max_cat_to_onehot = list("int",list(low= as.integer(opt_param$max_cat_to_onehot*0.9),high=as.integer(opt_param$max_cat_to_onehot*1.1), log=FALSE)),
    max_cat_threshold = list("int",list(low= as.integer(opt_param$max_cat_threshold*0.9),high=as.integer(opt_param$max_cat_threshold*1.1), log=FALSE))
  )
  
  # Tune hyperparameters
  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_xgb),
                            dtrain=train_py,
                            num_boost_round=r_to_py(opt_param$opt_rounds+2000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(10L),   # Number of early-stopping rounds
                            max_minutes=r_to_py(60L*24L*2L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            silence=r_to_py(FALSE)
  )
  
  
  
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_",ep,"_dart.rds")))
  # validate CV -------------------------------------------------------------
  
  # Define Cross-Validation -------------------------------------------------
  
  if (T){
    opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_",ep,"_gbtree.rds")))
    
    cros_v<-group_vfold_cv(model_data,
                           "gen_ProvSegmentID",
                           20)
    
    out_res<-list()
    for (i in 1:nrow(cros_v)){
      
      trn<-training(cros_v$splits[[i]])
      
      final_prep<-prep(recip_main,trn)
      
      tst<-testing(cros_v$splits[[i]]) %>% 
        bake(object=final_prep) #%>% 
        #head(1)
      
      trn<-training(cros_v$splits[[i]]) %>% 
        bake(object=final_prep)
      
      train_py = xgb.model$DMatrix(data = trn %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(), 
                                   label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                   weight =trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                   enable_categorical=r_to_py(TRUE),
                                   nthread=8L)
      
      test_py = xgb.model$DMatrix(data = tst %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(), 
                                  label=tst %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                  weight =tst %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                  enable_categorical=r_to_py(TRUE),
                                  nthread=8L)
      
      
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
      pred_quantiles = xgb$predict(test_py,
                                   pred_type="quantiles",
                                   n_samples=2000L,
                                   quantiles=c(0.05,0.25,0.5,0.75,0.95))
      
      # xgb$predict(test_py, pred_type="parameters")
      # pred_quantiles = xgb$predict(test_py, pred_type="expectiles")
      
      out<-pred_quantiles %>% 
        rename_with(.cols=any_of(c("quant_0.5","expectile_0.5")),~paste0("Predicted")) %>% 
        bind_cols(tst %>%
                    select(any_of(ep),tx_Taxa,everything()) %>%
                    rename_with(.cols=any_of(ep),~paste0("Observed")) %>% 
                    mutate(endpoint=ep,
                           cv_fold=i)
        ) %>% 
        mutate(gen_ProvSegmentID=testing(cros_v$splits[[i]])$gen_ProvSegmentID)
      if (F){
        

        explainer = shap$TreeExplainer(xgb$booster)
        
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
        
        sumry<-out %>% 
          group_by(tx_Taxa,gen_ProvSegmentID) %>% 
          summarise(Observed=median(Observed),
                    Predicted=median(Predicted))
        
        m1<-sumry %>% 
          filter(tx_Taxa=="Brook (speckled) Trout") %>% 
          left_join(bind_rows(stm_lns),
                    by=c("gen_ProvSegmentID"="ProvSegmentID")) %>% 
          sf::st_as_sf()
        
        mapview::mapview(m1,zcol="Predicted")+mapview::mapview(m1,zcol="Observed")
        
      }
      
      out_res[[i]]<-out
      
    }
    
    out_res<-bind_rows(out_res)
    
    saveRDS(out_res,file.path("data","models","LSS",paste0("best_params_OOB_Pred_",ep,".rds")))
    
    
  }
  
}

