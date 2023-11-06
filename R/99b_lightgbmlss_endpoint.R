library(reticulate)
library(tidyverse)
library(tidymodels)

adj<-0.005


if (F) {
  # Setup python environment
  #install_miniconda()
  conda_create("AEC_Model", python_version="3.11")
  use_condaenv("AEC_Model")
  
  miniconda_update()
  
  conda_install("AEC_Model","IPython",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/XGBoostLSS.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/StatMixedML/LightGBMLSS.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/dsgibbons/shap.git",pip=T)
}

use_condaenv("AEC_Model")


lss.model <- import("lightgbmlss.model")
xgb.model <- import("xgboostlss.model")
distr.lgb<-import("lightgbmlss.distributions")
distr.xgb<-import("xgboostlss.distributions")


# Prepare Data ------------------------------------------------------------

model_data<-read_rds(file.path("data","final","Model_building_finalendpoint_data.rds"))

resp<-model_data %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[1:7]
#ep<-resp[[1]]
#resp<-tail(resp,4)

for (ep in rev(resp)) {
  model_data<-read_rds(file.path("data","final","Model_building_finalendpoint_data.rds")) %>% 
    filter(if_any(any_of(ep),~!is.na(.)))
  
  
  recip_main<-recipe(x=model_data %>%
                       select(-starts_with("gen_")) %>% 
                       select(starts_with(c(#"tx_",
                         "hb_",
                         "nr_")),
                         any_of(ep),
                         any_of("case_weight"))
  ) %>% 
    update_role(
      starts_with(c(#"tx_",
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
    step_dummy(all_factor_predictors()) %>% 
    step_mutate(across(starts_with(c("case_weight")),~as.numeric(.)))
  
  if (grepl("perc",ep)) {
    recip_main<-recip_main %>%
      step_mutate(across(any_of(!!ep),~car::logit(.,F,0.005)))
  } else {
      recip_main<-recip_main %>%
        step_mutate(across(any_of(!!ep),~log1p(.)))
  }
  
  
  final_data<-prep(recip_main,model_data) %>% juice()
  
  weight<-final_data %>% select(any_of("case_weight")) %>% unlist()
  #weight[]<-1
  
  train_py = xgb.model$DMatrix(data = final_data %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
                               label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                               weight =weight %>% as.matrix() %>% r_to_py(),
                               nthread=8L)
  
  # train_py<-lss.model$lgb$Dataset(
  #   data = final_data %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
  #   label = final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
  #   weight =  weight %>% as.matrix() %>% r_to_py(),
  #   categorical_feature = which(sapply(final_data,is.factor)) %>% r_to_py(),
  #   free_raw_data = r_to_py(FALSE)
  # )
  
  # dist_class = distr.xgb$distribution_utils$DistributionClass()
  # candidate_distributions = list(distr.xgb$Gaussian, 
  #                                distr.xgb$Gumbel,
  #                                distr.xgb$Laplace, 
  #                                #distr.xgb$Weibull,
  #                                distr.xgb$ZAGamma,
  #                                distr.xgb$ZALN,
  #                                distr.xgb$Expectile
  # )
  # 
  # dist_nll = dist_class$dist_select(target=final_data %>% select(any_of(ep)) %>% as.matrix()  %>% r_to_py(),
  #                                   candidate_distributions=r_to_py(candidate_distributions),
  #                                   max_iter=r_to_py(50L),
  #                                   plot=r_to_py(F))
  # sel_dist_nll<-dist_nll$distribution[1]
  
  # Prepare Parameters ------------------------------------------------------
  
  # params_lightgbm = list(boosting= list("categorical",list("gbdt")),
  #                        feature_pre_filter= list("categorical",list("false")),
  #                        eta= list("float",list(low= 1e-7, high=10, log=TRUE)), 
  #                        max_depth= list("int",list(low= 10L, high=1000L, log=FALSE)),
  #                        num_leaves= list("int",list(low= 124L, high=2056L, log=FALSE)),           
  #                        min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
  #                        # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
  #                        # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
  #                        min_gain_to_split= list("float",list(low= 1e-7, high=30, log=TRUE)),
  #                        #min_sum_hessian_in_leaf=list("float",list(low= 1e-7, high=300, log=TRUE)),
  #                        subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
  #                        #feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
  #                        feature_fraction_bynode = list("float",list(low= 0.2, high=0.8, log=FALSE)),
  #                        bagging_freq = list("int",list(low= 1L, high=10L, log=FALSE)),
  #                        bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
  #                        max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
  #                        max_cat_threshold = list("int",list(low= 124L, high=512L, log=FALSE)),
  #                        min_data_per_group = list("int",list(low= 5L, high=256L, log=FALSE)),
  #                        cat_smooth = list("float",list(low= 0L, high=50L, log=FALSE)),
  #                        cat_l2 = list("float",list(low= 0L, high=50L, log=FALSE)),
  #                        histogram_pool_size = list("int",list(low= 2000L, high=2000L, log=FALSE))
  #                        #device_type = "cpu",
  #                        #categorical_feature = paste0("name:",colnames(final_data %>% select(where(is.factor))))
  # )
  
  params_xgb = list(booster = list("categorical",list("gbtree")),
                    #feature_pre_filter= list("categorical",list("false")),
                    eta= list("float",list(low= 1e-7, high=1e-1, log=TRUE)), 
                    max_depth= list("int",list(low= 500L, high=5000L, log=FALSE)),
                    #num_leaves= list("int",list(low= 124L, high=2056L, log=FALSE)),           
                    #min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
                    # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
                    # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
                    gamma = list("float",list(low= 1e-10, high=10, log=TRUE)),
                    min_child_weight =list("float",list(low= 1e-10, high=10, log=TRUE)),
                    subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
                    #feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                    colsample_bynode = list("float",list(low= 0.2, high=0.8, log=FALSE))#,
                    #bagging_freq = list("int",list(low= 1L, high=10L, log=FALSE)),
                    #bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE))
  )
  
  # Define Expectiles ------------------------------------------------------------
  
  # xgb = xgb.model$XGBoostLSS(
  #   distr.xgb[[sel_dist_nll]][[sel_dist_nll]](
  #   )
  # )
  
  
  xgb = xgb.model$XGBoostLSS(
    distr.xgb$Expectile$Expectile(
      stabilization = "MAD",
      expectiles = r_to_py(c(0.05,0.25,0.5,0.75,0.95)),
      penalize_crossing=r_to_py(TRUE)
    )
  )
  
  # Models ------------------------------------------------------------------
  
  
  opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_xgb),
                            dtrain=train_py,
                            num_boost_round=r_to_py(2000L),        # Number of boosting iterations.
                            nfold=r_to_py(5L),                    # Number of cv-folds.
                            early_stopping_rounds=r_to_py(25L),   # Number of early-stopping rounds
                            #max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            max_minutes=r_to_py(60L*3L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                            #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
                            silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
                            #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
                            #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
  )
  
  
  
  saveRDS(opt_param,file.path("data","final",paste0("best_params_",ep,".rds")))
  
  
  
  # validate CV -------------------------------------------------------------
  
  # Define Cross-Validation -------------------------------------------------
  
  cros_v<-group_vfold_cv(model_data,
                         "gen_ProvSegmentID",
                         5)
  
  out_res<-list()
  for (i in 1:nrow(cros_v)){
    xgb = xgb.model$XGBoostLSS(
      distr.xgb$Expectile$Expectile(
        stabilization = "None",
        expectiles = r_to_py(c(0.05,0.5,0.95)),
        penalize_crossing=r_to_py(TRUE)
      )
    )
    
    # xgb = xgb.model$XGBoostLSS(
    #   distr.xgb[[sel_dist_nll]][[sel_dist_nll]](
    #   )
    # )
    
    # lgblss <- lss.model$LightGBMLSS(
    #   distr$Expectile$Expectile(
    #     stabilization=r_to_py("None"),
    #     expectiles = r_to_py(c( 0.05, 0.5, 0.95)),
    #     penalize_crossing = r_to_py(TRUE)
    #   )
    # )
    
    trn<-training(cros_v$splits[[i]])
    
    final_prep<-prep(recip_main,trn)
    
    tst<-testing(cros_v$splits[[i]]) %>% 
      bake(object=final_prep)
    
    trn<-juice(final_prep)
    

    train_py = xgb.model$DMatrix(data = trn %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
                                 label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                 weight =trn %>% select(any_of("case_weight")) %>% unlist() %>% as.matrix() %>% r_to_py(),
                                 nthread=8L)
   
    test_py = xgb.model$DMatrix(data = tst %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
                                 label=tst %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                                 weight =tst %>% select(any_of("case_weight")) %>% unlist() %>% as.matrix() %>% r_to_py(),
                                 nthread=8L)
    
    # train_py<-lss.model$lgb$Dataset(
    #   data = trn %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
    #   label = trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
    #   weight =  trn %>% select(any_of("case_weight")) %>% unlist() %>% as.matrix() %>% r_to_py()
    # )
    
    xgb$train(r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
              train_py,
              num_boost_round=opt_param$opt_rounds
    )
    
    # pred_expectile<-xgb$predict(test_py,
    #                              pred_type="quantiles",
    #                              n_samples=100L,
    #                              quantiles=c(0.05,0.25,0.5,0.75,0.95)) 
    
    pred_expectile = xgb$predict(test_py,
                                pred_type="expectiles")
    
    out<-pred_expectile %>% 
      bind_cols(
        tst %>% 
          select(any_of(ep),
                 #tx_Taxa
          )
      ) %>% 
      mutate(cv_fold=i)
    
    out_res[[i]]<-out
    
  }
  
  saveRDS(out_res,file.path("data","final",paste0("best_params_OOB_Pred_",ep,".rds")))
  
  gc()
  # out_res %>% 
  #   bind_rows() %>% 
  #   ggplot(aes(x=!!sym(ep),y=expectile_0.5))+
  #   geom_point()+
  #   geom_abline(intercept = 0,slope=1,size=0.25)
  
  
  
}

OOB<-tibble(fl=list.files(file.path("data","final"),"OOB")) %>% 
  mutate(data=map(fl,~readRDS(file.path("data","final",.x)))) %>% 
  mutate(Endpoint=case_when(
    grepl("Spec_BKT_Bioperc",fl) ~ "Brook Trout Percent Biomass",
    grepl("Spec_BKT_Abuperc",fl) ~ "Brook Trout Percent Abundance",
    grepl("Spec_RBD_Bioperc",fl) ~ "Rainbow Darter Percent Biomass",
    grepl("Spec_RBD_Abuperc",fl) ~ "Rainbow Darter Percent Abundance",
    grepl("Spec_BKT_Bio",fl) ~ "Brook Trout Total Biomass",
    grepl("Spec_BKT_Abu",fl) ~ "Brook Trout Total Abundance",
    grepl("Spec_RBD_Bio",fl) ~ "Rainbow Darter Total Biomass",
    grepl("Spec_RBD_Abu",fl) ~ "Rainbow Darter Total Abundance",
    grepl("Comm_Biomass",fl) ~ "Total Biomass",
    grepl("Comm_Abundance",fl) ~ "Total Abundance",
  )) %>% 
  mutate(plt=map2(data,Endpoint,
                 function(x,title){
                   #browser()
                   y<-bind_rows(x) %>% 
                     rename_with(.cols=contains("resp_"),~paste0("Obs")) %>% 
                     rename_with(.cols=contains("_0.5"),~paste0("Pred"))
                   
                   min_v<-min(c(min(y$Obs),min(y$Pred)))
                   max_v<-max(c(max(y$Obs),max(y$Pred)))
                   
                   ggplot(y,aes(y=Pred,x=Obs)) +
                     geom_point(size=0.25,alpha=0.5,colour="grey1")+
                     geom_smooth(method="lm",linewidth=0.25,colour="blue4")+
                     scale_x_continuous(trans="identity",limits=c(min_v,max_v))+
                     scale_y_continuous(trans="identity",limits=c(min_v,max_v))+
                     geom_abline(slope=1,intercept = 0)+
                     labs(x="Observed",y="Predicted",title=title)+
                     ggpubr::stat_cor(method="spearman",cor.coef.name="R2")+
                     theme_bw()
                 } 
                 ))

map2(OOB$plt,OOB$Endpoint,~ggsave(paste0(file.path("data","final",.y),".pdf"),
                                  .x,
                                  height=5,width=5,units="in"
                                  ))

# ggplot(bind_rows(a1) ,
#        aes(y=Pred,x=Obs))+
#   geom_point(size=0.25,alpha=0.5,colour="grey1")+
#   geom_smooth(method="lm",linewidth=0.25,colour="blue4")+
#   # scale_x_continuous(trans="log1p",limits=c(0,200))+
#   # scale_y_continuous(trans="log1p",limits=c(0,200))+
#   geom_abline(slope=1,intercept = 0)+
#   labs(x="Observed",y="Predicted",title="Total Community Biomass")+
#   ggpubr::stat_cor()+
#   theme_bw()



