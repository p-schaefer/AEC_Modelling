library(reticulate)
library(tidyverse)
library(tidymodels)

boosting_rounds<-1500L
n_folds<-5L

if (F) {
  # Setup python environment
  install_miniconda()
  conda_create("AEC_Model", python_version="3.11")
  use_condaenv("AEC_Model")
  
  miniconda_update()
  
  #conda_install("AEC_Model","git+https://github.com/StatMixedML/XGBoostLSS.git",pip=T)
  
  #conda_install("AEC_Model","git+https://github.com/StatMixedML/LightGBMLSS.git",pip=T)
  
  conda_install("AEC_Model","git+https://github.com/p-schaefer/LightGBMLSS.git",pip=T)
  
}

use_condaenv("AEC_Model")

# multiprocessing <- import("multiprocessing")
# 
# xgb_data <- import("xgboostlss.datasets.data_loader")
# xgb_model <- import("xgboostlss.model")
# xgb_distr<-import("xgboostlss.distributions.Expectile")
sel<-import("sklearn.model_selection")
numpy<-import("numpy")
lss.model <- import("lightgbmlss.model")
#xgb.model <- import("xgboostlss.model")
distr.lgb<-import("lightgbmlss.distributions")
#distr.xgb<-import("xgboostlss.distributions")
shap<-import("shap")

# Prepare Data ------------------------------------------------------------

taxa_keep<-readRDS(file.path("data","taxa_keep.rds"))
model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
  filter(tx_Taxa %in% taxa_keep$tx_Taxa) %>% 
  mutate(tx_Taxa = factor(tx_Taxa, levels=taxa_keep$tx_Taxa)) %>% 
  #filter(year(as.Date(gen_SampleDate))>1994)   %>% 
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
#ep<-resp[[2]]

for (ep in resp){
  print(ep)
  
  model_data <- model_data0 %>% 
    mutate(across(starts_with(c("case_weight")),~as.numeric(.))) %>% 
    group_by(gen_ProvReachID,gen_link_id,gen_StreamName,across(starts_with("tx_"))) %>% 
    summarise(across(where(is.numeric),~median(.x,na.rm=T)),
              across(!where(is.numeric),~tail(.x,1)),
              .groups="drop")
  
  recip_main<-recipe(x=model_data %>%
                       select(-starts_with("gen_")) %>% 
                       select(starts_with(c("tx_",
                                            "hb_",
                                            #"br_",
                                            "nr_",
                                            "LDI_"
                       )),
                       -starts_with("LDI_Natural"),
                       -contains("lat"),
                       -contains("lon"),
                       -contains("lumped"),
                       -contains("_SD"),
                       -contains("_iFL"),
                       -any_of(c("tx_Family","tx_Environment")),
                       any_of(ep),
                       any_of(paste0("cat_",ep)),
                       any_of("case_weight")) %>% 
                       mutate(across(any_of("case_weight"),~as.numeric(.x)))
  ) %>% 
    update_role(
      starts_with(c("tx_",
                    "hb_",
                    "br_",
                    "nr_",
                    "LDI_")),
      new_role = "predictor"
    ) %>% 
    step_zv(all_predictors()) %>% 
    step_nzv(all_predictors(),freq_cut = 500) %>% 
    step_lincomb(starts_with(c("br_","nr_","LDI_"))) %>%
    step_corr(method="spearman",threshold = 0.7) %>% 
    step_unorder(all_factor_predictors()) %>% 
    update_role(
      any_of(!!ep),
      new_role = "outcome"
    ) %>% 
    step_YeoJohnson(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_mutate(across(starts_with(c("case_weight")),~as.numeric(.))) #%>% 
  #themis::step_upsample(!!paste0("cat_",ep),over_ratio  = 0.1)
  
  final_prep<-prep(recip_main,
                   model_data )
  
  saveRDS(list(recip_main=recip_main,
               final_prep=final_prep),
          file.path("data","models","LSS",paste0("Final_Recipe_",ep,".rds")))
  
  final_data<- final_prep %>%
    juice()
  
  # cros_v<-group_vfold_cv(model_data,
  #                        "gen_ProvReachID",
  #                        6) %>% 
  #   tidy() %>% 
  #   group_by(Data) %>% 
  #   nest() %>% 
  #   summarise(x=map(data,~split(.x$Row,.x$Fold))) %>% 
  #   deframe()
  # 
  # cros_v<-cros_v[c("Analysis","Assessment")]
  # 
  # kf<-sel$GroupKFold(6L)
  # 
  # folds_generator <- py_run_string("folds = ((train_idx, test_idx) for train_idx, test_idx in kf.split(X,groups='gen_ProvReachID'))")
  # 
  # # define a generator function
  # cros_v_generator <-function(train_idx,test_idx) {
  #   value <- start
  #   function() {
  #     value <<- value + 1
  #     value
  #   }
  # }
  # 
  # # convert the function to a python iterator
  # iter <- py_iterator(sequence_generator(10))
  
  folds_object = sel$GroupKFold(n_folds)
  
  train_py = lss.model$Dataset(
    data=final_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>%   r_to_py(),
    label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
    group=table(as.numeric(model_data$gen_ProvReachID))
    #weight =final_data %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
  )
  
  # Define Model ------------------------------------------------------------
  
  xgb = lss.model$LightGBMLSS(
    distr.lgb$ZAGamma$ZAGamma(
      stabilization = "None",
      response_fn = "exp",
      loss_fn="nll"
    )
  )
  
  # Prepare Parameters ------------------------------------------------------
  params_lightgbm = list(
    boosting= list("categorical",list("dart")), #"gbdt",
    xgboost_dart_mode = list("categorical",list(T,F)),
    rate_drop= list("float",list(low= 0, high=0.5, log=FALSE)),
    skip_drop= list("float",list(low= 0.1, high=0.7, log=FALSE)),
    # data_sample_strategy = list("categorical",list("goss")), #this made fits worse
    # top_rate = list("float",list(low= 0.2, high=0.5, log=FALSE)),
    # other_rate = list("float",list(low= 0.1, high=0.4, log=FALSE)),
    feature_pre_filter= list("categorical",list(F)),
    learning_rate = list("float",list(low= 1e-4, high=3, log=TRUE)), 
    max_depth= list("int",list(low= 100L, high=2000L, log=FALSE)),
    num_leaves= list("int",list(low= 150L, high=25000L, log=FALSE)),           
    min_data_in_leaf= list("int",list(low= 5L, high=200L, log=FALSE)),       
    min_gain_to_split= list("float",list(low= 1e-4, high=9, log=TRUE)),
    min_sum_hessian_in_leaf =list("float",list(low= 1e-4, high=999, log=TRUE)),
    feature_fraction_bynode = list("float",list(low= 0.4, high=0.8, log=FALSE)),
    baging_freq=list("none",list(1L)),
    bin_construct_sample_cnt =list("none",list(2000000L)),
    bagging_fraction = list("float",list(low= 0.3, high=0.7, log=FALSE)),
    max_cat_to_onehot = list("int",list(low= 2L, high=10L, log=FALSE)),
    max_cat_threshold = list("int",list(low= 1500L, high=5000L, log=FALSE)),
    min_data_per_group = list("int",list(low= 2L, high=50L, log=FALSE)),
    cat_smooth = list("float",list(low= 0L, high=60L, log=FALSE)),
    cat_l2 = list("float",list(low= 1e-4, high=0.1, log=TRUE)),
    histogram_pool_size = list("none",list(-1L)),
    lambda_l1=list("float",list(low= 1e-4, high=0.1, log=TRUE)),
    lambda_l2=list("float",list(low= 1e-4, high=0.1, log=TRUE))
  )
  
  # Tune hyperparameters
  oo<-try(lm(xyz~0),silent=T)
  while(inherits(oo,"try-error")){
    oo<-try({
      dart_log <- py_capture_output({
        opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_lightgbm),
                                  train_set=train_py,
                                  num_boost_round=r_to_py(boosting_rounds),    # Number of boosting iterations.
                                  folds=folds_object,
                                  multivariate=r_to_py(FALSE),
                                  n_startup_trials=r_to_py(100L),
                                  nfold=r_to_py(n_folds),                    # Number of cv-folds.
                                  early_stopping_rounds=r_to_py(20L),   # Number of early-stopping rounds
                                  max_minutes=r_to_py(60L*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                                  n_trials=r_to_py(400L),
                                  silence=r_to_py(FALSE),
                                  seed=r_to_py(1234L),
                                  hp_seed=r_to_py(1234L)
        )
      })
    },silent=F)
  }
  
  write(dart_log,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart_log.txt")))
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart.rds"))) 
  
  opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart.rds"))) 
  
  gg<-gc()
  # Add linear_tree ----------------------------------------------------------------
  
  params_lightgbm = list(
    linear_tree= list("categorical",list(T)),
    linear_lambda= list("float",list(low= 1e-4, high=0.1, log=TRUE)),
    
    # boosting = list("none",list(opt_param$boosting)),
    # xgboost_dart_mode = list("none",list(opt_param$xgboost_dart_mode)),
    # rate_drop= list("none",list(opt_param$rate_drop)),
    # skip_drop= list("none",list(opt_param$skip_drop)),
    # max_drop= list("none",list(opt_param$max_drop)),
    # # data_sample_strategy = list("none",list(opt_param$data_sample_strategy)),
    # # top_rate = list("none",list(opt_param$top_rate)),
    # # other_rate = list("none",list(opt_param$other_rate)),
    # feature_pre_filter= list("none",list(opt_param$feature_pre_filter)),
    # learning_rate = list("none",list(opt_param$learning_rate)),
    # max_depth= list("none",list(opt_param$max_depth)),
    # num_leaves= list("none",list(opt_param$num_leaves)),
    # min_data_in_leaf= list("none",list(opt_param$min_data_in_leaf)),
    # min_gain_to_split= list("none",list(opt_param$min_gain_to_split)),
    # min_sum_hessian_in_leaf =list("none",list(opt_param$min_sum_hessian_in_leaf)),
    # feature_fraction_bynode = list("none",list(opt_param$feature_fraction_bynode)),
    # baging_freq=list("none",list(opt_param$baging_freq)),
    # bin_construct_sample_cnt =list("none",list(opt_param$bin_construct_sample_cnt)),
    # bagging_fraction = list("none",list(opt_param$bagging_fraction)),
    # max_cat_to_onehot = list("none",list(opt_param$max_cat_to_onehot)),
    # max_cat_threshold = list("none",list(opt_param$max_cat_threshold)),
    # min_data_per_group = list("none",list(opt_param$min_data_per_group)),
    # cat_smooth = list("none",list(opt_param$cat_smooth)),
    # cat_l2 = list("none",list(opt_param$cat_l2)),
    # histogram_pool_size = list("none",list(opt_param$histogram_pool_size)),
    # lambda_l1=list("none",list(opt_param$lambda_l1)),
    # lambda_l2=list("none",list(opt_param$lambda_l2))
    
    boosting= list("categorical",list("dart")), #"gbdt",
    xgboost_dart_mode = list("categorical",list(T,F)),
    rate_drop= list("float",list(low= 0, high=0.5, log=FALSE)),
    skip_drop= list("float",list(low= 0.1, high=0.7, log=FALSE)),
    # data_sample_strategy = list("categorical",list("goss")), #this made fits worse
    # top_rate = list("float",list(low= 0.2, high=0.5, log=FALSE)),
    # other_rate = list("float",list(low= 0.1, high=0.4, log=FALSE)),
    feature_pre_filter= list("categorical",list(F)),
    learning_rate = list("float",list(low= 1e-4, high=3, log=TRUE)), 
    max_depth= list("int",list(low= 100L, high=2000L, log=FALSE)),
    num_leaves= list("int",list(low= 150L, high=25000L, log=FALSE)),           
    min_data_in_leaf= list("int",list(low= 5L, high=200L, log=FALSE)),       
    min_gain_to_split= list("float",list(low= 1e-4, high=9, log=TRUE)),
    min_sum_hessian_in_leaf =list("float",list(low= 1e-4, high=999, log=TRUE)),
    feature_fraction_bynode = list("float",list(low= 0.4, high=0.8, log=FALSE)),
    baging_freq=list("none",list(1L)),
    bin_construct_sample_cnt =list("none",list(2000000L)),
    bagging_fraction = list("float",list(low= 0.3, high=0.7, log=FALSE)),
    max_cat_to_onehot = list("int",list(low= 2L, high=10L, log=FALSE)),
    max_cat_threshold = list("int",list(low= 1500L, high=5000L, log=FALSE)),
    min_data_per_group = list("int",list(low= 2L, high=50L, log=FALSE)),
    cat_smooth = list("float",list(low= 0L, high=60L, log=FALSE)),
    cat_l2 = list("float",list(low= 1e-4, high=0.1, log=TRUE)),
    histogram_pool_size = list("none",list(-1L)),
    lambda_l1=list("float",list(low= 1e-4, high=0.1, log=TRUE)),
    lambda_l2=list("float",list(low= 1e-4, high=0.1, log=TRUE))
  )
  
  # need to regenerate data when setting linear_tree
  train_py = lss.model$Dataset(
    data=final_data %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame() %>% r_to_py(),
    label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
    group=table(as.numeric(model_data$gen_ProvReachID))
    #weight =final_data %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py()
  )
  
  # Tune hyperparameters
  oo<-try(lm(xyz~0),silent=T)
  while(inherits(oo,"try-error")){
    oo<-try({
      linear_tree_log <- py_capture_output({
        opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_lightgbm),
                                  train_set=train_py,
                                  #num_boost_round=r_to_py(opt_param$opt_rounds + 1000L),        # Number of boosting iterations.
                                  num_boost_round=r_to_py(boosting_rounds + 500L),    # Number of boosting iterations.
                                  folds=folds_object,
                                  multivariate=r_to_py(FALSE),
                                  n_startup_trials=r_to_py(100L),
                                  # n_startup_trials=r_to_py(10L),
                                  nfold=r_to_py(n_folds),                    # Number of cv-folds.
                                  early_stopping_rounds=r_to_py(20L),   # Number of early-stopping rounds
                                  max_minutes=r_to_py(60L*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                                  n_trials=r_to_py(400L),
                                  silence=r_to_py(FALSE),
                                  seed=r_to_py(1234L),
                                  hp_seed=r_to_py(1234L)
        )
      })
    },silent=F)
  }
  
  
  write(linear_tree_log,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart_ltree_log.txt")))
  saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_dart_ltree.rds")))
  gg<-gc()
  
}

