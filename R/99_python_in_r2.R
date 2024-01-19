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

model_data0<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds"))%>% 
  mutate(across(contains("_Perc_"),~inv.logit(.,adj))) %>% 
  mutate(across(contains("_Comm_"),~expm1(.))) 

resp<-model_data0 %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[!grepl("Perc",resp)]
ep<-resp[[1]]

model_data <- model_data0

# model_data <- model_data0 %>% 
#   # mutate(across(any_of(resp),~case_when(
#   #   .==0 ~ 1e-4,
#   #   .==1 ~ 1-1e-4,
#   #   T ~ .
#   # ))) %>% 
#   select(-tx_Family:-tx_Repro_3) #%>% 
#   # pivot_wider(
#   #   names_from=tx_Taxa,
#   #   values_from=any_of(ep),
#   #   values_fill = 0
#   # )

recip_main<-recipe(x=model_data %>%
                     select(-starts_with("gen_")) %>% 
                     select(starts_with(c("tx_",
                                          "hb_",
                                          "nr_")),
                            any_of(ep),
                            any_of(unique(model_data0$tx_Taxa)),
                            any_of("case_weight"))
) %>% 
  update_role(
    starts_with(c("tx_",
                  "hb_",
                  "nr_")),
    new_role = "predictor"
  ) %>% 
  # update_role(
  #   any_of(levels(model_data0$tx_Taxa)),
  #   new_role = "response"
  # ) %>% 
  step_nzv(all_predictors()) %>% 
  step_unorder(all_factor_predictors()) %>% 
  update_role(
    any_of(!!ep),
    new_role = "outcome"
  ) %>% 
  #step_naomit(any_of(!!ep)) %>% 
  #step_mutate(across(starts_with(c("tx_")),~as.integer(.))) %>% 
  step_mutate(across(starts_with(c("case_weight")),~as.numeric(.)))
#step_mutate(across(starts_with("resp_Perc_"),~inv.logit(.,adj)))

final_data<-prep(recip_main,
                 model_data ) %>%
  juice()


#final_data[[ep]]<-as.integer(final_data[[ep]])

weight<-final_data %>% select(any_of("case_weight")) %>% unlist()
# weight<-round(weight/sum(weight)*10000000000,1)
# weight<-tibble(wt=weight) %>% 
#   group_by(wt) %>% 
#   mutate(wt2=paste0(wt,row_number())) %>% 
#   mutate(wt2=as.numeric(as.character(wt2)) )%>% 
#   mutate(wt2=floor(wt2)) %>% 
#   pull(wt2)
#weight[]<-1

# train_py<-lss.model$lgb$Dataset(
#   data = final_data %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
#   label = final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
#   weight =  weight %>% as.matrix() %>% r_to_py()#,
#   #feature_name = r_to_py(colnames(final_data %>% select(-any_of(ep)))),
#   #categorical_feature = r_to_py(which(colnames(final_data) %in% colnames(final_data %>% select(starts_with(c("tx_"))))))
#   #free_raw_data=r_to_py(FALSE)
# )

train_py = xgb.model$DMatrix(data = final_data %>% select(-any_of("case_weight"),-starts_with("resp_")) %>% as.data.frame() %>%   r_to_py(), 
                             label=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
                             #weight =weight %>% as.matrix() %>% r_to_py(),
                             enable_categorical=r_to_py(TRUE),
                             nthread=8L)

# bound = r_to_py(1)
# target_support = r_to_py("positive_integer")
# 
# candidate_flows = list(
#   distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=2L,  bound=bound, order="linear"),
#   distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=3L,  bound=bound, order="linear")#,
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=4L,  bound=bound, order="linear"),
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=6L,  bound=bound, order="linear")#,
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=8L,  bound=bound, order="linear"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=12L, bound=bound, order="linear"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=16L, bound=bound, order="linear")#,
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=20L, bound=bound, order="linear")#,
#   
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=2L,  bound=bound, order="quadratic"),
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=3L,  bound=bound, order="quadratic")#,
#   #distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=4L,  bound=bound, order="quadratic")#,
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=6L,  bound=bound, order="quadratic"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=8L,  bound=bound, order="quadratic"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=12L, bound=bound, order="quadratic"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=16L, bound=bound, order="quadratic"),
#   # distr.xgb$SplineFlow$SplineFlow(target_support=target_support, count_bins=20L, bound=bound, order="quadratic")
# )
# 
# flow_nll = distr.xgb$flow_utils$NormalizingFlowClass()
# 
# flow_nll<-flow_nll$flow_select(target=final_data %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
#                                candidate_flows=r_to_py(candidate_flows),
#                                max_iter=50L, n_samples=10000L, plot=r_to_py(TRUE),
#                                figure_size=r_to_py(c(12L, 5L)))
# flow_nll
# 
# dist_class = distr.xgb$distribution_utils$DistributionClass()
# candidate_distributions = list(distr.xgb$Gaussian,
#                                distr.xgb$Cauchy,
#                                distr.xgb$Gumbel,
#                                distr.xgb$Laplace,
#                                #distr.xgb$Weibull,
#                                #distr.xgb$ZAGamma,
#                                #distr.xgb$ZALN,
#                                distr.xgb$Expectile
# )
# 
# candidate_distributions = list(distr.xgb$ZIPoisson,
#                                distr.xgb$ZINB,
#                                distr.xgb$Poisson,
#                                distr.xgb$NegativeBinomial#,
#                                #distr.xgb$Expectile
# )
# 
# dist_nll = dist_class$dist_select(target=final_data %>% select(any_of(ep)) %>% as.matrix()  %>% r_to_py(),
#                                   candidate_distributions=r_to_py(candidate_distributions),
#                                   max_iter=r_to_py(50L),
#                                   plot=r_to_py(T))
# sel_dist_nll<-dist_nll$distribution[1]

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
                       eta= list("float",list(low= 1e-5, high=30, log=TRUE)), 
                       max_depth= list("int",list(low= 10L, high=1000L, log=FALSE)),
                       num_leaves= list("int",list(low= 100L, high=2000L, log=FALSE)),           
                       min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
                       # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
                       # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
                       min_gain_to_split= list("float",list(low= 1e-7, high=30, log=TRUE)),
                       #min_sum_hessian_in_leaf=list("float",list(low= 1e-7, high=300, log=TRUE)),
                       subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       #feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       feature_fraction_bynode = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       bagging_freq = list("int",list(low= 1L, high=1L, log=FALSE)),
                       bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                       max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
                       max_cat_threshold = list("int",list(low= 124L, high=512L, log=FALSE)),
                       min_data_per_group = list("int",list(low= 5L, high=256L, log=FALSE)),
                       cat_smooth = list("float",list(low= 0L, high=50L, log=FALSE)),
                       cat_l2 = list("float",list(low= 0L, high=50L, log=FALSE)),
                       histogram_pool_size = list("int",list(low= 2000L, high=2000L, log=FALSE))
                       #device_type = "cpu",
                       #categorical_feature = paste0("name:",colnames(final_data %>% select(where(is.factor))))
)

params_xgb = list(booster = list("categorical",list("gbtree")),
                  #tree_method = list("categorical",list("hist")),
                  #feature_pre_filter= list("categorical",list("false")),
                  eta= list("float",list(low= 1e-6, high=2.5, log=TRUE)), 
                  max_depth= list("int",list(low= 100L, high=1000L, log=FALSE)),
                  #num_leaves= list("int",list(low= 124L, high=2056L, log=FALSE)),           
                  #min_data_in_leaf= list("int",list(low= 5L, high=100L, log=FALSE)),       
                  # lambda_l1= list("float",list(low= 0, high=0, log=TRUE)),                
                  # lambda_l2= list("float",list(low= 0, high=0, log=TRUE)),                
                  gamma = list("float",list(low= 1e-6, high=2.5, log=TRUE)),
                  min_child_weight =list("float",list(low= 1e-6, high=2.5, log=TRUE)),
                  subsample= list("float",list(low= 0.2, high=0.8, log=FALSE)),
                  #feature_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                  colsample_bynode = list("float",list(low= 0.2, high=0.8, log=FALSE)),
                  max_cat_to_onehot = list("int",list(low= 4L, high=20L, log=FALSE)),
                  max_cat_threshold = list("int",list(low= 64L, high=512L, log=FALSE))
                  #bagging_freq = list("int",list(low= 1L, high=10L, log=FALSE)),
                  #bagging_fraction = list("float",list(low= 0.2, high=0.8, log=FALSE))
)

# Define Expectiles ------------------------------------------------------------

# lgblss = lss.model$LightGBMLSS(
#   distr$ZABeta$ZABeta(
#     stabilization = r_to_py('None'),
#     response_fn = r_to_py('exp'), 
#     loss_fn = r_to_py('nll')
#   )
#   # distr$Expectile$Expectile(
#   #   stabilization=r_to_py("None"),              # Options are "None", "MAD", "L2".
#   #   expectiles = r_to_py(c( 0.05, 0.5, 0.95)),         # List of expectiles to be estimated, in increasing order.
#   #   penalize_crossing = r_to_py(TRUE)           # Whether to include a penalty term to discourage crossing of expectiles.
#   # )
# )

# lgblss2 = lss.model$LightGBMLSS(
#   distr$Beta$Beta(
#     stabilization=r_to_py("MAD"),              # Options are "None", "MAD", "L2".
#     response_fn = r_to_py("exp")           # Whether to include a penalty term to discourage crossing of expectiles.
#   )
# )


# xgb = xgb.model$XGBoostLSS(
#   distr.xgb$Expectile$Expectile(
#     stabilization = "MAD",
#     expectiles = r_to_py(c(0.10,0.5,0.90)),
#     penalize_crossing=r_to_py(TRUE)
#   )
# )

# xgb = xgb.model$XGBoostLSS(
#   distr.xgb$Poisson$Poisson(
#     stabilization = "None",
#     response_fn = "softplus",
#     loss_fn="nll"
#   )
# )

xgb = xgb.model$XGBoostLSS(
  distr.xgb$ZALN$ZALN(
    stabilization = "MAD",
    response_fn = "exp",
    loss_fn="nll"
  )
)

# xgb = xgb.model$XGBoostLSS(
#   distr.xgb$SplineFlow$SplineFlow(
#     target_support="unit_interval",     # Specifies the support of the target. Options are "real", "positive", "positive_integer" or "unit_interval"
#     count_bins=12L,              # The number of segments comprising the spline.
#     bound=1,               # By adjusting the value, you can control the size of the bounding box and consequently control the range of inputs that the spline transform operates on.
#     order="linear",            # The order of the spline. Options are "linear" or "quadratic".
#     stabilization="None",      # Options are "None", "MAD" or "L2".
#     loss_fn="nll"
#   )
# )
# Models ------------------------------------------------------------------


# opt_param = lgblss$hyper_opt(hp_dict=r_to_py(params_lightgbm),
#                              train_set=train_py,
#                              num_boost_round=r_to_py(10000L),        # Number of boosting iterations.
#                              nfold=r_to_py(5L),                    # Number of cv-folds.
#                              early_stopping_rounds=r_to_py(15L),   # Number of early-stopping rounds
#                              max_minutes=r_to_py(60*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
#                              #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
#                              silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
#                              #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
#                              #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
# )

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

opt_param = xgb$hyper_opt(hp_dict=r_to_py(params_xgb),
                          dtrain=train_py,
                          num_boost_round=r_to_py(5000L),        # Number of boosting iterations.
                          nfold=r_to_py(5L),                    # Number of cv-folds.
                          early_stopping_rounds=r_to_py(15L),   # Number of early-stopping rounds
                          #max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                          max_minutes=r_to_py(60L*24L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
                          #n_trials=r_to_py("None"),              # The number of trials. If this argument is set to None, there is no limitation on the number of trials.
                          silence=r_to_py(FALSE)#,              # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
                          #seed=r_to_py(123L),                   # Seed used to generate cv-folds.
                          #hp_seed=r_to_py("None")                # Seed for random number generator used in the Bayesian hyperparameter search.
)



saveRDS(opt_param,file.path("data","models","LSS",paste0("best_params_",ep,".rds")))
#saveRDS(opt_param,paste0("best_params_",ep,".rds"))


# opt_param = xgblss$hyper_opt(hp_dict=r_to_py(params_xgboost),
#                              dtrain=train_py_xgb,
#                              num_boost_round=r_to_py(100L),        # Number of boosting iterations.
#                              nfold=r_to_py(5L),                    # Number of cv-folds.
#                              early_stopping_rounds=r_to_py(20L),   # Number of early-stopping rounds
#                              max_minutes=r_to_py(10L),             # Time budget in minutes, i.e., stop study after the given number of minutes.
#                              silence=r_to_py(FALSE)                # Controls the verbosity of the trail, i.e., user can silence the outputs of the trail.
# )



# validate CV -------------------------------------------------------------

# Define Cross-Validation -------------------------------------------------

if (F){
  cros_v<-group_vfold_cv(model_data,
                         "gen_ProvSegmentID",
                         5)
  
  out_res<-list()
  for (i in 1:nrow(cros_v)){
    
    # xgb = xgb.model$XGBoostLSS(
    #   distr.xgb$Poisson$Poisson(
    #     stabilization = "None",
    #     response_fn = "exp",
    #     loss_fn="nll"
    #   )
    # )
    
    xgb = xgb.model$XGBoostLSS(
      distr.xgb$Dirichlet$Dirichlet(
        D = length(levels(model_data0$tx_Taxa)),
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
    
    train_py = xgb.model$DMatrix(data = trn %>% select(-any_of(levels(model_data0$tx_Taxa)),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
                                 label=trn %>% select(any_of(levels(model_data0$tx_Taxa))) %>% as.matrix() %>% r_to_py(), 
                                 weight =trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                 enable_categorical=r_to_py(TRUE),
                                 nthread=8L)
    
    test_py = xgb.model$DMatrix(data = tst %>% select(-any_of(levels(model_data0$tx_Taxa)),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
                                label=tst %>% select(any_of(levels(model_data0$tx_Taxa))) %>% as.matrix() %>% r_to_py(), 
                                weight =tst %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
                                enable_categorical=r_to_py(TRUE),
                                nthread=8L)
    
    # train_py = xgb.model$DMatrix(data = trn %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
    #                              label=trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
    #                              weight =trn %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
    #                              enable_categorical=r_to_py(TRUE),
    #                              nthread=8L)
    # 
    # test_py = xgb.model$DMatrix(data = tst %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(), 
    #                             label=tst %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(), 
    #                             weight =tst %>% select(any_of("case_weight")) %>% as.matrix() %>% r_to_py(),
    #                             enable_categorical=r_to_py(TRUE),
    #                             nthread=8L)
    
    xgb$train(
      r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
      train_py,
      num_boost_round=opt_param$opt_rounds
    )
    
    pred_quantiles = xgb$predict(test_py,
                                 pred_type="quantiles",
                                 n_samples=1000L,
                                 quantiles=r_to_py(c(0.05,0.5,0.95))) #%>% 
    # bind_cols(
    #   tst %>%
    #     select(any_of(ep),
    #            tx_Taxa
    #     )
    # ) 
    
    cn<-colnames(trn %>% select(any_of(levels(model_data0$tx_Taxa))))
    
    out<-pred_quantiles %>% 
      group_by(target) %>% 
      mutate(id=row_number()) %>% 
      select(id,target,quant_0.5) %>% 
      pivot_wider(names_from=target,
                  values_from=quant_0.5) %>% 
      select(-id) %>% 
      setNames(paste("Predicted", cn)) %>% 
      bind_cols(tst) %>% 
      select(contains(cn),everything())
    
    
    
    # lgblss$train(r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
    #              train_py,
    #              num_boost_round=opt_param$opt_rounds
    # )
    
    
    # train_py<-lss.model$lgb$Dataset(
    #   data = trn %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
    #   label = trn %>% select(any_of(ep)) %>% as.matrix() %>% r_to_py(),
    #   weight =  trn %>% select(any_of("case_weight")) %>% unlist() %>% as.matrix() %>% r_to_py()
    # )
    # 
    # lgblss$train(r_to_py(opt_param[names(opt_param)!="opt_rounds"]),
    #              train_py,
    #              num_boost_round=opt_param$opt_rounds
    # )
    # 
    # pred_expectile = lgblss$predict(tst %>% select(-any_of(ep),-any_of("case_weight")) %>% as.data.frame() %>%   r_to_py(),
    #                                 pred_type="expectiles")
    
    # out<-pred_expectile %>% 
    #   bind_cols(
    #     tst %>% 
    #       select(any_of(ep),
    #              tx_Taxa
    #              )
    #   ) %>% 
    #   mutate(cv_fold=i)
    
    out_res[[i]]<-out
    
  }
  
  saveRDS(out_res,file.path("data","final",paste0("best_params_OOB_Pred_",ep,".rds")))
  
  # out_res %>% 
  #   bind_rows() %>% 
  #   ggplot(aes(x=resp_Perc_Biomass,y=expectile_0.5))+
  #   geom_point()+
  #   geom_abline(intercept = 0,slope=1,size=0.25)+
  #   facet_wrap(~tx_Taxa)
  
  
  
}
