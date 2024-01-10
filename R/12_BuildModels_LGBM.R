library(tidyverse)
library(tidymodels)
library(bonsai)
library(doParallel)

adj<-0.005

inv.logit <- function(f,a) {
  a <- (1-2*a)
  zapsmall((a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f))))
}

model_data<-read_rds(file.path("data","final","Model_building_finaltaxa_data.rds")) %>% 
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

#model_data<-ingredients::select_sample(model_data,2000,seed=1234)

for (equalization in c(0,-0.25,-0.5,-1,1,2,4)) {
  for (use_weights in c("wgtd","unwghtd")){
    for (ep in colnames(model_data)[grepl("^resp_",colnames(model_data))]) {
      for (ep_type in c("cat_","")){
        
        equalization_title<-case_when(
          equalization==0 ~ "_EqlzNONE",
          equalization==-0.25 ~ "_EqlzUP25",
          equalization==-0.5 ~ "_EqlzUP50",
          equalization==-1 ~ "_EqlzUP100",
          equalization==1 ~ "_EqlzDWN100",
          equalization==2 ~ "_EqlzDWN50",
          equalization==4 ~ "_EqlzDWN25",
        )
        
        ep2<-paste0(ep_type,ep)
        
        pred_var<-ep2
        
        if (ep_type=="") {
          ep3<-paste0("cat_",ep)
        } else {
          ep3<-ep2
        }

        out_file<-file.path("data","models","fits",paste0("Fit_",pred_var,"_",use_weights,equalization_title,".rds"))
        if (file.exists(out_file)) next()
        print(out_file)
        #stop()
        
        sub_data<-model_data %>% 
          filter(!if_any(any_of(ep),~is.na(.)))
        
        recip_main<-recipe(x=sub_data %>%
                             select(starts_with(c("tx_",
                                                  "hb_",
                                                  "nr_")),
                                    any_of(ep),
                                    any_of(ep2),
                                    any_of(ep3),
                                    any_of("case_weight"),
                                    -starts_with("gen_"),
                                    any_of("gen_ProvSegmentID")
                             ) 
                           
        ) %>% 
          update_role(
            starts_with(c("tx_",
                          "hb_",
                          "nr_",
                          "gen_ProvSegmentID"
            )),
            new_role = "predictor"
          ) %>% 
          step_nzv(all_predictors()) %>% 
          step_unorder(all_factor_predictors()) %>% 
          update_role(
            any_of(!!pred_var),
            new_role = "outcome"
          ) 
        
        if (equalization!=0){
          
          if (equalization<0){
            
            equalization_mod<-abs(equalization)
            recip_main<-recip_main %>% 
              themis::step_upsample(!!ep3,over_ratio  = equalization_mod)
            
          } else {
            equalization_mod<-abs(equalization)
            
            recip_main<-recip_main %>% 
              themis::step_downsample(!!ep3,under_ratio  = equalization_mod)
          }
        }
        
        final_data<-prep(recip_main,
                         sub_data) %>%
          juice()
        
        cros_v_1<-group_vfold_cv(sub_data, #group_vfold_cv
                                 group ="gen_ProvSegmentID",
                                 #strata = !!ep,
                                 v=5)
        
        cros_v_2<-group_vfold_cv(sub_data, #group_vfold_cv
                                 group ="gen_ProvSegmentID",
                                 #strata = !!ep,
                                 v=20)
        
        
        r_mod<-boost_tree(trees = 1000,
                          mtry = tune(),
                          min_n = tune(),
                          tree_depth = tune(),
                          learn_rate =tune(),
                          loss_reduction = tune(),
                          sample_size =tune(),
                          stop_iter = tune()) %>% 
          set_engine("lightgbm",
                     #bin_construct_sample_cnt = 1000000,
                     #feature_pre_filter ="false",
                     #first_metric_only = "true",
                     #metric ="quantile",
                     #alpha =tune(),
                     validation=0.3,
                     validation_groups="gen_ProvSegmentID",
                     rem_validation_groups=T,
                     num_threads = 32,#parallel::detectCores(logical = FALSE)-1,
                     boosting ="gbdt",#"dart",
                     num_leaves=tune(),
                     lambda_l1 = tune(),
                     lambda_l2 = tune(),
                     max_cat_to_onehot =tune(),
                     max_cat_threshold =tune(),
                     min_data_per_group=tune(),
                     cat_smooth =tune(),
                     cat_l2 =tune(),
                     min_sum_hessian_in_leaf = tune()
          )
        
        if (grepl("cat_",pred_var)){
          r_mod<-set_mode(r_mod,"classification")
        } else {
          r_mod<-set_mode(r_mod,"regression")
        }
        
        wflow <- workflow() %>% 
          add_model(r_mod) %>% 
          add_recipe(recip_main) 
        
        if (use_weights == "wgtd"){
          wflow <- wflow %>% 
            add_case_weights(case_weight)
        }
        
        tune_param <- r_mod %>% 
          hardhat::extract_parameter_set_dials() %>% 
          update(sample_size=sample_prop(range=c(0.3,0.7))) %>% 
          update(stop_iter=stop_iter(range=c(3,50))) %>%
          update(min_n=min_n(range=c(5,80))) %>%
          update(learn_rate=learn_rate(range=c(-6,2.5))) %>%
          update(loss_reduction=loss_reduction(range=c(-10,2.5))) %>%
          update(tree_depth=tree_depth(range=c(10L,500L))) %>%
          update(num_leaves=num_leaves(range=c(2^(6-1),2^(13-1)))) %>%
          update(lambda_l1=penalty_L1(range=c(-6,2.5))) %>%
          update(lambda_l2=penalty_L2(range=c(-6,2.5))) %>%
          #update(alpha=sample_prop(range=c(0.5,0.5))) %>% 
          update(cat_smooth=trees(range=c(0,100))) %>%
          update(max_cat_to_onehot=trees(range=c(2L,15L))) %>%
          update(max_cat_threshold=trees(range=c(256L,1024L))) %>%
          update(min_data_per_group=trees(range=c(4L,400L))) %>%
          update(cat_l2=penalty_L2(range=c(-6,2.5))) %>%
          update(min_sum_hessian_in_leaf=penalty_L2(range=c(-6,3))) %>% 
          update(mtry=mtry(range=c(5,ncol(final_data)-5)))
        
        tune_out0<-tune_grid(
          object=wflow,
          resamples=cros_v_1,
          param_info=tune_param,
          #metrics=yardstick::metric_set(mae,rmse,rsq),
          grid = 250,
          control=control_grid(
            verbose = F,
            save_pred = FALSE,
            save_workflow = FALSE
          )
        )
        
        
        saveRDS(tune_out0,file.path("data","models","tuning",paste0("Tune0_",pred_var,"_",use_weights,equalization_title,".rds")))
        
        # tune_out<-tune_bayes(
        #   object=wflow,
        #   resamples=cros_v_1,
        #   param_info=tune_param,
        #   #metrics=yardstick::metric_set(mae,rmse,rsq),
        #   initial= tune_out0,
        #   iter = 50,
        #   control=control_bayes(
        #     verbose = F,
        #     verbose_iter = T,
        #     uncertain = 8L,
        #     no_improve = 25L,
        #     save_pred = FALSE,
        #     time_limit = 60
        #   )
        # )
        
        tune_out<-finetune::tune_sim_anneal(
          object=wflow,
          resamples=cros_v_1,
          param_info=tune_param,
          #metrics=yardstick::metric_set(mae,rmse,rsq),
          initial= tune_out0,
          iter = 50,
          control=finetune::control_sim_anneal(
            verbose = F,
            verbose_iter = T,
            restart = 8L,
            no_improve = 25L,
            save_pred = FALSE,
            time_limit = 60
          )
        )
        
        
        
        saveRDS(tune_out,file.path("data","models","tuning",paste0("Tune1_",pred_var,"_",use_weights,equalization_title,".rds")))
        
        final_res <- fit_resamples(finalize_workflow(wflow,select_best(tune_out)),
                                   resamples=cros_v_2,
                                   control = control_resamples(save_pred = TRUE))
        
        final_pred<-collect_predictions(final_res) %>% 
          left_join(sub_data %>% 
                      select(everything(),-any_of(pred_var))%>% 
                      mutate(.row=row_number()),
                    by=".row") %>% 
          mutate(var_pred=pred_var)
        
        saveRDS(final_pred,out_file)
        
        rm(tune_out)
        rm(tune_out0)
        rm(final_pred)
        
        gg<-gc()
      }
    }
  }
}

if (F) {
  a1 %>%
    mutate(region=stringr::str_split(gen_ProvSegmentID,"\\.",n=2,simplify = T)[,1]) %>% 
    group_by(tx_Taxa
             #,region
    ) %>%
    summarize(roc=yardstick::roc_auc_vec(truth=`cat_resp_Perc_Abundance`,
                                         estimate=matrix(c(`.pred_0 - 1`,
                                                           `.pred_1 - 20`,
                                                           `.pred_20 - 40`,
                                                           `.pred_40 - 60`,
                                                           `.pred_60 - 80`,
                                                           `.pred_80 - 100`),
                                                         ncol=6)
    )) %>% View()
}

# # Define Recipe -----------------------------------------------------------
# 
# recip_main<-recipe(x=model_data) %>% 
#   update_role(
#     starts_with(c("tx_",
#                   "hb_",
#                   "nr_")),
#     new_role = "predictor"
#   ) %>% 
#   step_nzv(all_predictors()) %>% 
#   step_unorder(all_factor_predictors())
# 
# # Define Cross-Validation -------------------------------------------------
# 
# cros_v<-group_vfold_cv(model_data,
#                        "gen_ProvSegmentID",
#                        5)
# 
# # Define Model ------------------------------------------------------------
# 
# 
# r_mod<-boost_tree(mode="regression",
#                   learn_rate=tune(),
#                   mtry=tune(),
#                   trees=tune(),
#                   min_n=tune(),
#                   tree_depth=tune(),
#                   loss_reduction=tune()) %>% 
#   set_engine("xgboost",
#              #objective = "quantile",
#              #alpha = 0.5,
#              boosting = "gbdt",
#              num_threads = parallel::detectCores(logical = FALSE),
#              num_leaves = tune(),
#              bagging_fraction = tune(),
#              bagging_freq = 1,
#              early_stopping_round = 50,
#              max_cat_to_onehot= tune(),
#              min_data_per_group = tune(),
#              max_cat_threshold = tune(),
#              cat_l2 = tune(),
#              cat_smooth = tune()
#   )
# 
# 
# 
# # Tune Models -------------------------------------------------------------
# 
# resp<-model_data %>% select(starts_with("resp_")) %>% colnames()
# resp<-resp[grepl("Perc",resp)]
# resp<-resp[1]
# 
# for (ep in resp) {
#   
#   wflow <- workflow() %>% 
#     add_model(r_mod) %>% 
#     add_recipe(recip_main %>% 
#                  update_role(
#                    any_of(!!ep),
#                    new_role = "outcome"
#                  )%>% 
#                  step_naomit(any_of(!!ep)) %>% 
#                  step_naomit(c(everything(),-any_of("case_weight")))
#     ) %>% 
#     add_case_weights(case_weight)
#   
#   # all_cores <- parallel::detectCores(logical = FALSE)
#   # cl <- makePSOCKcluster(all_cores)
#   # registerDoParallel(cl)
#   
#   tune_param <- r_mod %>% 
#     hardhat::extract_parameter_set_dials() %>% 
#     update(
#       num_leaves = tune(),
#       bagging_fraction = tune(),
#       bagging_freq = 1,
#       early_stopping_round = 50,
#       max_cat_to_onehot= tune(),
#       min_data_per_group = tune(),
#       max_cat_threshold = tune(),
#       cat_l2 = tune(),
#       cat_smooth = tune()
#       
#       mtry = sample_prop(c(0.1,0.9)),
#       trees = trees(c(100000,1000000)), 
#       min_n = min_n(c(20,50)),
#       max.depth = tree_depth(c(100,100000)),
#       num.random.splits = num_random_splits(c(3,50)),
#       regularization.factor=regularization_factor(range = c(0, 1), trans = NULL),
#       regularization.usedepth=regularize_depth(values = c(TRUE, FALSE))
#     ) 
#   
#   tune_out<-tune_bayes(
#     object=wflow,
#     resamples=cros_v,
#     param_info=tune_param,
#     metrics=yardstick::metric_set(mae,rmse,rsq),
#     initial= 25,
#     iter = 50,
#     control=control_bayes(
#       verbose = T,
#       verbose_iter = T,
#       no_improve = 15L,
#       uncertain = 5,
#       save_pred = FALSE,
#       time_limit = 60,
#       parallel_over = "everything"
#     )
#   )
#   
#   # stopCluster(cl)
#   
#   saveRDS(tune_out,file.path("models",paste0("ModelTune_",ep,".csv")))
#   
# }
# 
# 
