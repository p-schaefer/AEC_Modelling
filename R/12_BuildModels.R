library(tidyverse)
library(tidymodels)
library(ranger)
library(doParallel)

tx_data<-read_csv(file.path("data","final","Model_building_taxa_data.csv"))

inv.logit <- function(f,a) {
  a <- (1-2*a)
  zapsmall((a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f))))
}

# Finalize Model Data -----------------------------------------------------

model_data<-tx_data %>% 
  filter(!is.na(`name to use`)) %>% 
  filter(!is.na(ProvSegmentID)) %>% 
  mutate(
    tx_Repro_1=str_split(`Reproductive Guild`,": ",simplify = T)[,1],
    tx_Repro_2=str_split(`Reproductive Guild`,": ",simplify = T)[,2],
    tx_Repro_3=str_split(`Reproductive Guild`,": ",simplify = T)[,3],
  ) %>% 
  select(# Taxa Info
    gen_ProvSegmentID=ProvSegmentID,
    gen_link_id=link_id,
    gen_SampleEventID=SampleEventID,
    gen_StreamName=StreamName,
    gen_SampleDate=SampleDate,
    # Responses
    resp_Comm_Biomass=Comm_Biomass,
    resp_Comm_Abundance=Comm_Abundance,
    resp_Perc_Biomass=Perc_Biomass,
    resp_Perc_Abundance=Perc_Abundance,
    # Taxa Data
    tx_Taxa=`name to use`,
    tx_Family=Family,
    tx_Tolerance=Tolerance,
    tx_Trophic_Class=`Trophic Class`,
    tx_Thermal_Regime=`Thermal Regime`,
    tx_General_Habitat=`General Habitat(s)`,
    tx_Environment=Environment,
    tx_Repro_1,
    tx_Repro_2,
    tx_Repro_3,
    # Habitat
    hb_Temperature_Class=Temperature_Class,
    hb_Temperature=Temperature_30yr_MeanJuly,
    hh_GDDair_UpstreamCatchmentMean=GDDair_UpstreamCatchmentMean,
    hb_Turbidity=Turbidity_percUpstreamChannel_TurbGeo,
    hb_Slope=Slope_ReachChannel_Percent,
    hb_BFI_RCA=BFI_RCA,
    hb_BFI_UCA=BFI_UCA,
    hb_UCA=Upstream_Catchment_Area,
    hb_Lake_Inf=Lake_Influence_Code,
    hb_Wadeability=Wadeability,
    # Stressor
    starts_with("nr_")
  ) %>% 
  group_by(gen_ProvSegmentID) %>% 
  mutate(case_weight=importance_weights(1/n())) %>% 
  ungroup() %>% 
  select(case_weight,everything()) %>% 
  mutate(
    resp_Comm_Biomass=log1p(resp_Comm_Biomass),
    resp_Comm_Abundance=log1p(resp_Comm_Abundance),
    resp_Perc_Biomass=car::logit(resp_Perc_Biomass,F,0.005),
    resp_Perc_Abundance=car::logit(resp_Perc_Abundance,F,0.005)
  ) %>% 
  drop_na()

taxa_prop <- model_data %>% 
  group_by(tx_Taxa) %>% 
  summarise(
    mean_biomass_perc=inv.logit(mean(resp_Perc_Biomass,na.rm=T),0.005)*100,
    mean_abund_perc=inv.logit(mean(resp_Perc_Abundance,na.rm=T),0.005)*100
  ) %>% 
  arrange(desc(mean_abund_perc)) %>% 
  filter(mean_abund_perc>0.01)

model_data<-model_data %>% 
  filter(tx_Taxa %in% taxa_prop$tx_Taxa) %>% 
  mutate(across(where(is.character),~factor(.))) 


# Define Cross-Validation -------------------------------------------------

cros_v<-group_vfold_cv(model_data,
                       "gen_ProvSegmentID",
                       10)

# Define Recipe -----------------------------------------------------------

recip_main<-recipe(x=model_data) %>% 
  update_role(
    starts_with(c("tx_",
                  "hb_",
                  "nr_")),
    new_role = "predictor"
  ) %>% 
  step_nzv(all_predictors()) %>% 
  step_unorder(all_factor_predictors())


# Define Model ------------------------------------------------------------

r_mod<-rand_forest(mode="regression",
                   mtry=tune(),
                   trees=tune(),
                   min_n=tune()) %>% 
  set_engine("ranger",
             replace=FALSE,
             max.depth = tune(),
             splitrule = "extratrees",
             num.random.splits = tune(),
             regularization.factor = tune(),
             regularization.usedepth = tune(),
             always.split.variables = model_data %>% select(starts_with("tx_")) %>% colnames(),
             quantreg = TRUE,
             keep.inbag = TRUE,
             oob.error = FALSE,
             num.threads=8,
             respect.unordered.factors="partition"
  )


# Set Tuning Range --------------------------------------------------------

tune_param <- r_mod %>% 
  hardhat::extract_parameter_set_dials() %>% 
  update(
    mtry = sample_prop(c(0.1,0.9)),
    trees = trees(c(500,10000)), 
    min_n = min_n(c(2,50)),
    max.depth = tree_depth(c(20,100000)),
    num.random.splits = num_random_splits(c(3,50)),
    regularization.factor=regularization_factor(range = c(0, 1), trans = NULL),
    regularization.usedepth=regularize_depth(values = c(TRUE, FALSE))
  ) 

# Tune Models -------------------------------------------------------------

resp<-model_data %>% select(starts_with("resp_")) %>% colnames()
resp<-resp[grepl("Perc",resp)]

for (ep in resp) {
  
  wflow <- workflow() %>% 
    add_model(r_mod) %>% 
    add_recipe(recip_main %>% 
                 update_role(
                   any_of(ep),
                   new_role = "outcome"
                 )%>% 
                 step_naomit(any_of(ep))
    ) %>% 
    add_case_weights(case_weight)
  
  # all_cores <- parallel::detectCores(logical = FALSE)
  # cl <- makePSOCKcluster(all_cores)
  # registerDoParallel(cl)
  
  tune_out<-tune_bayes(
    object=wflow,
    resamples=cros_v,
    param_info=tune_param,
    metrics=yardstick::metric_set(mae,rmse,rsq),
    initial= 10,
    iter = 50,
    control=control_bayes(
      verbose = T,
      verbose_iter = T,
      no_improve = 15L,
      uncertain = 5,
      save_pred = FALSE,
      time_limit = 60,
      parallel_over = "everything"
    )
  )
  
  # stopCluster(cl)
  
  saveRDS(tune_out,file.path("models",paste0("ModelTune_",ep,".csv")))
  
}




# library(reticulate)
# 
# if (F) {
#   # Setup Python Environment
#   # Make sure microsoft VS and CUDA is installed before you start
#   
#   install_miniconda(force = TRUE)
#   
#   conda_list()
#   conda_create("aec_model")
#   
#   use_condaenv("aec_model")
#   conda_install("aec_model","cupy")
#   conda_install("aec_model","git")
#   conda_install("aec_model","git+https://github.com/StatMixedML/Py-BoostLSS.git",pip=T)
# }
# 
# use_condaenv("aec_model")
# 
