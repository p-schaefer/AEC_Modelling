library(tidyverse)
source("R/00_Functions/Endpoint_Calc_Functions.R")

fp<-file.path("app","Model_Explore2","data",paste0("Model_data_v5_dart.gpkg"))
con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Density = "resp_Comm_Abundance",
         Biomass = "resp_Comm_Biomass")

dat <- tbl(con,"Model_Predictions") %>%
  select(tx_Taxa,gen_ProvReachID,contains(unlist(ep)),contains(pred_names)) %>% 
  collect()


sum_fun<-function(x){
  quant_fn<-function(x,n) quantile(x,probs = seq(0, 1, length.out = n + 1),na.rm=T)
  
  #browser()
  sel_ep<-if_else(grepl("resp_Comm_Abundance",cur_column()),
                  "resp_Comm_Abundance",
                  "resp_Comm_Biomass")
  col<-gsub(paste0(sel_ep,"_"),"",cur_column())
  col<-gsub("_refdiff$|_ref$","",col)
  if (col=="observed") col <- "quant_0.75"
  #col_sel<-paste0(sel_ep,"_")
  
  sel_modelpredictions <- cur_data() %>% 
    select(starts_with(sel_ep)) %>% 
    rename_with(~gsub(paste0(sel_ep,"_"),"",.x)) %>% 
    select(observed,contains(col))
    # select(`Observed`=observed,
    #        `Predicted - Current`=col,
    #        `Predicted - Reference`=paste0(col,"_ref"),
    #        `(Current - Reference)`=paste0(col,"_refdiff"))
  
  
  
  if (grepl("_refdiff",cur_column())){
    diff_list<-sel_modelpredictions[[4]]
    diff_list<-diff_list[!is.na(diff_list)]
    has_0 <- any(diff_list==0)
    if (length(diff_list)==0 | any(is.infinite(diff_list))) diff_list<-0
    diff_list <- diff_list[diff_list!=0]
    if (has_0) diff_list <- c(0,diff_list)

    diff_list<-c(-abs(diff_list),abs(diff_list))
    
    rng2 <- list(
      pretty=pretty(diff_list,8),
      quantile=quant_fn(diff_list,8)#,
      #getJenksBreaks=BAMMtools::getJenksBreaks(diff_list,8)
    )
    
    rng2 <- map(rng2,function(x){
      x<-x[x!=0]
      if (length(x)==0) x<-0
      x<-as.numeric(scales::number(x))
      x<-unique(x)
      if (length(x)<2) x<-c(-4,-3,-2,-1,1,2,3,4)
      return(x)
    })
    
    rng <- rng2
    
  } else {
    val_list<-c(sel_modelpredictions[[1]],sel_modelpredictions[[2]],sel_modelpredictions[[3]])
    val_list<-val_list[!is.na(val_list)]
    has_0 <- any(val_list==0)
    if (length(val_list)==0 | any(is.infinite(val_list))) val_list<-0
    val_list <- val_list[val_list!=0]
    if (has_0) val_list <- val_list <- c(0,val_list)
    
    
    rng <- list(
      pretty=pretty(val_list,8),
      quantile=quant_fn(val_list,8)#,
      #getJenksBreaks=BAMMtools::getJenksBreaks(val_list,8)
    )
    
    rng <- map(rng,function(x){
      if (length(x)==0) x<-0
      x<-as.numeric(scales::number(x))
      x<-unique(x)
      if (length(x)<2) x<-c(0,1,2,3,4)
      return(x)
    })
    
  }
  
  rng <- map(rng,~c(.x,rep(NA_real_,20-length(.x))))
  
  rng <- map(rng,~tibble(rng=.x))
  rng <- map(rng,~rename_with(.x,~paste0(cur_column()))) 

  return(rng)
}

dat2 <- dat %>% 
  mutate(tx_Taxa=case_when(
    tx_Taxa %in% taxa ~ "tx_Taxa",
    T ~ tx_Taxa
  )) %>% 
  select(tx_Taxa,contains("observed"),contains("quant_0.75")) %>% 
  group_by(tx_Taxa) %>% 
  reframe(
    across(
      starts_with("resp_"),
      ~sum_fun(.x)
    )
  ) 

dat3 <- dat2 %>% 
  mutate(break_type=rep(c("pretty","quantile"),length.out=nrow(dat2))) %>% #,"getJenksBreaks"
  select(break_type,everything()) %>% 
  unnest(c(everything()))

t1<-dplyr::copy_to(df=dat3,
                   con,
                   "value_breaks",
                   overwrite =T,
                   append=F,
                   temporary =F,
                   analyze=T,
                   in_transaction=T)

s2<-RSQLite::dbSendQuery(con, "CREATE INDEX value_breaks_idx ON value_breaks (tx_Taxa,break_type);")

DBI::dbDisconnect(con)
