library(reticulate)
library(tidyverse)
library(tidymodels)

use_condaenv("AEC_Model")
booster<-"dart"


# Load Python Modules -----------------------------------------------------

lss.model <- import("lightgbmlss.model")
distr.lgb<-import("lightgbmlss.distributions")
shap<-import("shap")
graphviz <-import("graphviz")

# Load Data ---------------------------------------------------------------

taxa_keep<-readRDS(file.path("data","taxa_keep.rds"))

model_data0_fin<-read_rds(file.path("data","final","Prediction_finaltaxa_data.rds")) %>% 
  filter(tx_Taxa %in% taxa_keep$tx_Taxa) %>% 
  mutate(tx_Taxa = factor(tx_Taxa, levels=taxa_keep$tx_Taxa)) 

model_refdata0_fin<-read_rds(file.path("data","final","Prediction_ref_finaltaxa_data.rds")) %>% 
  filter(tx_Taxa %in% taxa_keep$tx_Taxa) %>% 
  mutate(tx_Taxa = factor(tx_Taxa, levels=taxa_keep$tx_Taxa)) 

# resp<-model_data0 %>% select(starts_with("resp_")) %>% colnames()
# resp<-resp[!grepl("Perc|cat_",resp)]
# #ep<-resp[[1]]

# dt<-"Current"
# ep<-"resp_Comm_Abundance"

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
    
    if (F){
      opt_param<-readRDS(file.path("data","models","LSS",paste0("best_params_lightgbm_",ep,"_",booster,".rds")))
      
      trn <- train_final %>% select(-starts_with(c("case_weight","resp_","cat_resp_"))) %>% as.data.frame()
      colnames(trn)
      
      
      tree_dot_all = xgb$booster$dump_model(-1L)
      
      tree_dot_all$pandas_categorical[1]
      
      out <- list()
      for (i in 1:length(tree_dot_all$tree_info)) {
        tree_dot <- tree_dot_all$tree_info[[i]]$tree_structure
        list.names <- strsplit(names(unlist(tree_dot,recursive = T)), split=".", fixed=TRUE)
        node.names <- sapply(list.names, function(x) paste(x, collapse="$"))
        node.names2 <- paste0("tree_dot","$",node.names)
        
        taxa_splits <- node.names2[grepl("split_feature",node.names2)]
        
        taxa_splits2 <- sapply(taxa_splits,function(x) eval(parse(text=x)))
        names(taxa_splits2)<-NULL
        taxa_splits3 <- taxa_splits[taxa_splits2==0]
        
        taxa_splits4 <- gsub("split_feature","threshold",taxa_splits3)
        taxa_splits5 <- sapply(taxa_splits4,function(x) eval(parse(text=x)))
        names(taxa_splits5)<-NULL
        
        taxa_splits6 <- lapply(taxa_splits5,function(x) str_split(x,"\\|\\|"))
        taxa_splits6 <- lapply(taxa_splits6,function(x) unlist(lapply(x,function(y) tree_dot_all$pandas_categorical[[1]][as.numeric(y)+1])))

        out[[length(out)+1]] <- taxa_splits6
        
        # taxa_splits44 <- gsub("split_feature","internal_value",taxa_splits3)
        # taxa_splits55 <- sapply(taxa_splits44,function(x) eval(parse(text=x)))
        # names(taxa_splits55)<-NULL
        
      }
      
      out_matrix <- matrix(nrow=13,ncol=13)
      out_matrix[] <- 0
      out_matrix <- as.data.frame(out_matrix)
      rownames(out_matrix) <- sort(tree_dot_all$pandas_categorical[[1]])
      colnames(out_matrix) <- sort(tree_dot_all$pandas_categorical[[1]])
      
      for (i in 1:length(out)) {
        if (length(out[[i]])==0) next()
        for (ii in 1:length(out[[i]])) {
          if (length(out[[i]][[ii]])==1){
            out_matrix[out[[i]][[ii]],out[[i]][[ii]]] <- out_matrix[out[[i]][[ii]],out[[i]][[ii]]]+1
          } else {
            pw <- expand.grid(sort(out[[i]][[ii]]),sort(out[[i]][[ii]]))
            pw <- lapply(1:nrow(pw),function(x) as.character(unlist(pw[x,])))
            # pw1 <- combn(sort(out[[i]][[ii]]),2,simplify=F)
            # pw2 <- combn(rev(sort(out[[i]][[ii]])),2,simplify=F)
            # pw <- c(pw1,pw2)
            for (iii in pw) {
              if (iii[1]==iii[2]) next()
              out_matrix[iii[1],iii[2]] <- out_matrix[iii[1],iii[2]]+1
            }
          }
        }
      }
      pheatmap::pheatmap(as.matrix(out_matrix))

      heatmap(as.matrix(out_matrix),
              Colv=as.dendrogram(hclust(dist(t(as.matrix(out_matrix))),method ="ward.D")),
              Rowv=as.dendrogram(hclust(dist(t(as.matrix(out_matrix))),method ="ward.D")))
      #explainer = shap$TreeExplainer(xgb$booster)
      
      #tree_dot = xgb$booster$dump_model()["tree_info"]
      dot_data = lss.model$lgb$create_tree_digraph(xgb$booster,
                                                   tree_index=2255L,
                                                   orientation ="horizontal",
                                                   show_info=c("data_percentage","split_gain","internal_count",
                                                               "leaf_count","internal_value","internal_weight",
                                                               "leaf_weight","internal_weight")
                                                   )
      dot_data$view()
      #a<-lss.model$lgb$plot_tree(xgb$booster,tree_index=0L)
    }
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
