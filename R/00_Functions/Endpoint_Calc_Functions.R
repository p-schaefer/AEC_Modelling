rZAGamma<-function(n=1000,concentration,rate,gate) {
  ot<-try(rgamma(n,shape=concentration, rate=rate)*Rlab::rbern(n,1-gate),silent=T)
  if (inherits(ot,"try-error")){
    return(NA_real_)
  } else {
    return(ot)
  }
}

sim_fish_ep<-function(df,n,infocols){
  rZAGamma<-function(n=1000,concentration,rate,gate) {
    ot<-try(rgamma(n,shape=concentration, rate=rate)*Rlab::rbern(n,1-gate),silent=T)
    if (inherits(ot,"try-error")){
      return(NA_real_)
    } else {
      return(ot)
    }
  }
  
  map_dfr(1:n,
      ~df %>% 
        rowwise() %>% 
        mutate(EstimatedBiomass=rZAGamma(1,resp_Comm_Biomass_concentration,resp_Comm_Biomass_rate,resp_Comm_Biomass_gate),
               NumberOfFish=rZAGamma(1,resp_Comm_Abundance_concentration,resp_Comm_Abundance_rate,resp_Comm_Abundance_gate)
               ) %>% 
        ungroup() %>% 
        mutate(across(any_of(c("EstimatedBiomass","NumberOfFish")),~expm1(.x))) %>% 
        filter(!if_any(any_of(c("EstimatedBiomass","NumberOfFish")),~is.na(.x))) %>% 
        filter(!if_all(any_of(c("EstimatedBiomass","NumberOfFish")),~.x==0)) %>% 
        select(ProvReachID,all_of(c("EstimatedBiomass","NumberOfFish")),all_of(info_cols)) %>% 
        set_names(c("SampleEventID",c("EstimatedBiomass","NumberOfFish"),info_cols)) %>% 
        mutate(SampleDate="2000-01-01") %>% 
        calc_fish_ep()
      ) 
}

calc_fish_ep<-function(df) {

  # Calculate endpoints -----------------------------------------------------
  
  ep_tbl<-df %>% 
    group_by(SampleEventID,SampleDate) %>% 
    mutate(
      # General Community
      Comm_Biomass=sum(EstimatedBiomass,na.rm=T),
      Comm_Abundance=sum(NumberOfFish,na.rm=T),
      Comm_Biomass_SATI=sum(EstimatedBiomass[!is.na(OPT_TEMP)],na.rm=T),
      Comm_Abundance_SATI=sum(NumberOfFish[!is.na(OPT_TEMP)],na.rm=T),
      Comm_Richness=length(unique(SpeciesCode)),
      EstimatedBiomass_perc=EstimatedBiomass/Comm_Biomass,
      NumberOfFish_perc=NumberOfFish/Comm_Abundance,
      EstimatedBiomass_perc_SATI=EstimatedBiomass/Comm_Biomass_SATI,
      NumberOfFish_perc_SATI=NumberOfFish/Comm_Abundance_SATI,
      SATI_EstimatedBiomass_perc=EstimatedBiomass_perc_SATI*OPT_TEMP,
      SATI_NumberOfFish_perc=NumberOfFish_perc_SATI*OPT_TEMP,
    ) %>% 
    summarise(
      # General Community
      Comm_Biomass=head(Comm_Biomass,1),
      Comm_Abundance=head(Comm_Abundance,1),
      Comm_Richness=head(Comm_Richness,1),
      # Thermal Regime 
      Therm_SATI_Bioperc=sum(SATI_EstimatedBiomass_perc,na.rm=T),
      Therm_SATI_Abuperc=sum(SATI_NumberOfFish_perc,na.rm=T),
      
      Therm_cold_Bioperc=sum(EstimatedBiomass_perc[Thermal.Regime=="coldwater"],na.rm=T),
      Therm_cool_Bioperc=sum(EstimatedBiomass_perc[Thermal.Regime=="coolwater"],na.rm=T),
      Therm_coolcold_Bioperc=sum(EstimatedBiomass_perc[Thermal.Regime %in% c("coolwater","coldwater")],na.rm=T),
      Therm_warm_Bioperc=sum(EstimatedBiomass_perc[Thermal.Regime %in% c("warmwater")],na.rm=T),
      Therm_cold_Abuperc=sum(NumberOfFish_perc[Thermal.Regime=="coldwater"],na.rm=T),
      Therm_cool_Abuperc=sum(NumberOfFish_perc[Thermal.Regime=="coolwater"],na.rm=T),
      Therm_coolcold_Abuperc=sum(NumberOfFish_perc[Thermal.Regime %in% c("coolwater","coldwater")],na.rm=T),
      Therm_warm_Abuperc=sum(NumberOfFish_perc[Thermal.Regime %in% c("warmwater")],na.rm=T),
      Therm_cold_Bio=sum(EstimatedBiomass_perc[Thermal.Regime=="coldwater"],na.rm=T),
      Therm_cool_Bio=sum(EstimatedBiomass_perc[Thermal.Regime=="coolwater"],na.rm=T),
      Therm_coolcold_Bio=sum(EstimatedBiomass_perc[Thermal.Regime %in% c("coolwater","coldwater")],na.rm=T),
      Therm_warm_Bio=sum(EstimatedBiomass_perc[Thermal.Regime %in% c("warmwater")],na.rm=T),
      Therm_cold_Abu=sum(NumberOfFish_perc[Thermal.Regime=="coldwater"],na.rm=T),
      Therm_cool_Abu=sum(NumberOfFish_perc[Thermal.Regime=="coolwater"],na.rm=T),
      Therm_coolcold_Abu=sum(NumberOfFish_perc[Thermal.Regime %in% c("coolwater","coldwater")],na.rm=T),
      Therm_warm_Abu=sum(NumberOfFish_perc[Thermal.Regime %in% c("warmwater")],na.rm=T),
      Therm_warm_Rich=length(NumberOfFish_perc[Thermal.Regime %in% c("warmwater")]),
      Therm_cold_Rich=length(NumberOfFish_perc[Thermal.Regime %in% c("coldwater")]),
      Therm_cool_Rich=length(NumberOfFish_perc[Thermal.Regime %in% c("coolwater")]),
      Therm_coolcold_Rich=length(NumberOfFish_perc[Thermal.Regime %in% c("coolwater","coldwater")]),
      # Trophic Guild 
      Troph_invert_Bioperc=sum(EstimatedBiomass_perc[grepl("invertivore",Trophic.Class)],na.rm=T),
      Troph_detrit_Bioperc=sum(EstimatedBiomass_perc[grepl("detritivore",Trophic.Class)],na.rm=T),
      Troph_carn_Bioperc=sum(EstimatedBiomass_perc[grepl("carnivore",Trophic.Class)],na.rm=T),
      Troph_herb_Bioperc=sum(EstimatedBiomass_perc[grepl("herbivore",Trophic.Class)],na.rm=T),
      Troph_plank_Bioperc=sum(EstimatedBiomass_perc[grepl("planktivore",Trophic.Class)],na.rm=T),
      Troph_invert_Bio=sum(EstimatedBiomass_perc[grepl("invertivore",Trophic.Class)],na.rm=T),
      Troph_detrit_Bio=sum(EstimatedBiomass_perc[grepl("detritivore",Trophic.Class)],na.rm=T),
      Troph_carn_Bio=sum(EstimatedBiomass_perc[grepl("carnivore",Trophic.Class)],na.rm=T),
      Troph_herb_Bio=sum(EstimatedBiomass_perc[grepl("herbivore",Trophic.Class)],na.rm=T),
      Troph_plank_Bio=sum(EstimatedBiomass_perc[grepl("planktivore",Trophic.Class)],na.rm=T),
      Troph_invert_Abuperc=sum(NumberOfFish_perc[grepl("invertivore",Trophic.Class)],na.rm=T),
      Troph_detrit_Abuperc=sum(NumberOfFish_perc[grepl("detritivore",Trophic.Class)],na.rm=T),
      Troph_carn_Abuperc=sum(NumberOfFish_perc[grepl("carnivore",Trophic.Class)],na.rm=T),
      Troph_herb_Abuperc=sum(NumberOfFish_perc[grepl("herbivore",Trophic.Class)],na.rm=T),
      Troph_plank_Abuperc=sum(NumberOfFish_perc[grepl("planktivore",Trophic.Class)],na.rm=T),
      Troph_invert_Abu=sum(NumberOfFish_perc[grepl("invertivore",Trophic.Class)],na.rm=T),
      Troph_detrit_Abu=sum(NumberOfFish_perc[grepl("detritivore",Trophic.Class)],na.rm=T),
      Troph_carn_Abu=sum(NumberOfFish_perc[grepl("carnivore",Trophic.Class)],na.rm=T),
      Troph_herb_Abu=sum(NumberOfFish_perc[grepl("herbivore",Trophic.Class)],na.rm=T),
      Troph_plank_Abu=sum(NumberOfFish_perc[grepl("planktivore",Trophic.Class)],na.rm=T),
      Troph_invert_Rich=length(NumberOfFish_perc[grepl("invertivore",Trophic.Class)]),
      Troph_detrit_Rich=length(NumberOfFish_perc[grepl("detritivore",Trophic.Class)]),
      Troph_carn_Rich=length(NumberOfFish_perc[grepl("carnivore",Trophic.Class)]),
      Troph_herb_Rich=length(NumberOfFish_perc[grepl("herbivore",Trophic.Class)]),
      Troph_plank_Rich=length(NumberOfFish_perc[grepl("planktivore",Trophic.Class)]),
      # Tolerance
      Tol_tol_Bioperc=sum(EstimatedBiomass_perc[Tolerance=="tolerant"],na.rm=T),
      Tol_intol_Bioperc=sum(EstimatedBiomass_perc[Tolerance=="intolerant"],na.rm=T),
      Tol_tol_Abuperc=sum(NumberOfFish_perc[Tolerance=="tolerant"],na.rm=T),
      Tol_intol_Abuperc=sum(NumberOfFish_perc[Tolerance=="intolerant"],na.rm=T),
      Tol_tol_Bio=sum(EstimatedBiomass_perc[Tolerance=="tolerant"],na.rm=T),
      Tol_intol_Bio=sum(EstimatedBiomass_perc[Tolerance=="intolerant"],na.rm=T),
      Tol_tol_Abu=sum(NumberOfFish_perc[Tolerance=="tolerant"],na.rm=T),
      Tol_intol_Abu=sum(NumberOfFish_perc[Tolerance=="intolerant"],na.rm=T),
      Tol_tol_Rich=length(NumberOfFish_perc[Tolerance=="tolerant"]),
      Tol_intol_Rich=length(NumberOfFish_perc[Tolerance=="intolerant"]),
      # Individual Species - 
      Spec_RBD_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==337],na.rm=T),
      Spec_RBD_Abuperc=sum(NumberOfFish_perc[SpeciesCode==337],na.rm=T),
      Spec_BKT_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==80],na.rm=T),
      Spec_BKT_Abuperc=sum(NumberOfFish_perc[SpeciesCode==80],na.rm=T),
      Spec_GDF_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==181],na.rm=T),
      Spec_GDF_Abuperc=sum(NumberOfFish_perc[SpeciesCode==181],na.rm=T),
      Spec_CMC_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==186],na.rm=T),
      Spec_CMC_Abuperc=sum(NumberOfFish_perc[SpeciesCode==186],na.rm=T),
      Spec_RDG_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==366],na.rm=T),
      Spec_RDG_Abuperc=sum(NumberOfFish_perc[SpeciesCode==366],na.rm=T),
      Spec_CKC_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode==212],na.rm=T),
      Spec_CKC_Abuperc=sum(NumberOfFish_perc[SpeciesCode==212],na.rm=T),
      Spec_SKP_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(381,382)],na.rm=T),
      Spec_SKP_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(381,382)],na.rm=T),
      Spec_LMP_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(11:14)],na.rm=T),
      Spec_LMP_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(11:14)],na.rm=T),
      Spec_DRT_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(335:340,342)],na.rm=T),
      Spec_DRT_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(335:340,342)],na.rm=T),
      Spec_CEB_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(311, 313:317)],na.rm=T),
      Spec_CEB_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(311, 313:317)],na.rm=T),
      Spec_CTF_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(231:233)],na.rm=T),
      Spec_CTF_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(231:233)],na.rm=T),
      Spec_SAL_Bioperc=sum(EstimatedBiomass_perc[SpeciesCode %in% c(71:83)],na.rm=T),
      Spec_SAL_Abuperc=sum(NumberOfFish_perc[SpeciesCode %in% c(71:83)],na.rm=T),
      # Environment
      Env_benthic_Bioperc=sum(EstimatedBiomass_perc[Environment=="benthic"],na.rm=T),
      Env_pelagic_Bioperc=sum(EstimatedBiomass_perc[Environment=="pelagic"],na.rm=T),
      Env_benthopelagic_Bioperc=sum(EstimatedBiomass_perc[Environment=="benthopelagic"],na.rm=T),
      Env_benthic_Abuperc=sum(NumberOfFish_perc[Environment=="benthic"],na.rm=T),
      Env_pelagic_Abuperc=sum(NumberOfFish_perc[Environment=="pelagic"],na.rm=T),
      Env_benthopelagic_Abuperc=sum(NumberOfFish_perc[Environment=="benthopelagic"],na.rm=T),
      Env_benthic_Bio=sum(EstimatedBiomass_perc[Environment=="benthic"],na.rm=T),
      Env_pelagic_Bio=sum(EstimatedBiomass_perc[Environment=="pelagic"],na.rm=T),
      Env_benthopelagic_Bio=sum(EstimatedBiomass_perc[Environment=="benthopelagic"],na.rm=T),
      Env_benthic_Abu=sum(NumberOfFish_perc[Environment=="benthic"],na.rm=T),
      Env_pelagic_Abu=sum(NumberOfFish_perc[Environment=="pelagic"],na.rm=T),
      Env_benthopelagic_Abu=sum(NumberOfFish_perc[Environment=="benthopelagic"],na.rm=T),
      Env_benthic_Rich=length(NumberOfFish_perc[Environment=="benthic"]),
      Env_pelagic_Rich=length(NumberOfFish_perc[Environment=="pelagic"]),
      Env_benthopelagic_Rich=length(NumberOfFish_perc[Environment=="benthopelagic"]),
      # Reproductive Guild
      ## Guarders
      Rep_guard_Bioperc=sum(EstimatedBiomass_perc[grepl("^B\\.",Reproductive.Code)],na.rm=T),
      Rep_guard_Abuperc=sum(NumberOfFish_perc[grepl("^B\\.",Reproductive.Code)],na.rm=T),
      Rep_guard_Rich=length(NumberOfFish_perc[grepl("^B\\.",Reproductive.Code)]),
      ## Nesting?
      Rep_OSS_Bioperc=sum(EstimatedBiomass_perc[grepl("\\.1\\.",Reproductive.Code)],na.rm=T),
      Rep_OSS_Abuperc=sum(NumberOfFish_perc[grepl("\\.1\\.",Reproductive.Code)],na.rm=T),
      Rep_OSS_Rich=length(NumberOfFish_perc[grepl("\\.1\\.",Reproductive.Code)]),
      Rep_BrHid_Bioperc=sum(EstimatedBiomass_perc[grepl("\\.2\\.",Reproductive.Code)],na.rm=T),
      Rep_BrHid_Abuperc=sum(NumberOfFish_perc[grepl("\\.2\\.",Reproductive.Code)],na.rm=T),
      Rep_BrHid_Rich=length(NumberOfFish_perc[grepl("\\.2\\.",Reproductive.Code)]),
      ## Lithophilic
      Rep_Lith_Bioperc=sum(EstimatedBiomass_perc[grepl("Litho",Reproductive.Guild)],na.rm=T),
      Rep_Lith_Abuperc=sum(NumberOfFish_perc[grepl("Litho",Reproductive.Guild)],na.rm=T),
      Rep_Lith_Rich=length(NumberOfFish_perc[grepl("Litho",Reproductive.Guild)]),
      .groups="drop"
    ) 
  
  return(ep_tbl)
}