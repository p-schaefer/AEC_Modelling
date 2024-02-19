source("R/01_DownloadGISData.R",local=T)

source("R/02_GenerateLookups.R",local=T)

source("R/03_ProcessHydrology.R",local=T)

source("R/04_ProcessGISPreds.R",local=T)

source("R/05_DownloadBioData.R",local=T)

source("R/06_ProcessBioData.R",local=T)

source("R/07_Calculate_Attributes.R",local=T)

source("R/10a_CombineData.R",local=T)

source("R/10b_Calculate_Attributes_at_Bio.R",local=T)

source("R/11a_CombineData2.R",local=T)

source("R/11b_FormatModelData.R",local=T)

source("R/11c_FormatModelData.R",local=T)

source("R/11d_FormatPredictionData.R",local=T)

source("R/12_lightgbmLLS_tune_r_to_py.R",local=T)

source("R/13_Train_Final.R",local=T)

source("R/14_Model_Evaluation.R",local=T)

source("R/15_Landscape_Prediction.R",local=T)

source("app/Model_Explore2/add_data_prep.R",local=T)
