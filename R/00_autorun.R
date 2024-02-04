#source("R/01_DownloadGISData.R",local=T)

#source("R/02_GenerateLookups.R",local=T)

#source("R/03_ProcessHydrology.R",local=T)

#source("R/04_ProcessGISPreds.R",local=T)

#source("R/05_DownloadBioData.R",local=T)

#source("R/06_ProcessBioData.R",local=T)

#source("R/07_Calculate_Attributes.R",local=T)


# source("R/10a_CombineData.R",local=T)
# print("**1")
# 
# source("R/10b_Calculate_Attributes_at_Bio.R",local=T)
# print("**2")
# 
# source("R/11a_CombineData2.R",local=T)
# print("**3")
# 
# source("R/11b_FormatModelData.R",local=T)
# print("**4")
# 
# source("R/11c_FormatModelData.R",local=T)
# print("**5")

source("R/12_lightgbmLLS_tune_r_to_py.R",local=T)
print("**1")

source("R/13_Train_Final.R",local=T)
print("**2")

source("R/14_Model_Evaluation.R",local=T)
print("**3")

source("R/15_Landscape_Prediction.R",local=T)
print("**4")