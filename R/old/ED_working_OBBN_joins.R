library(tidyverse)


dat <- readRDS(file.path("data","raw","Bio","OBBN_ALL.rds"))

head(dat$tblOBBNBenthic)
head(dat$tblOBBNSampleEvent)

names(dat$tblOBBNBenthic) #taxonmic information
names(dat$tblOBBNSampleEvent) #collection method, mesh size, date
names(dat$tblOBBNCollectionArea) # lat, long
names(dat$tblOBBNSite) #waterbody type, lat, long, reference site
names(dat$tblOBBNPebble) #pebble measures, don't need for now

##all have SITE CODE, SITE VISIT ID, some also have COLLECTION AREA ID 

# 
# dat <- readRDS(file.path("data","raw","Bio","BIC_ALL_ED.rds"))
# 
# 
# dat$tblBenthicSurveySE$SiteCode
# 
# check <- dat$tblOBBNBenthic %>%
#   dplyr::select(`SITE CODE`) %>%
#   distinct()
# nrow(dat$tblBenthicSurveySE) #349 unique sampling events
# nrow(check)#1163 distinct site codes 
# 
# check2 <- check %>%
#   filter(`SITE CODE` %in% c(dat$tblBenthicSurveySE$SiteCode))
# 
# nrow(check2) #only 13 of those site codes are in BenthicSurveySE$SiteCode
# 
# check3 <- dat$tblOBBNBenthic %>%
#   filter(`SITE CODE`=="EC011WM")
# 
# # so confused. There are less sampling events in the SE tab then sites (let alone sampling events) in the OBBN database
# #according to the instructions online, the samping event table is referred to only by Survey Pebble. BUt suvey Pebble has
# #far more IDs than the sampling event table 
# 
# view(dat$tblBenthicTallyArea)
# view(dat$tblBenthicTallySubstrate)
# 
# 
# ##see what matches in any of the tables
# 
# m1 <- dat$tblBenthicSurveySE %>%
#   dplyr::select(SampleEventID) %>%
#   left_join(dat$tblBenthicSurveyPebble %>%
#               dplyr::select(SampleEventID) %>%
#               distinct() %>%
#               mutate(survpeb=1))
# 
# sum(m1$survpeb, na.rm=TRUE)/nrow(m1) # 25% of sampling events are in pebble data
