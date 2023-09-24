
#Packages
library(dplyr)



###Directories
#Cleaned Data
setwd("c:/Users/agust/Documents/SEDS/SS_Social_Media/Project papers/P1/Cleaned Data")
getwd()

#Raw data
setwd("c:/Users/agust/Documents/SEDS/SS_Social_Media/Project papers/P1/Raw Data")

#Downloads
setwd("c:/Users/agust/Downloads")





#Data: total 563,312, collapsed 313,003
# total of 563,312 observations across the three topics including both original 
# messages and any retweets of those messages as observations. In order to 
# organize the data into analyzable format, we collapsed and counted all retweets 
# of a given message. Thus, we were left with a final analyzable data set that 
# included 313,003 original messages and their corresponding retweet counts across 
# all topics (48,394 for gun control, 29,061 for same-sex marriage, and 235,548 
# for climate change).



####### Raw data ####### guess I wont use it...

allSets_SMaPP_rawRTs <- readRDS("allSets_SMaPP_rawRTs.rds")
View(allSets_SMaPP_rawRTs) #8173240
table(allSets_SMaPP_rawRTs$topic)

#C       G       M 
#2957097 2473038 2743105 

#Ideology, wont use it
#ideologyFull_SMaPP <- readRDS("ideologyFull_SMaPP.rds")
#View(ideologyFull_SMaPP) #9613233


####### Cleaned Data #######

#"preproc"
C_MEC_SASpreproc <- read.csv("MEC_SASpreproc_Climate.csv")
G_MEC_SASpreproc <- read.csv("MEC_SASpreproc_Gun.csv")
M_MEC_SASpreproc <- read.csv("MEC_SASpreproc_Marriage.csv")

nrow(C_MEC_SASpreproc) + nrow(C_MEC_SASpreproc) + nrow(C_MEC_SASpreproc)
#706644


#"ingroup"
allSets_GEE_ingroup <- readRDS("allSets_GEE_ingroup.rds")
#549632

#Remove duplicates on specific column
distinct_GEE_ingroup <- allSets_GEE_ingroup %>% 
  distinct(twid_first, .keep_all = TRUE)
#274766

table(distinct_GEE_ingroup$topic)
#C       G      M 
#204668  43945  26153 

#Separate files, same as avobe
#C_MEC_SAS_ingroup <- read.csv("MEC_SAS_ingroup_Climate.csv")
#G_MEC_SAS_ingroup <- read.csv("MEC_SAS_ingroup_Gun.csv")
#M_MEC_SAS_ingroup <- read.csv("MEC_SAS_ingroup_Marriage.csv")
#nrow(C_MEC_SAS_ingroup) + nrow(G_MEC_SAS_ingroup) + nrow(M_MEC_SAS_ingroup)
#549632
