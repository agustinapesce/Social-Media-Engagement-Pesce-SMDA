# Date: May 28th, 2016
# Author: Julian Wills
# Purpose: step 2 of pipeline to preprocess raw data. Starts with dictionary encoded .csvs and outputs .csvs for SAS analysis.
# Dependencies: 
#  mec1_funcs.R (Moral Emotion Contagion project-specific helper functions )
# Options:
#  set user directory to path from OSF
#  saving intermediate files is recommended but requires an additional ~680 MB

# Choose directory and set paths --------------------------------------------------------

scriptDir <- paste0(dirname(sys.frame(1)$ofile),"/")
userDir <- gsub("Rscripts/","",scriptDir)

saveIntermed <- TRUE #If 'TRUE', script will output all intermediate files. Otherwise, only the final dataset will be saved.
# saveIntermed <- FALSE
clearMem <- TRUE #If 'FALSE', all dataframe objects will stay in memory. Set to 'TRUE' to avoid memory bottleneck. 

userDir <- ("/Users/Julian/GDrive/1 Twitter Project/00 - Paper/mec_osfPipeline/") #Julian's path
scriptDir <- paste0(userDir,"Rscripts/")
dataDir <- paste0(userDir,"rawData/")
outputDir <- paste0(userDir,"cleanedData/")
dictDir <- paste0(userDir,"dictionaries/")


# Source helpers functions and load packages  --------------------------------------------------------
require(checkpoint) || {install.packages("checkpoint"); require(checkpoint)}
checkpoint("2016-05-28")

helperFile <- paste0(scriptDir,"mec1_funcs.R")
source(helperFile) 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  
}

packages <- c("RcppParallel", "class", "stringr", "magrittr","tibble", "dplyr", "readr", "tictoc")
ipak(packages)

helperFile <- paste0(scriptDir,"mec1_funcs.R")
source(helperFile)

select <- dplyr::select #in case masked by MASS package


# Consolidate dictionary encoding -----------------------------------------

# Load in most recent R datafile 
dAll.recompIdeo <- readRDS(str_c(outputDir,"allSets_recompIdeo.rds"))

# Add relevant columns from each encoding
dAll.Encoded <- dAll.recompIdeo %>% 
  bind_cols(
    read.csv(file = str_c(dictDir,"moral_emo/allSets_encoded.csv")) %>% tbl_df() %>% 
      select(ACount_unq = AffectCount, MCount_unq = MoralCount, shared)
  ) %>% 
  bind_cols(
    read.csv(file = str_c(dictDir,"moral_emo_pos/allSets_encoded.csv")) %>% tbl_df() %>% 
      select(posCount_unq = Affect_posCount, shared_pos = shared)
  ) %>% 
  bind_cols(
    read.csv(file = str_c(dictDir,"moral_emo_neg/allSets_encoded.csv")) %>% tbl_df() %>% 
      select(negCount_unq = Affect_negCount, shared_neg = shared)
  ) 


# Delete temporary files if encoding was successful; otherwise stop script
nEncodings <- sum(str_detect(names(dAll.Encoded), "ACount_unq|posCount_unq|negCount_unq"))

if ( nEncodings < 3) {
  encodingFail <- NA
  stopifnot(encodingFail)
  break
} else {
  unlink(str_c(dictDir,"moral_emo/allSets_encoded.csv"))
  unlink(str_c(dictDir,"moral_emo_pos/allSets_encoded.csv"))
  unlink(str_c(dictDir,"moral_emo_neg/allSets_encoded.csv"))
  
  # Remove preprocessed files before dictionary encoding
  if (saveIntermed) {
    unlink(str_c(dictDir,"moral_emo/allSets_trim.csv"))
    unlink(str_c(dictDir,"moral_emo_pos/allSets_trim.csv"))
    unlink(str_c(dictDir,"moral_emo_neg/allSets_trim.csv"))
    unlink(str_c(dictDir,"moral_emo_neg/allSets_recompIdeo.rds"))
  }
  
}

# Error check: do total number of unretweeted tweets match?
all.equal(
  dSource3 %>% filter(count==0) %>% summarise(totalN=n()) %$% totalN,
  dClean %>% filter(!str_detect(text,"^RT @")) %>% filter(!twid %in% dClean$rtwid) %>% nrow()
) %>% stopifnot()


if (saveIntermed) {saveRDS(dAll.Encoded,file = str_c(outputDir,"allSets_encoded.rds"))}

# Tidy and Split up for primary SAS Analysis -------------------------------------------------------

# Load in output from dictionary script. 
if (saveIntermed) {dAll.Encoded <- readRDS(str_c(outputDir,"allSets_encoded.rds"))}

# Remove all text data. 
# Round all decimal values to .001's. 
dAll.SAS <- dAll.Encoded %>% 
  select(-text,-text_orig,-topic_full,-user.location,-user.description,
         -src,-twid_first,-time, -src_cnt,-topic_order, -nSrcs,
         -ideo_skew:-ideo_med, -ideo_sd, -ideo_first:-ideo_kurt,
         -crossTalk) %>% 
  mutate_each(funs(round(.,4)),ideo_mean,followers,ideology)

# Split topics for SAS analysis.
dAll.SAS %>%
  filter(topic=="M") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SASpreproc_Marriage.csv"),row.names=F)
dAll.SAS %>%
  filter(topic=="G") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SASpreproc_Gun.csv"),row.names=F)
dAll.SAS %>%
  filter(topic=="C") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SASpreproc_Climate.csv"),row.names=F)

# Tidy and Split up for SAS GEE Analysis (ingroup/outgroup) -------------------------------------------------------

# Load in output from dictionary script. 
if (saveIntermed) {dAll.Encoded <- readRDS(str_c(outputDir,"allSets_encoded.rds"))}

dGEE_1 <- dAll.Encoded %>% 
  mutate(propRep = ifelse(count == 0, 0, propRep)) %>% 
  mutate(src_Rep = sign(ideology)/2)

dGEE_2 <- dGEE_1 %>% 
  mutate(count2 = ((count)*propRep),
         RT_Rep = +.5) %>% 
  bind_rows(
    dGEE_1 %>% 
      mutate(count2 = ((count)*(1-propRep)),
             RT_Rep = -.5)
  ) %>% 
  select(topic, count_old = count, count = count2, RT_Rep, everything()) %>% 
  arrange(topic, -count_old, twid_first, -count) %>% 
  mutate(ingroup = ifelse(sign(ideology) == sign(RT_Rep), .5, -.5)) %>% 
  filter(!is.na(ideology)) %>% 
  group_by(topic, src_Rep) %>% 
  mutate(extr = abs(ideology)) %>% 
  mutate(mod_dist = cume_dist(extr)) %>% 
  ungroup() 


# Error checking: verify permutations look correct
src_Rep_outgroup_n <- dGEE_2 %>% count(ingroup, src_Rep, RT_Rep) %>% ungroup() %>% slice(2) %>% select(n)
src_Rep_ingroup_n <- dGEE_2 %>% count(ingroup, src_Rep, RT_Rep) %>% ungroup() %>% slice(4) %>% select(n)
all.equal(src_Rep_outgroup_n,src_Rep_ingroup_n) %>% stopifnot()

if (saveIntermed) {saveRDS(dGEE_2,file = str_c(outputDir,"allSets_GEE_ingroup.rds"))}

dSAS_GEE <- dGEE_2 %>%   
  select(topic, twid_first, count_total = count_old, count, ingroup, Rep_src=src_Rep, Rep_RT = RT_Rep, src_id, 
         mod_dist, ACount_unq:shared_neg, ideology, 
         url, media, followers, user.verified, extr) 

dSAS_GEE %>%
  filter(topic=="M") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SAS_ingroup_Marriage.csv"),row.names=F)
dSAS_GEE %>%
  filter(topic=="G") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SAS_ingroup_Gun.csv"),row.names=F)
dSAS_GEE %>%
  filter(topic=="C") %>% 
  select(-topic) %>% 
  write.csv( str_c(outputDir,"MEC_SAS_ingroup_Climate.csv"),row.names=F)
