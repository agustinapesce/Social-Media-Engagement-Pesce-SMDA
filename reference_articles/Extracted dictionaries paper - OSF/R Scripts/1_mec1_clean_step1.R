# Date: May 28th, 2016
# Author: Julian Wills
# Purpose: step 1 of pipeline to preprocess raw data. Starts with raw tweets and outputs .csvs for dictionary encoding.
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

# userDir <- ("/Users/Julian/GDrive/1 Twitter Project/00 - Paper/mec_osfPipeline/") #Julian's path
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

packages <- c("tm", "Matrix", "RcppParallel", "quanteda", "class", "e1071", "stringr", "feather",
              "magrittr","tibble", "dplyr", "readr", "tictoc", "lubridate")
ipak(packages)

helperFile <- paste0(scriptDir,"mec1_funcs.R")
source(helperFile)

select <- dplyr::select #in case masked by MASS package

# NOTE: Switching 'paste0' to 'stringr::str_c' for consistency and readability 

# Keyword Filtering ------------------------------------

# Create dataframe referencing each topic and path to raw data
topics <- c("M","G","C") #topics="M"
topics_fullName <- c("Same-Sex Marriage", "Gun Control", "Climate Change")
dCollection <- data.frame(topic=topics,topic_full=topics_fullName)

keywords_M <- c("gaymarriage","gay-marriage","gaywedding","gay-wedding","same sex","samesex",
                "lovewins","gayrights","marref",
                "gay marriage","same sex","gay wedding","gay rights") %>% paste0(collapse="|")

keywords_G <- c("guncontrol","gunviolence","endgunviolence",
                "second amendment","2nd amendment","gunlaws",
                "gunsense","gunsafety","2A",
                "gun culture","gun murders","gun threat","gun offenses","gun crisis","gun reform",
                "gun owner","gun owners","gun ownership","gun crime","gun ban","gun smuggling",
                "gun confiscation","gun rights","gun deaths","gun violence","gun safety",
                "gun laws","gun control") %>% paste0(collapse="|")

keywords_C <- c("climate change","climate","climatechange","global warming","globalwarming") %>% 
  paste0(collapse="|")

for (t in topics) { # t="M" # t="G" # t="C"
  
  currentTopic <- filter(dCollection,topic==t)$topic_full
  cat("\n Filtering keywords for",currentTopic,". . .")
  
  # Set working directory 
  dataDir %>% setwd()
  
  # Load in raw data. Rename variables. Effect code media and url. 
  # Replace HTML characters.
  dRaw <- tbl_df(read.csv(str_c(t,"_raw.csv"),header=T,sep=",",encoding='UTF-8')) %>% 
    rename(twid=id_str,
           time=timestamp,
           rtwid=retweeted_status.id_str,
           followers=user.followers_count,
           url=contains.url,
           media=contains.media) %>% 
    mutate_each(funs(ifelse(.=="True",1,-1)),media,url) %>% 
    mutate(text_orig=text,
           text=str_replace_all(text,"&amp;#8216;|&amp;#8211;|&amp;#8217;|&amp;#039;|&amp;#8230;",""),
           text=str_replace_all(text, "[\u201C\u201D\u201E\u201F\u2033\u2036\u2018\u2019\u201A\u201B\u2032\u2035]", "'"),
           text=str_replace_all(text,"&amp;","&")
    )
  
  # Only include tweets that contain specific tokens 
  keywords <- get(str_c("keywords_", t))
  dFilt <- dRaw %>% #slice(1) %>% 
    filter(str_detect(removePunctuation(char_tolower(text)),
                      paste(keywords, collapse="|", sep="|")))
  
  
  # NOTE: As of 03/01/17, this script results in 6 tweets missing from gun control due to a problematic 
  # gun emoji, Although the exclusion of these tweets has a negligible impact on the model, we will manually 
  # add them back in here so that our data/results can be reproduced exactly. 
  
  if (t == "G") {
    missingTweets_G <- c(675511727988035584, 675508947667648512, 675712722684653568,
                         674329411236192256, 674329421420093440, 675508950905802752)
    
    dFilt <- dFilt %>% 
      bind_rows(
        dRaw %>% 
          
          # Replace problematic emojis and characters with blanks
          filter(twid %in% missingTweets_G) %>% 
          mutate(text=iconv(text, "latin1", "ASCII", "byte"),
                 text=str_replace_all(text,'<f0><9f><94><ab>', ''),
                 text=str_replace_all(text,'#gunsens<e', '#gunsense')
          )
        
      )
  }
  
  
  # Remove duplicate IDs
  dClean <- dFilt %>% group_by(twid) %>% slice(1) %>% ungroup() %>% arrange(time) 
  
  # filtAppend <- dCollection %>% filter(topic==t) %$% filtAbbrev
  dfName <- str_c("d",t,"Filt")
  dfOutput <- str_c(t,"_filt",".RData")
  assign(dfName,dClean)
  if (saveIntermed) save(list=dfName, file=str_c(outputDir,dfOutput))
  
}

# clear dataframes that are no longer needed to save memory 
if (clearMem) rm(dRaw,dFilt,dClean)

# Aggregate by Retweet ID ------------------------------------

# Switch to directory for latest files were dumped
outputDir %>% setwd()

topics <- c("M","G","C")
for (t in topics) { #t="M"
  
  currentTopic <- filter(dCollection,topic==t)$topic_full
  cat("\n Aggregating by Retweet ID for",currentTopic,". . .")
  
  # Load in dataframe created from previous step
  dfName <- str_c("d",t,"Filt")
  dfLoad <-  str_c(t,"_filt",".RData")
  if (saveIntermed) {dClean <- local(get(load(dfLoad))) 
  } else {dClean <- get(str_c("d",t,"Filt"))}
  
  # Summarize ideology variables for retweeters of each message. 
  # Merge with original dataframe. 
  # Assign "1" indicating tweets that originated in corpus, and "0" if not
  dRT1 <- dClean %>% group_by(rtwid) %>% 
    mec_computeIdeo() %>% 
    rename(twid=rtwid) %>%   
    left_join(dClean,by="twid") %>% distinct(twid, .keep_all=TRUE) %>% filter(!is.na(time)) %>% arrange(desc(count)) %>% 
    select(-rtwid) %>% 
    mutate(origCorp=1) %>% 
    mutate(isRT=ifelse(count>0,1,0))
  
  # Append tweets that were never retweeted
  dRT0 <- dRT1 %>% 
    bind_rows(
      dClean %>% filter(is.na(rtwid)) %>% 
        mutate(count=as.integer(0)) %>% 
        mutate(origCorp=1) %>% 
        mutate(isRT=ifelse(count>0,1,0))
    ) %>% 
    arrange(desc(count)) %>% 
    distinct(twid, .keep_all=TRUE) %>%
    select(-rtwid) 
  
  # Error check: do number of distinct retweets match?
  all.equal(dRT1,filter(dRT0,count>0)) %>% stopifnot()
  
  # Append retweets that originate from outside the corpus. 
  # Compute summary ideology variables. Remove columns with missing information (e.g., User ID)
  # Rearrange columns to lead with tweet ID and retweet count. Remove NAs. 
  # Sort by highest retweet count.
  dRT1.All <- dRT1 %>% bind_rows(
    dClean %>% group_by(rtwid) %>% 
      mec_computeIdeo() %>% 
      left_join(dClean,by="rtwid") %>% select(-twid) %>% rename(twid=rtwid) %>% 
      anti_join(dRT1,by="twid") %>% distinct(twid, .keep_all=TRUE)  %>%
      select(-user.description,-user.screen_name,-user.verified,-user.id_str,-user.location,
             -time,-followers) %>% 
      select(twid,count,everything()) %>% 
      filter(!is.na(twid)) %>% 
      mutate(origCorp=0) %>% 
      mutate(isRT=ifelse(count>0,1,0))
  ) %>% arrange(desc(count))
  
  dRT0.All <- dRT0 %>% 
    bind_rows(
      dRT1.All %>% filter(origCorp==0)
    ) %>% arrange(desc(count))
  
  # Error check: do total number of retweets match?
  all.equal(
    dRT0.All %>% summarise(totalRT=sum(as.numeric(count))),
    dClean %>% filter(!is.na(rtwid)) %>% summarise(totalRT=as.numeric(n()))
  )  %>% stopifnot()
  
  
  # Error check: do total number of original tweets match?
  all.equal(
    dRT0  %>% summarise(totalOrigT=n()),
    dClean %>% filter(is.na(rtwid)) %>% summarise(totalOrigT=n())
  ) %>% stopifnot()
  
  # Error check: do total number of tweets & retweets match?
  all.equal(
    dRT0.All %>% 
      group_by(origCorp) %>% 
      summarise(totalN=sum(count+origCorp)) %>% 
      summarise(totalN=sum(totalN)),
    dClean %>% summarise(totalN=as.numeric(n()))
  ) %>% stopifnot()
  
  dfName <- str_c("d",t,"RT0")
  dfOutput <- str_c(t,"_RTsum",".RData")
  assign(dfName,dRT0.All)
  if (saveIntermed) save(list=dfName, file=str_c(outputDir,dfOutput))
  
}

if (clearMem) rm(dRT1.All,dRT1,dRT0.All,dRT0,dClean)

# Aggregate by Retweet Source --------------------------------

topics <- c("M","G","C")
for (t in topics) { #t="M"
  
  currentTopic <- filter(dCollection,topic==t)$topic_full
  cat("\n Aggregating by Retweet Source for",currentTopic,". . .")
  
  # Load in dataframes created from previous steps
  dfName <- str_c("d",t,"Filt")
  dfLoad <-  str_c(t,"_filt",".RData")
  if (saveIntermed) {dClean <- local(get(load(dfLoad)))
  } else {dClean <- get(dfName)}
  
  dfName <- str_c("d",t,"RT0")
  dfLoad <-  str_c(t,"_RTsum",".RData")
  if (saveIntermed) {dRT0.All <- local(get(load(dfLoad)))
  } else dRT0.All <- get(dfName)
  
  # Aggregate dataframe based on retweet source, rather than retweet ID
  # Warning messages will appear (ignore).
  dSource1 <- dClean %>% mec_collapseMiddlemen()
  
  # Remove leading text from retweets (i.e., "RT @[Username]:" ) 
  # Warning message will appear (ignore).
  dTrim1 <- dRT0.All %>% 
    filter(grepl("^RT @",text)) %>% 
    mutate(text2=text) %>% 
    separate(text2,c("t1","t2"),"^RT @", extra="drop") %>% 
    separate("t2",c("t3","t4")," ", extra="drop") %>% 
    mutate(RT_username=str_replace(t3,":","")) %>% 
    select(-t1,-t4,-t3) %>% 
    mutate(text=str_replace(text,paste0("^RT @",RT_username),""),
           text=str_replace(text,"^:","")) %>% 
    rename(middleman=user.screen_name) %>% 
    rename(user.screen_name=RT_username)
  
  # Remove leading whitespace from tweets
  dTrim2 <- dRT0.All %>%
    filter(!twid %in% dTrim1$twid) %>%  
    bind_rows(dTrim1) %>% 
    mutate(text=str_replace(text,"^ ","")) %>% 
    rename(src=user.screen_name)
  
  dSource2 <- dSource1 %>% 
    left_join(dTrim2 %>% select(text, twid_first=twid), by = c("text", "twid_first")) %>% 
    distinct(text,src, .keep_all=TRUE) %>% 
    ungroup()
  
  dSource3 <- dSource2 %>% 
    bind_rows(
      dClean %>% 
        filter(!str_detect(text,"^RT @")) %>% 
        filter(!twid %in% dClean$rtwid) %>% 
        rename(src=user.screen_name,twid_first=twid) %>% 
        select(-rtwid,-user.id_str) %>% 
        mutate(count=0)
    ) %>% 
    # count number of tweets each user has in corpus
    group_by(src_id) %>% 
    mutate(src_id_n = n()) %>% 
    ungroup()
  

  
  # Error check: do total number of retweets match?
  all.equal(
    dSource2 %>% summarise(totalN=sum(count)) %$% totalN,
    dClean %>% filter(str_detect(text,"^RT @")) %>% nrow()
  ) %>% stopifnot()
  
  # Error check: do total number of unretweeted tweets match?
  all.equal(
    dSource3 %>% filter(count==0) %>% summarise(totalN=n()) %$% totalN,
    dClean %>% filter(!str_detect(text,"^RT @")) %>% filter(!twid %in% dClean$rtwid) %>% nrow()
  ) %>% stopifnot()
  
  dfName <- str_c("d",t,"Source")
  dfOutput <- str_c(t,"_RTsource",".RData")
  assign(dfName,dSource3)
  if (saveIntermed) save(list=dfName, file=str_c(outputDir,dfOutput))
  
}

# clear dataframes that are no longer needed to save memory 
if (clearMem) rm(dMRT0,dGRT0,dCRT0,dSource1,dSource2,dSource3,dTrim1,dTrim2,dClean,dRT0.All) 

# Concatenate dataframes --------------------------------------------------------

outputDir %>% setwd()

load("M_RTsource.RData")
load("G_RTsource.RData")
load("C_RTsource.RData")

dAll.Source <- 
  dMSource %>% mutate(topic="M",topic_full="Same-Sex Marriage") %>% 
  bind_rows(
    dGSource %>% mutate(topic="G",topic_full="Gun Control"),
    dCSource %>% mutate(topic="C",topic_full="Climate Change") 
  )

if (clearMem) rm(dMSource,dGSource,dCSource) # clear dataframes that are no longer needed to save memory 
if (saveIntermed) saveRDS(dAll.Source,file = str_c(outputDir,"allSets_RTsource.rds"))

# Ideology Post-Processing ------------------------------------------------

# dAll.Source <- readRDS(file = str_c(outputDir,"allSets_RTsource.rds")) #uncomment to load file directory

dAll.Ideo <- dAll.Source

if (saveIntermed) saveRDS(dAll.Ideo,file = str_c(outputDir,"allSets_ideo.rds"))

#  Fill Missing Data on Followers/Verification ------------------------------------------------

# Load in english-only raw retweets 
dAll.rawRTs <- readRDS(str_c(dataDir,"allSets_SMaPP_rawRTs.rds"))

# For for all tweets originating outside corpus, fill in followers and overwrite user verification.
# Drop cases that are still missing since it implies they are non English tweets. 
# Followers computed as the mean followers of users across dataset. 
# Also compute 'user.cnt' to estimate how active/popular each user is (number of distinct retweets authored).
# Dummy code user verified (0 = unverified, 1 = verified). Sort from greatest to least retweets.
# (pre-corpus = retweeted tweets that originated before the data collection period)

dAll.Verif <- NULL
topics <- c("M","G","C")
for (t in topics) { #t="M"
  
  currentTopic <- filter(dCollection,topic==t)$topic_full
  dfName <- str_c("d",t,"Filt")
  dfLoad <-  str_c(t,"_filt",".RData")
  if (saveIntermed) dClean <- local(get(load(dfLoad))) 
  else dClean <- get(dfName)
  
  cat("\n Grabbing user IDs of pre-corpus retweet authors for",currentTopic,". . .")
  dRTSrcIDs <- dAll.Ideo %>% filter(topic==t) %>% 
    filter(count>0) %>% 
    left_join(
      dAll.rawRTs %>% filter(topic==t) %>% 
        select(src,rt.user.verified:rt.created) %>% 
        group_by(src) %>% 
        summarise(src_id=first(rt.user.id_str),
                  src_cnt=n())
    )
  
  cat("\n Grabbing user IDs of corpus tweet authors for",currentTopic,". . .")
  dTweetSrcIDs <- dAll.Ideo %>% filter(topic==t) %>% 
    filter(count==0) %>% 
    left_join(
      dClean %>% 
        select(src=user.screen_name,src_id=user.id_str) %>% 
        group_by(src) %>% 
        summarise(src_id=first(src_id),
                  src_cnt=n())
    )
  
  cat("\n Retrieving followers/verification of pre-corpus tweet authors for",currentTopic,". . .")
  dVerif <- dRTSrcIDs %>% 
    filter(!is.na(followers)) %>% 
    bind_rows(
      dRTSrcIDs %>% 
        filter(is.na(followers)) %>% 
        select(-followers,-user.verified) %>% 
        left_join(
          dAll.rawRTs %>% filter(topic==t) %>% 
            select(src,rt.user.verified:rt.created) %>% 
            group_by(src) %>% 
            summarise(followers=mean(rt.followers),
                      user.verified=first(rt.user.verified))
        )
    ) %>% 
    bind_rows(dTweetSrcIDs) %>% 
    filter(!is.na(src_id)) %>%
    mutate(user.verified=as.numeric(ifelse(user.verified=="False",0,
                                           ifelse(user.verified=="True",1,user.verified))),
           user.verified=ifelse(count==0,0,user.verified)) %>% 
    arrange(desc(count))
  
  nNonEng <- dRTSrcIDs %>% filter(is.na(src_id)) %>% nrow()
  cat("\n Removed",nNonEng,"non-English tweets from",currentTopic,". . .")
  
  # Error checking
  sumNAs <- dVerif %>% select(followers,user.verified,src_cnt,src_id) %>% mec_countNas() %>% 
    rowwise() %>%  mutate(sumNAs = sum(followers,user.verified,src_cnt,src_id)) %>% 
    select(sumNAs) %>% ungroup() %>% unlist() %>% magrittr::extract2(1)
  if (!all.equal(sumNAs,0)) cat("WARNING: Missing Values Remaining...")
  
  dAll.Verif <- dAll.Verif %>% bind_rows(dVerif)
  
}

if (clearMem) rm(dRTSrcIDs,dTweetSrcIDs,dAll.rawRTs) 
if (saveIntermed) saveRDS(dAll.Verif,file = str_c(outputDir,"allSets_verif.rds"))

#  Fill in and Update Ideology Scores ------------------------------------------------

# Load in user ideology file
dIdeo.raw <- readRDS(str_c(dataDir,"ideologyFull_SMaPP.rds"))

# Fill in missing ideology estimates wherever possible. 
# For estimates still missing, indicate with binary 'hasIdeo' variable.
dAll.ideoClean_1 <- dAll.Verif %>% 
  filter(is.na(ideology)) %>% 
  select(-ideology) %>% 
  left_join(
    dIdeo.raw %>% select(src_id=id_str,ideology=theta)
  ) %>% 
  bind_rows(
    dAll.Verif %>% 
      filter(!is.na(ideology))
  ) %>% 
  mutate(hasIdeo=ifelse(is.na(ideology),0,1)) %>% 
  mutate(topic_order=ifelse(topic=="M",1,ifelse(topic=="G",2,3))) %>% 
  group_by(topic_order) %>% 
  arrange(topic_order,desc(count)) %>% 
  ungroup()

# Error checking
missingIdeo1 <- dAll.Verif %>% select(ideology) %>% mec_countNas() %>% magrittr::extract2(1)
missingIdeo2 <- dAll.ideoClean_1 %>% select(ideology) %>% mec_countNas() %>% magrittr::extract2(1)
cat("\n Filled in",missingIdeo1 - missingIdeo2,"missing Ideology Estimates")


# Update Obama's ideology score based on more accurate estimate
dElites <- read_csv(str_c(dataDir,"accounts-twitter-data-with-ideology.csv")) %>% 
  #  Change POTUS screenname to Obama
  mutate(id_str = ifelse(screen_name == "POTUS", ObamaID, id_str)) %>% 
  # Rename variables to be consistent with RT data. 
  select(src_id = id_str, ideology = phi) 

# Swap out ideology/party for elites
dAll.ideoComplete <- dAll.ideoClean_1 %>% 
  left_join(
    dElites %>% rename(ideo_new = ideology)
  ) %>% 
  mutate(ideology  = ifelse(!is.na(ideo_new) & ideo_new != ideology, ideo_new, ideology),
         # Effect code ideology of tweeter (lib = -.5; con = +.5)
         src_Rep = sign(ideology)/2,
         # Effect code average ideology of retweeters (lib = -.5; con = +.5)
         RT_Rep = sign(ideo_mean)/2)

if (clearMem) rm(missingIdeo1,missingIdeo2,dAll.Verif,dIdeo.raw) 
if (saveIntermed) saveRDS(dAll.ideoComplete,file = str_c(outputDir,"allSets_ideoComplete.rds"))

#  Prepare for Dictionary Encoding  ----------------

if (saveIntermed) {dAll.ideoComplete <- readRDS(str_c(outputDir,"allSets_ideoComplete.rds"))}

# Trim to text column (and topic + twid_first to make sure everything matches up). 
dAll.trim <- dAll.ideoComplete %>% select(text, topic, twid_first)

# Save to each dictionary encoding directory
write.csv(dAll.trim,file = str_c(dictDir,"moral_emo/allSets_trim.csv"),row.names=F, fileEncoding = "utf-8")
write.csv(dAll.trim,file = str_c(dictDir,"moral_emo_pos/allSets_trim.csv"),row.names=F, fileEncoding = "utf-8")
write.csv(dAll.trim,file = str_c(dictDir,"moral_emo_neg/allSets_trim.csv"),row.names=F, fileEncoding = "utf-8")

# Feed through dictionary script ---------------------------------------------------------------------

# See OSF wiki for next steps 
