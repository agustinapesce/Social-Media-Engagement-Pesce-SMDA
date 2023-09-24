# Twitter project, data cleaning
# Oct 28, 2015
# Julian Wills 
# Purpose: standardize transformations to avoid bugs


# Helper functions --------------------------------------------------------
require(magrittr) || {install.packages("magrittr"); require(magrittr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(readr) || {install.packages("readr"); require(readr)}
require(tictoc) || {install.packages("tictoc"); require(tictoc)}
require(lubridate) || {install.packages("lubridate"); require(lubridate)}

mec_computeIdeo <- function(df, ...) {
  library(e1071);
  # summarizes ideology variables 
  dFin <- df %>% 
    summarise(ideo_mean=mean(ideology,na.rm=T),
              ideo_sd=sd(ideology,na.rm=T),
              count=n(),
              propRep=sum(ideology>0,na.rm=T)/count,
              ideo_skew=skewness(ideology,na.rm=T),
              ideo_min=min(ideology,na.rm=T),
              ideo_max=max(ideology,na.rm=T),
              ideo_med=median(ideology,na.rm=T),
              ideo_first=first(ideology),
              ideo_last=last(ideology),
              ideo_same=sum(ideology>0,na.rm=T)/n(),
              ideo_same=ifelse(ideo_same<.5,1-ideo_same,ideo_same),
              ideo_kurt=kurtosis(ideology,na.rm=T))
  return (dFin)
}

mec_sumUsers <- function(df, ...) {
  
  #merge tweet authors who are already in sample and recalculate summaries
  dT3 <- df %>% filter(grepl("^RT @",text)) %>% 
    separate(text,c("t1","t2"),"^RT @") %>% 
    separate("t2",c("t3","t4")," ") %>% 
    mutate(RT_username=str_replace(t3,":","")) %>% 
    select(-t1,-t4,-t3) %>% 
    select(RT_username) %>% 
    left_join(df,by=c("RT_username"="user.screen_name")) %>% 
    filter(!is.na(user.id_str)) %>% 
    group_by(user.id_str) %>%
    summarise_each(funs(mean(.,na.rm=T)),ACount,MCount,ACount_unq,
                   MCount_unq,shared,ideology,followers) %>% 
    left_join(df %>% group_by(user.id_str) %>% summarise_each(funs(first),text,user.screen_name:time)) 
  
  dT4 <- df %>% filter(grepl("^RT @",text)) %>% 
    mutate(text2=text) %>% 
    separate(text2,c("t1","t2"),"^RT @") %>% 
    separate("t2",c("t3","t4")," ") %>% 
    mutate(RT_username=str_replace(t3,":","")) %>% 
    select(-t1,-t4,-t3) %>% 
    filter(!RT_username %in% dT3$user.screen_name) 
  
  nOutUsers <- nrow(distinct(dT4,RT_username))
  dT5 <- dT3$user.id_str
  dT6 <- NULL; dTa <- NULL; dTb <- NULL; j = 1;
  for (i in seq(1,nrow(dT4),100)) {
    dTa <- seq(i,99+i,1)
    dTb <- dTa[!dTa %in% dT5]
    dT6 <- append(dTb,dT6)
    if (length(dT6) > nOutUsers){
      dT6 <- dT6[1:nOutUsers]
      break
    }
  }
  
  dT6 <- as.data.frame(dT6)
  
  # users outside the original sample
  dT7 <- dT4 %>% group_by(RT_username) %>% 
    summarise_each(funs(mean(.,na.rm=T)),ACount,MCount,ACount_unq,MCount_unq,shared) %>% 
    left_join(dT4 %>% group_by(RT_username) %>% summarise_each(funs(first),text)) %>% 
    #arrange(desc(count)) %>% 
    # left_join(dGRT0.All,by=c("RT_username"="user.screen_name")) 
    bind_cols(dT6)  %>% 
    #arrange(desc(count)) %>% 
    rename(user.id_str=dT6,user.screen_name=RT_username)
  
  dFin <- dT3 %>% full_join(dT7)
  
  # return (dFin)
}

# There are more tweets starting w/ RT than tweets with retweet ID.

mec_countNas  <- function(df, ...)  {
  df_range <- df %>% ncol()
  df %>% summarise_at(vars(1:df_range),funs(sum(is.na(.)|is.infinite(.)))) 
  
} 

mec_demean <- function(df, response=NA, ...)  { #response="count" response="twid"
  # mean centers all numeric variables, except for response variable
  allVars <- df %>% names()
  numVars <- df %>% select(which(sapply(., is.numeric)))  %>% names()
  if (!is.na(response)) {numVars <- numVars[numVars!=response]}
  varIdxs <- which(allVars %in% numVars)
  df %>% mutate_each(funs(.-mean(.,na.rm=T)),varIdxs)
}

mec_collapseMiddlemen <- function(df, ...) { #df=dMAll.GM # df = dClean
  library(stringr); library(tidyr); library(e1071);
  # aggregates retweets that reference the same source
  
  cat("\n Stripping RT @, replacing username with source, assigning middleman...")
  dStrip <- df %>% filter(grepl("^RT @",text)) %>% mutate(text2=text) %>% 
    separate(text2,c("t1","t2"),"^RT @") %>% 
    separate("t2",c("t3","t4")," ") %>% 
    mutate(RT_username=str_replace(t3,":","")) %>% 
    select(-t1,-t4,-t3) %>% 
    mutate(text=str_replace(text,paste0("^RT @",RT_username),""),
           text=str_replace(text,"^:","")) %>% 
    rename(middleman=user.screen_name,
           user.screen_name=RT_username)
  
  cat("\n Summarizing mean/sd ideology and total number of RTs for each message from each source...")
  dSrc1 <- dStrip %>% 
    select(src=user.screen_name, usr=middleman, rtwid, twid, text, everything()) %>%
    group_by(text,src) %>% 
    mec_computeIdeo() %>% 
    select(count,everything()) %>% 
    arrange(desc(count)) #%>% mutate(propCross=ifelse(ideology<0,propRep,1-propRep))
  
  cat("\n Computing number of distinct retweeters for each retweet...")
  cat("\n Preserving static message variables (e.g. media), and joining...")
  dSrc2 <- dStrip %>% 
    select(src=user.screen_name,usr=middleman,rtwid,twid,text,everything()) %>% 
    group_by(text,src) %>% 
    distinct(rtwid, .keep_all= TRUE) %>% 
    summarise(nRTers=n()) %>% 
    arrange(desc(nRTers)) %>% 
    left_join(dSrc1, by = c("text", "src")) %>% 
    left_join(
      dStrip %>% rename(src=user.screen_name) %>% rename(user.screen_name=middleman), by = c("text", "src")
    ) %>% 
    group_by(text,src) %>%
    summarise_each(funs(first),src:ideo_sd,time,user.verified:media, #danger select
                   propRep,ideo_skew:ideo_kurt,text_orig,twid) %>% 
    select(count, nRTers, text, src:ideo_sd, twid_first=twid, everything()) %>%
    ungroup() %>% 
    arrange(desc(count))
  
  dSrc3 <- df %>% 
    filter(!grepl("^RT @",text)) %>% 
    rename(src=user.screen_name) %>% 
    group_by(text,src) %>% 
    summarise_each(funs(first), followers,ideology,user.location,user.description)
  
  dSrc4 <-  dSrc2 %>% 
    mutate(text=str_replace(text,"^ ","")) %>% 
    left_join(dSrc3, by = c("text", "src")) %>% 
    rename(nSrcs=nRTers)
  
  # fill in missing values with average followers, ideology, location, and description
  dFin <- dSrc4 %>% 
    group_by(src) %>% 
    filter(is.na(followers)) %>% 
    select(-ideology, -followers, -user.location, -user.description) %>% 
    left_join(
      dSrc4 %>% 
        group_by(src) %>% 
        filter(!is.na(followers)) %>% 
        mutate_each(funs(mean(.,na.rm=T)), ideology,followers) %>% 
        distinct(src, .keep_all= TRUE) %>% 
        select(src,ideology,followers), by = "src"
    ) %>% 
    left_join(
      dSrc4 %>% 
        group_by(src) %>% 
        filter(!is.na(followers)) %>% 
        summarise_each(funs(max(.,na.rm=T)), user.location,user.description), by = "src"
    ) %>% 
    bind_rows(
      dSrc4 %>% 
        group_by(src) %>% 
        filter(!is.na(followers))
    ) %>% 
    arrange(desc(count))
  
  # return (dFin)
}

mec_stripMiddlemen <- function(df, ...) {
  library(stringr); library(tidyr); 
  # assigns middlemen and source for relevant retweets. Not collapsed by retweet
  
  # strip RT @, replace username with source, assign middleman, replace these rows in original corpus
  dFin <- df %>% filter(grepl("^RT @",text)) %>% mutate(text2=text) %>% 
    separate(text2,c("t1","t2"),"^RT @") %>% 
    separate("t2",c("t3","t4")," ") %>% 
    mutate(RT_username=str_replace(t3,":","")) %>% 
    select(-t1,-t4,-t3) %>% 
    mutate(text=str_replace(text,paste0("^RT @",RT_username),""),text=str_replace(text,"^:","")) %>% 
    rename(middleman=user.screen_name) %>% rename(user.screen_name=RT_username) %>% 
    bind_rows(
      df %>% filter(!grepl("^RT @",text))
    ) %>% arrange(time)
  
  return (dFin)
}

mec_userEdges <- function(df1,df2,w=FALSE,...) {
  # write user edges and lonely tweeters to .csv, as well as average use of moral, emotion, and interaction
  # df1: full data, df2: RT0 data. 
  library(stringr); library(tidyr); 
  
  dT1 <- df1 %>% mec_stripMiddlemen() %>% select(twid,rtwid,user.screen_name) %>% 
    left_join(
      df2 %>% mec_sumUsers() %>% select(-time,-text)
    ) 
  
  dT2 <- dT1 %>% filter(is.na(rtwid),!is.na(ACount)) %>% 
    select(author=user.screen_name,aID=user.id_str,twid) %>% #twids that were retweeted
    left_join(
      df1 %>% 
        filter(!is.na(rtwid)) %>% 
        select(retweeter=user.screen_name,rID=user.id_str,twid=rtwid),by="twid") 
  
  
  dT3 <- dT2 %>% filter(!is.na(rID)) %>% select(user.id_str=aID) %>% 
    left_join(dT1 %>% select(user.id_str,ACount:shared)) %>% 
    distinct(user.id_str) %>% mutate(AxM=ACount*MCount) %>% 
    bind_rows(
      dT2 %>% filter(!is.na(rID)) %>% select(user.id_str=rID) %>% 
        left_join(dT1 %>% select(user.id_str,ACount:shared)) %>% 
        distinct(user.id_str) %>% mutate(AxM=ACount*MCount)
    ) %>% select(user.id_str,ACount,MCount,AxM)
  
  
  
  if (w==TRUE) {
    tStr <- deparse(substitute(df1))
    
    #lonely tweeters
    dT2 %>% filter(is.na(rID)) %>% select(user.screen_name=author,aID) %>% left_join(
      df1 %>% select(user.screen_name)
    ) %>% distinct(aID) %>% select(aID) %>% 
      write.csv(paste0(tStr,"_lonelyTweeters_",Sys.time(),".csv"),row.names=F)
    
    #edges
    dT2 %>% filter(!is.na(rID)) %>% select(aID,rID) %>% 
      write.csv(paste0(tStr,"_userEdges_",Sys.time(),".csv"),row.names=F)
    
    
    save(dT1,dT2,dT3,file=paste0(tStr,"_userIDs_",Sys.time(),".RData"))
    write.csv(dT3,paste0(tStr,"_AxMperUser_",Sys.time(),".csv"),row.names=F)
  }
  
  return(dT3)
}
