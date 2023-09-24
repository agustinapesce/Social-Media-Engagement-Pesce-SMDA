# Date: May 10th, 2017
# Author: Julian Wills
# Purpose: Analyses conducted in R (e.g. bootstrapping for non-independence)
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
outputDir <- paste0(userDir,"cleanedData/")

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

packages <- c("tm", "Matrix", "RcppParallel", "quanteda", "class", "stringr",
              "magrittr","tibble", "dplyr", "readr")
ipak(packages)

helperFile <- paste0(scriptDir,"mec1_funcs.R")
source(helperFile)

select <- dplyr::select #in case masked by MASS package


# Bootstrapping -----------------------------------------------------------

# Load data
dAll.Dicts <- readRDS("/Users/Julian/GDrive/1 Twitter Project/00 - Paper/mec_osfPipeline/cleanedData/allSets_encoded.rds") 

# Start bootstrapping 
dOut <- NULL
for (tpc in c("M", "G", "C")) {
  set.seed(01242017)
  
  cat("topic: ",tpc, "\n")
  
  for (rep in 1:2000) {
    
    cat(rep," ")
    
    dfGrp <- dAll.Dicts %>% 
      filter(topic==tpc) %>% 
      group_by(src_id)
    
    nGrps <- slice(dfGrp,1) %>% nrow()
    
    mTmp <- dfGrp %>% 
      filter(n() == 1) %>% 
      bind_rows(
        dfGrp %>% 
          filter(n() > 1) %>% 
          do(sample_n(.,1))
      ) %>% glm.nb(count ~ shared + MCount_unq + ACount_unq + media + url + followers + user.verified, data=.)
    
    dOut <- dOut %>%
      bind_rows(
        tidy(mTmp) %>%
          mutate(replicate = rep, topic = tpc, sample_size = nGrps)
      )
    
  }
  
}

# Modified data to help with plotting
dOut2 <- dOut %>% 
  filter(str_detect(term, "shared|Count")) %>% 
  left_join(
    dAll.Dicts %>% distinct(topic, topic_full)
  ) %>% 
  mutate(term = ifelse(term == "MCount_unq", "Moral Language", term),
         term = ifelse(term == "ACount_unq", "Emotional Language", term),
         term = ifelse(term == "shared", "Moral-Emotional Language", term),
         term = factor(term, levels = c("Emotional Language", "Moral Language", "Moral-Emotional Language"))) %>%  
  mutate(topic_full = factor(topic_full, levels = c("Gun Control", "Same-Sex Marriage", "Climate Change")))  %>%  
  group_by(term, topic_full) %>% 
  arrange(estimate) 

# Create figures
dOut2 %>% 
  ggplot(aes(estimate)) + geom_histogram(fill="grey", color="black", binwidth = .01) + 
  geom_vline(aes(xintercept = estimate), linetype="dashed", color = "red", size=.72, data=slice(dOut2,.025*n())) + 
  geom_vline(aes(xintercept = estimate), linetype="dashed", color = "red", size=.72, data=slice(dOut2,.975*n())) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "black", size=.72) + 
  facet_wrap(~topic_full+term, scales="free_x") +
  coord_cartesian(xlim = c(-.12, .41)) +
  theme_Publication() + 
  theme(panel.spacing.x = unit(1, "cm")) + 
  theme(strip.background = element_rect(fill = "grey", color = "grey")) + 
  theme(strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 9)) + 
  theme(axis.text=element_text(size=11))+
  labs(x = "Coefficient Estimate", y = "Frequency", 
       title = "Bootstrapped Distributions of Estimated Coefficients")


# compute mean and 95% CI
dOut2 %>% 
  arrange(estimate) %>% 
  summarize(CI_lo = quantile(estimate, .025),
            CI_hi = quantile(estimate, .975),
            mean = mean(estimate)) %>% 
  mutate_at(vars(CI_lo:mean), funs(round(.,3)))