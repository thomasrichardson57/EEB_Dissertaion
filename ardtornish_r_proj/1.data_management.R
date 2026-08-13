#----Import Data----
library(readxl)

birds <- read_xlsx("data/birds.xlsx", sheet = 1)
metadata <- read.csv("data/survey_metadata.csv", header = T)
rich <- read_xlsx("data/birds.xlsx", sheet = 2)

#----Stitch them Together----
pre_merge_bird_surveyid <- unique(birds$survey_id)
pre_merge_bird_surveyid

birds <- merge(birds, metadata, by = 'survey_id')
rich <- merge(rich, metadata, by = 'survey_id')

#----Exclude obs past 30 minutes (birds)----

birds <- birds[birds$minutes < 30, ]

#----Check----
unique(birds$survey_id)
unique(birds$species)
length(unique(birds$species))
unique(birds$habitat)

#### check it all matches ####
unique(birds$survey_id)

#----make things factors----
birds$survey_id <- as.factor(birds$survey_id)
birds$species <- as.factor(birds$species)
birds$site <- as.factor(birds$site)
birds$habitat <- as.factor(birds$habitat)

rich$habitat <- as.factor(rich$habitat)


#### create an observation time column ####

birds$seconds[is.na(birds$seconds)] <- 0
birds$time <- birds$minutes + (birds$seconds/60)

#### convert start times ####
library("stringr")
library("lubridate")


birds$start_time_padded <- str_pad(birds$start_time, width = 4, pad = "0")
range(birds$start_time_padded)

# Convert to decimal hours
birds$start_time.dec <- as.numeric(substr(birds$start_time_padded, 1, 2)) + 
  as.numeric(substr(birds$start_time_padded, 3, 4)) / 60






