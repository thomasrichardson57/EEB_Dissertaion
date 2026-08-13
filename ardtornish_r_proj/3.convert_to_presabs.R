#### packages ####

library(dplyr)

#---- Rearange ----

#as only the observed birds appear for each survey in original dataframe,
#add 1 to each to denote its presence
birds$pres <- 1

# birds$days <- birds$days.x

#creates a df of all info besides the species pres/abs
template <- birds %>% distinct(site, days, survey_id,
                               habitat, elevation, start_time.dec)
#create a list of all of the bird species observed, also useful to check
#for errors and duplicates
birdlist <- unique(birds$species)
birdlist

survey_list <- unique(birds$survey_id)
survey_list

#creates a template with all possible combinations of transect and bird spp
template2 <- expand.grid( survey_id=survey_list, species=birdlist )
colnames(template)
colnames(template2)

template3<- merge(template2, template, by=c("survey_id"), all.x=T)

colnames(template3)

#creates a data frame with the presence absence data, all.x=T means 0's are 
#listed as NA rather than omitted
pres.colnames <- colnames(birds)[c(1,6,9,13,16,19,22)]

presence <- merge(template3, birds, by = c("survey_id", "days", "species",
                                            "habitat", "elevation", "start_time.dec"), all.x=T)
#presence <- merge (template3, birds, by = c(""), all.x=T)
colnames(presence)
#replaces NA's in the presence column with 0's
presence$pres[is.na(presence$pres)]<-0

presence$elevation <- as.numeric(presence$elevation)

## need to sort out the weather etc to populate up the dataframe ##

# relevel

presence$species <- relevel(presence$species, ref = 'meadow_pipit')









