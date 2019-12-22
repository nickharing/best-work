# 
# Get it Done data mining
# June 14, 2019
# Nick Haring nharing@sandiego.gov
# raw data came from here: https://data.sandiego.gov/datasets/get-it-done-311/
## NOTE: lines 7-72 were coded to bring in raw data and reduce it to a manageble size. I like to sub-sample large data sets to work faster and identify patterns.
##        All data exploration below line 72 uses, mini-gid_2016-2019.csv, which is attached to the assignment email. Just place mini-gid into your working dir folder and go from there. feel free to contact me if these are any problems: 619.995.0508

#Set up the workspace, and bring in Packages----
options(stringsAsFactors = FALSE, show.error.locations = T)

if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)} # This automates (mostly) calling up package libraries
if(!require(psych)) {install.packages("psych"); library(psych)}
if(!require(data.table)) {install.packages("data.table"); library(data.table)}
if(!require(shiny)) {install.packages("shiny"); library(shiny)}
if(!require(rmapshaper)) {install.packages("rmapshaper"); library(rmapshaper)}

# set up directories to fetch data from and save data to----
main_dir <- "C:/Users/nickharing/Documents/gid" 
data_dir <- paste(main_dir, "data", sep = "/") # this allows flexibility if I want to quickly access multiuple folders
meta_dir <- paste(main_dir, "metadata/", sep = "/")
output <- paste(main_dir, "output/", sep = "/") # a location to export data
setwd(data_dir) # we'll mostly be working here

# # set up directories to fetch data from and save data to----
# main_dir <- "H:/CSD/2.0 Interviews/3.0 PandA program coordinator"
# gid_dir <- paste(main_dir, "gid/", sep = "/") # this allows flexibility if I want to quickly access multiuple folders
# meta_dir <- paste(main_dir, "metadata/", sep = "/")
# output <- paste(main_dir, "output/", sep = "/") # a location to export data
# setwd(gid_dir) # we'll mostly be working here

# Bring in metadata tables, using oldschool read.csv because there's always more than one way to do something in R----
gid_services_file <- "get_it_done_311_services_datasd.csv"
gid_datadic_file <- "get_it_done_requests_dictionary_datasd.csv"
gid_services <- read.csv(paste(meta_dir, gid_services_file, sep = "/"))
gid_datadic <- read.csv(paste(meta_dir, gid_datadic_file, sep = "/")) # but throw in some tidyverse to see these data nice and clean

##  Step 1: Bring in all GID data from 2016-present and make it look easy--- [that didn't work, not so easy I guess]
# gid <- list.files(full.names = TRUE) %>%  # this lists the files in the wd and reads in .csv files, then combines them into one df with bind_rows
#   lapply(read_csv) %>% 
#   bind_rows 
# error binding all 4 files: Warning: 18 parsing failures. row col  expected    actual      file...

#looks like some column names changed in 2019 so let's have a look at the individual files
sixteen_file <- "get_it_done_2016_requests_datasd.csv"
sixteen <- read.csv(paste(gid_dir, sixteen_file, sep = "/"))
seventeen_file <- "get_it_done_2017_requests_datasd.csv"
seventeen <- read.csv(paste(gid_dir, seventeen_file, sep = "/"))
eighteen_file <- "get_it_done_2018_requests_datasd.csv"
eighteen <- read.csv(paste(gid_dir, eightteen_file, sep = "/"))
nineteen_file <- "get_it_done_2019_requests_datasd.csv"
nineteen <- read.csv(paste(gid_dir, nineteen_file, sep = "/")) 
glimpse(sixteen)
glimpse(seventeen)
glimpse(eighteen)
glimpse(nineteen)

# view the column names by extracting them from each file then making a table for comparison
name_compare <- tibble('2016' = names(sixteen), '2017' = names(seventeen), '2018' = names(eighteen), '2019' = names(nineteen)) 
# three 2019 column names have changed....seems fishy, changing them back to match 2016-2018. okay, I should have looked here first: https://data.sandiego.gov/stories/data-portal-quality-links/
new_nineteen <- nineteen %>% 
  rename(., long = lng, updated_datetime = date_updated,  requested_datetime =  date_requested) # getting the names to match

# Now let's put these data together for real (and export it back out as a .csv). commenting out this code because it takes forever to put this dataframe together and I don't want to accidentally re-run it.
# gid <- bind_rows(list(sixteen, seventeen, eighteen, new_nineteen))  %>% #pretty big file
#   write_csv(., path = paste(output,"gid_2016-2019.csv"))

# This file is too big to explore so I'm going to chop it down (to 1/4 its original size) to speed things up. if a pattern emerges I can test on the entire data frame. mini_gid allows for temporal comparisons
mini_gid <- sample_n(gid, size = 10000) %>% # I'm taking a random sample to preserve stastical patterns, if I learn something on this data set, it's likely to be applicable to the full dataset
  write_csv(., path = paste(output,"mini-gid_2016-2019.csv"))
  
# check for duplicated rows....dosen't look like it
dups <- anyDuplicated(mini_gid)

###--------
# START HERE with "mini-gid_2016-2019.csv"

mini_file <- "mini-gid_2016-2019.csv"
mini_gid <- read.csv(paste(data_dir, mini_file, sep = "/")) %>% 
  select(., -sap_notification_number, -comm_plan_code, 
         -comm_plan_name, -park_name, -service_request_parent_id) # declutter the df, can add columns back if we need to explore those variables
  
  
# Now let's ask some questions to see what's what (probably can't get to all of them but writing them down as I think of them)
#  which department get has the longest turn around time? (compare service_name and case_record_type by case_age_days)
#  what's up with council_district = NA? I think they may be outside of the City, and mostly trash pickup. maybe CSD provides trash services to some residents just outside City boundries? 
#  What type of reqests take the longest (see #1, pretty similar question)
#  what the average wait time for each district
#  does adding comments tend to speed up or increase the sucess rate of a case?
#  what's up with blank service_name and 2 version of Other/OTHER? (Also,recommend standardizing the service names text and making them a little more machine readable, if there are't already naming conventions, would be a good idea to implement some)
#  round down the lat/longs, concatenate, and find the mode - one way to see where most of the reports are comming from without mapping

# Coding on the flight to Hilton Head, fligh attendant told me no smoking allowed on the plane
# I told the F.A. that Ceci (%>%) n'est pas une pipe

## some basic stats to get a handle on these data
dup_rows <- length(duplicated(mini_gid)) - length(mini_gid$service_request_id) # no dups
dup_cols <- length(distinct(mini_gid)) - length(mini_gid) # no dups
summary_stats <- mini_gid %>% 
  select(case_record_type, service_name, case_age_days) %>% 
  #do(psych::describeBy(.$case_age_days, .$service_name)) # have to use do to used psych::describe with pipes, couldn't get it to work right, moving on
test<- psych::describeBy(summary_stats$case_age_days, summary_stats$case_record_type) 

## Checking out council_district = NA and has location data
council_NA <- mini_gid %>% 
    filter(., is.na(council_district), #pick the rows were interested in
         !is.na(lat)) %>%  # interesting, !is.na(lat) identified 2 lat/longs that were 0.0000, field was filled, not NA
  arrange(., case_origin, case_record_type, service_name)
# okay, very few mobile reports, and mostly trash pick up request, 38 mappable objects, most seem outside SD b/c not a lot of 32/33s and -117s for lat/long, I'll map the locations if I have time, would be a good deliverable to show public, exec. mgmt., or elected officials
# would it be possible to geofence the mobile app so it only works within the City?

## let's see how long it takes to get things done
TATs<- mini_gid %>%
  filter(., !is.na(case_age_days)) %>% #pick the rows were interested in
  group_by(., case_record_type, service_name) %>% #, service_name
  summarize( # box plot is the way to go with the results below
    total = length(case_age_days),
    mean_days = round(mean(case_age_days), digits = 1),
    lower_quart = quantile(case_age_days, probs=0.25, na.rm=TRUE),
    median_days = round(median(case_age_days), digits = 1),
    upper_quart = quantile(case_age_days, probs=0.75, na.rm=TRUE),
    max_days = max(case_age_days)
  ) %>% 
  arrange(., desc(median_days)) %>% 
  filter(., total > 50) # more common cases
# the top 3 reports take about 2 weeks or less to close. Do we have target TATs? 2 weeks is not too bad, but always room for improvement 

cd_TATs<- mini_gid %>% 
  filter(., !is.na(case_age_days), !is.na(council_district)) %>% #pick the rows were interested in
  group_by(., council_district) %>% #, service_name
  summarize( # box plot is the way to go with the results below
    total = length(council_district),
    mean_days = round(mean(case_age_days), digits = 1),
    lower_quart = quantile(case_age_days, probs=0.25, na.rm=TRUE),
    median_days = round(median(case_age_days), digits = 1),
    upper_quart = quantile(case_age_days, probs=0.75, na.rm=TRUE),
    max_days = max(case_age_days)
  ) %>% 
  arrange(., desc(median_days)) 
# interesting, CDs 1&5 have the fewest cases and longest waiting time
 
## Let's look at the comments and referrals
comments <- mini_gid %>%
  filter(., public_description != "", status != "Referred") #removing referral, let's look at them seperately

no_comments <- mini_gid %>% s
  filter(., public_description == "", status != "Referred") 
  
referrals <- mini_gid %>%
  filter(., !is.na(council_district), status == "Referred") %>% # About 23% of the sampled cases were referred out
  group_by(., council_district)
cd_referrals <- referrals %>% 
  summarize(total = length(council_district))
# CDs 1&5 had fewest referrals, but they also had the fewest overall cases, what's up with CD3?

## okay, I think I've done enough to identify some patterns and narrow down the broad QA needs. I dont't want to 
#  duplicate work as there are some similar data tools at: https://data.sandiego.gov/ so I'm going to
#  change focus on delivering a list of cases that could benefit form some QA.



