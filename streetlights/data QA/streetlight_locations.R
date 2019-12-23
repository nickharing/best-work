
# Streetlight locations QA 
# tldr; ~117 multiple rows, about 113 streetlights in Carlsbad, maybe one in Mexico,
# and maybe one just on the wrong side of the street east of the City; ~22% of the rows don't have an SAP ID,
# 
# 
#December, 2019
# Nick Haring nharing@sandiego.gov
# raw data came from here: https://data.sandiego.gov/datasets/streetlight_inventory/
#
####################################################################################3

#Set up the workspace, and bring in Packages----
options(stringsAsFactors = FALSE, show.error.locations = T)

if (!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)} # This automates (mostly) calling up package libraries
if (!require(psych)) {install.packages("psych"); library(psych)}
if (!require(data.table)) {install.packages("data.table"); library(data.table)}
if (!require(shiny)) {install.packages("shiny"); library(shiny)}
if (!require(rmapshaper)) {install.packages("rmapshaper"); library(rmapshaper)}

# # set up directories to fetch data from and save data to----
# main_dir <- "C:/Users/nickharing/Documents/GitHub/Shiny/sl_locs" 
# data_dir <- paste(main_dir, "data", sep = "/") # this allows flexibility if I want to quickly access multiuple folders
# meta_dir <- paste(main_dir, "metadata/", sep = "/")
# output <- paste(main_dir, "output/", sep = "/") # a location to export data
# setwd(data_dir) # we'll mostly be working here
# 
# set up directories to fetch data from and save data to----
main_dir <- "H:/CSD/2.0 Interviews/4.0 Sustainibility/1.0 Program coordinator/analytics/smart streetlights/R"
data_dir <- paste(main_dir, "data", sep = "/") # this allows flexibility if I want to quickly access multiuple folders
meta_dir <- paste(main_dir, "metadata/", sep = "/")
output <- paste(main_dir, "output/", sep = "/") # a location to export data
setwd(data_dir) # we'll mostly be working here

# Bring in data----
streetlight_file <- "streetlight_locations_datasd_v1.csv"
datadic_file <- "streetlight_locations_dictionary_datasd.csv"
streetlight_locations_raw <- read.csv(paste(data_dir, streetlight_file, sep = "/"), strip.white = TRUE, na.strings=c(""," ","NA")) # I'm stripping out white spaces and identifying blanks as NAs
data_dictionary <- read.csv(paste(meta_dir, datadic_file, sep = "/")) 

# create a df to work with
sl_locs <- streetlight_locations_raw 
------------------------------------------
# This file is pretty big so I'm going to chop it down (to ~1/10 its original size) to speed things up. If a pattern emerges I can test on the entire data frame.
mini_lights <- sample_n(sl_locs, size = 6000) %>% # I'm taking a random sample to preserve stastical patterns, if I learn something on this data set, it's likely to be applicable to the full dataset
write_csv(., path = paste(output,"mini-lights.csv")) #save the sub-sampled file
## some basic QA/stats to get a handle on these data
identical(mini_lights, distinct(mini_lights)) #check again for dups rows, identical = TRUE as expected
glimpse(mini_lights)
head(mini_lights)
summary(mini_lights)
psych::describe(mini_lights)

#okay, back to the main data set
-------------------------------------------------------------

#Let's look at the spatial data extremes to see if there are streetlights outside of the City (using the full data set for this one) 
lat_rows <- sl_locs %>% 
  select(., 1:5, lat, lng, 8) %>% # longitude was before latitude, so I flipped them
  dplyr::filter(., lat > (max(lat) * 0.998) | lat < (min(lat) * 1.00005)) %>% 
  arrange(., desc(lat)) 

lng_rows <- sl_locs %>% 
  select(., 1:5, lat, lng, 8) %>% 
  dplyr::filter(., lng > (max(lng) * 1.00025) | lng < (min(lng) * .99935)) %>% 
  arrange(., desc(lng)) 

lat_lng_rows <- union(lat_rows, lng_rows) %>% 
  arrange(., desc(lat)) 

#percentage of rows w/o NAs for each column (except lat/lon, they are entire)
nrow(sl_locs[!is.na(sl_locs$sap_id), ]) / nrow(sl_locs) * 100 # 78%
nrow(sl_locs[!is.na(sl_locs$streetlight_model), ]) / nrow(sl_locs) * 100 # 99.7%
nrow(sl_locs[!is.na(sl_locs$streetlight_type), ]) / nrow(sl_locs) * 100 # 79%
nrow(sl_locs[!is.na(sl_locs$streetlight_wattage), ]) / nrow(sl_locs) * 100 # 83%
nrow(sl_locs[!is.na(sl_locs$streetlight_voltage), ]) / nrow(sl_locs) * 100 # 65%
nrow(sl_locs[!is.na(sl_locs$location_description), ]) / nrow(sl_locs) * 100 # 79%


# looks like there are a lot of SAP IDs missing from this data set, let's find out how many
na_sap_ids <- sl_locs %>% 
  filter(., is.na(sap_id)) # over 13k!
nrow(na_sap_ids) / nrow(sl_locs) * 100 # 22% percent of streetlights w/o an SAP ID

# do the same with location_description
na_loc_desc <- sl_locs %>% 
  filter(., is.na(location_description)) # 12.9k
nrow(na_loc_desc) / nrow(sl_locs) * 100 # 21% percent of streetlights w/o a location description

#now both (maybe they're correlated)
na_sapandloc <- sl_locs %>% 
  filter(., is.na(sap_id) & is.na(location_description)) # 9.5k
nrow(na_sapandloc) / nrow(sl_locs) * 100  # about 15%

#now the top three
na_sapandloc <- sl_locs %>% 
  filter(., is.na(sap_id) & is.na(location_description)) # 9.5k
nrow(na_sapandloc) / nrow(sl_locs) * 100  # about 15%


#okay, the only columns that don't contain NAs are lat/lon so let's see how many rows have 6 NAs
na_six <- sl_locs %>% 
  filter(., is.na(sap_id) & is.na(location_description) & is.na(streetlight_model) & is.na(streetlight_type)
         & is.na(streetlight_wattage) & is.na(streetlight_voltage)) # 11
nrow(na_six) / nrow(sl_locs) * 100  # about 0.1%

# I think I have enough information to create a data set that is okay enough to create some interactive results. This data set is from 2017, so 
# maybe it has been updated. If not, there's a lot that could be done to validate these data. Based on the QA above, I think I'll use a data set with
# incomplete rows to start

# creating the data set for the shiny app made up of bad data
shiny_sl <- sl_locs[!complete.cases(sl_locs), ] %>% # this eliminates those rows with no NAs
  filter(., is.na(sap_id) & is.na(location_description)) %>%  # This removes rows with SAP IDs and location descriptions
  mutate(., )

# now looking for those duplicated (or triplicated, or more) rows
vars <- c(names(shiny_sl))
data.table::setDT(shiny_sl, key = vars) # sets up the data table
shiny_sl[, N := .N, by = key(shiny_sl)] # identifies duplicated rows and counts rows per group and adds the column, "N" to indicate how many times the row was duplicated
mulitple_rows <- shiny_sl[N > 1] # creates a new df of only multiple rows,

shiny_streetlight_issues <- union(mulitple_rows, shiny_sl) %>%  # Now union the two df together and you have a data set with a lot of rows (9526, almost 16%) of pretty bad data. 
write_csv(., path = paste(output,"shiny_streetlight_issues.csv"))# Let's map this in Shiny!




