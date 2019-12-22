#
# CSD Drinking Water sampling routes
#
# Authors: Nick Haring nharing@sandiego.gov, Zoe Scott, zscott@sandiego.gov 
# Description: applies a Traveling salesman calculation to find the most effecient DW sampling routes
#
# Modifications:
#
#
#######################################################################################################

# First, clear the workspace
rm(list = ls())

# Load Packages----
# __TSP----
if (!require(TSP)) {
  install.packages("TSP")
  library("TSP")
}
if (!require(maps)) {
  install.packages("maps")
  library("maps")
}
if (!require(sp)) {
  install.packages("sp")
  library("sp")
}
if (!require(maptools)) {
  install.packages("maptools")
  library("maptools")
}

# __ggplot/ggmap----
if (!require(ggmap)) {
  install.packages("ggmap")
  library("ggmap")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library("ggplot2")
}
if (!require(ggrepel)) {
  install.packages("ggrepel")
  library("ggrepel")
}
if (!require(magrittr)) {
  install.packages("magrittr")
  library("magrittr")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library("tidyverse")
}


# Load Data------
# upload sampling sites
sites <- read.csv("allsites.csv", stringsAsFactors = FALSE) 
View(sites)

# Compare sites----
# __US to them
# Bring in Nicole's sites to compare
compass <- read.csv("Compass Water Sampling Locations_2018-08-08.csv", stringsAsFactors = FALSE) 
# pare down to just the SYS names to create a lookup table
csites <- dplyr::select(compass, FAC_NAME) %>%
  mutate(
    match = "TRUE"
  )

# add that compare column to sites
sites1 <- mutate(sites, 
    site_match =
      dplyr::case_when(
        !is.na(distID) ~ qdapTools::lookup(
          distID, csites)
        )
      ) %>%
  dplyr::select(distID, site_match, address:comments)
  
dplyr::group_by(sites1, site_match)
  

# create a dataframe of those that did not match
nomatch <- dplyr::filter(sites1, is.na(site_match))
# write CSV of those that Nicole's list doesn't have
write.csv(sites1, file = "2018-08-14_Compare-SYS-Sites.csv")

# __them to US
# pare down to just the SYS names to create a lookup table
ssites <-  as.data.frame(unique(sites$distID)) %>%
  mutate(
    match = "TRUE"
  )

# add that compare column to sites
compass1 <- mutate(compass, 
                 site_match =
                   dplyr::case_when(
                     !is.na(FAC_NAME) ~ qdapTools::lookup(
                       FAC_NAME, ssites)
                   )
) %>%
  dplyr::select(FAC_NAME, site_match, FAC_SEQ_NUM:SAPOBJNR)


# create a dataframe of those that did not match
nomatch <- dplyr::filter(sites1, is.na(site_match))
# write CSV of those that Nicole's list doesn't have
write.csv(compass1, file = "2018-08-14_Compare-NB-to-ZS-Sites.csv")

# ____________________________________________

# __subset Monday----
subsites <- sites %>% 
  # Monday only, exclude non-valid sites and plants from TSP
  dplyr::filter(., day == "Thu" & site_type != "plant") %>%
  # add back on row for ALV, the start/end point
  dplyr::bind_rows(., sites[sites$distID == "81 SYS" & sites$day == "Thu", ])


# solve_TSP----
# create your Etsp lat/long dataframe (Euclidean traveling salesperson problem (TSP)
sdtsp <- ETSP(data.frame(x = subsites$lon, y = subsites$lat), labels = subsites$distID) 
# solve the TSP via default method arbitrary_insertion+two_opt
sdtour <- solve_TSP(sdtsp)
plot(sdtsp, sdtour) # plot the tour for comparing later

# create dataframe from labels (in order) on trip for "lookup"
sdtourTrip <- as.data.frame(labels(sdtour)) %>%
  # add column with number orders
  dplyr::mutate(
    ord = row_number()
  )

# add order column to subsites while using lookup to determine order
subsites <- dplyr::mutate(subsites,
                          ord =
                            qdapTools::lookup(
                              distID, sdtourTrip
                            )
) %>%
  dplyr::arrange(., ord)

# subsites map----
# __get_map SD----
# Way 1: create plot from mean of subsites dataframe (only contains Monday, no plants)
# NOTE: cannot have "NA"s in the lat/lon columns
subsandiego <- get_map(location = c(lon = mean(subsites[ ,"lon"]),
                                 lat = mean(subsites[, "lat"])),
                    zoom = 10, maptype = "roadmap", source = "google")

sandiego <- get_map(location = c(lon = mean(allsites[ ,"lon"]),
                                    lat = mean(allsites[, "lat"])),
                       zoom = 10, maptype = "roadmap", source = "google")

# Way 2: create plot from box of max/min lat/long from Monday sites from "sites" dataset
# centers all points in the middle of the plot
# make the box - Monday
# NOTE: cannot have "NA"s in the lat/lon columns
right <- signif(max(sites[sites$day == "Thu","lon"]), 8)
left <- signif(min(sites[sites$day == "Thu","lon"]), 8) 
top <- signif(max(sites[sites$day == "Thu","lat"]), 8)
bottom <- signif(min(sites[sites$day == "Thu","lat"]), 8)
box <- c(left, bottom, right, top)
sandiego <- get_map(location = box,
                    zoom = 10, maptype = "roadmap", source = "google")

# After choosing Way 1/2, plot the map with sites as points
ggmap(sandiego) + # using "device" removes lat/long axes labels, gives warnings
  geom_point(data = subsites, aes(x = lon, y = lat), colour = "blue", size = 1) +
  # add path determined by TSP
  geom_path(data = subsites, color = "red") +
  # add Monday plants on in different color

  geom_point(data = sites[sites$site_type == "plant" & sites$day == "Thu", ], aes(x = lon, y = lat), 
             colour = "red", size = 1.5)

# text labels for sites
# geom_jitter(width = 0.1, height = 0.1) +
# geom_text_repel(data = TSPsites, aes(label = paste("  ", as.character(distID), sep="")), # prevent overlap of labels
#           angle = 0, direction = "both", hjust = 0, color = "black", size = 2.75) +



# allsites map----
# 32.780157, -117.110713
# __get map----
allsites0 <- read.csv("allsites.csv", stringsAsFactors = FALSE)
allsites <- na.omit(allsites0) # remove NA lat/lons to plot points

# way 1: use mean point in SD
sandiego <- get_map(location = c(lon = -117.110713, 
                                 lat = 32.780157),
                    zoom = 10, maptype = "roadmap", source = "google")
# color pallete for graph
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

# way 2: box from all sites in SD
right <- signif(max(allsites[,"lon"]), 8)
left <- signif(min(allsites[,"lon"]), 8) 
top <- signif(max(allsites[,"lat"]), 8)
bottom <- signif(min(allsites[,"lat"]), 8)
box <- c(left, bottom, right, top)
sandiego <- get_map(location = box,
                    zoom = 10, maptype = "roadmap", source = "google")

# make the map
ggmap(sandiego) + # using "device" removes lat/long axes labels, gives warnings
  geom_point(data = allsites, aes(x = lon, y = lat, color = day, shape = site_type), 
             position = "jitter", size = 1) +
  scale_colour_manual(values=cbPalette)


# dummy code----
# all below is code to ignore
# __testMethods----
# test all the methods and compare with dot plot
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion",
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")
alltours <- sapply(methods, FUN = function(m) solve_TSP(sdtsp, method = m), simplify = FALSE)
# compare methods via dot graph
dotchart(sort(c(sapply(alltours, tour_length))),
         xlab = "tour length", xlim = c(1, 3))
# the defaults from before are the seemingly best options too
tour <- as.numeric(labels(sdtour)) # labels() returns a vector with the names of the cities in x (sdtour)
sites$tour <- tour # append that character vector to sites df

# # after solving TSP, get labels to reorder data for later creating route line in ggmap--UNSUCCESSFUL
# order <- labels(sdtour)
# sites$order <- order #this makes ordering funky because it's not in 001 format
# sites$order <- c(014, 012, 013, 010, 011, 009, 002, 004, 003, 015, 001, 007,  006, 005, 008)



# __osmar----
# don't like this one..

# load the osmar package
if (!require(osmar)) {
  install.packages("osmar")
  library("osmar")
}
if (!require(sp)) {
  install.packages("sp")
  library("sp")
}
if (!require(igraph)) {
  install.packages("igraph")
  library("igraph")
}

# ____example----
src <- osmsource_api(url = "https://overpass-api.de/api/map?bbox=-117.2894,32.5341,-116.8925,32.8986")
cb <- corner_bbox(-117.2894, 32.5341, -116.8925, 32.8986)

wb <- get_osm(cb, source = src) # throws a TON  of warning messages, not sure how to get around
wb
bb <- center_bbox(-1.53492, 53.81934, 1000, 1000)
bb
ctown <- get_osm(bb, source = src)
plot(ctown)
points(-1.53492, 53.81934, col = "red", lwd = 5)

