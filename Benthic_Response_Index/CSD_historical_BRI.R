#######################################################################################################
# infauna.parameters
# Created: 26OCT2011
# Author: RNH nharing@sandiego.gov
# Description: Calculates univariate parameters and tabulates them for the Annual report
# Modifications:
#######################################################################################################
  
#Clear the workspace
rm(list=ls(all=TRUE))          

options(stringsAsFactors=FALSE)

#load Libraries
library(RODBC)
library(reshape2)
library(vegan)

############################ USER DEFINED SETUP START ######################################
SqlStartDate <- "01-JAN-83" # start date for data query to Oracle
SqlEndDate <- "31-DEC-10"   # end date for data query to Oracle
SqlProject <- paste("'PL'",",","'PL OLD'",",","'PL RECVRY'",",","'SED MAP'")  

Project <- "PLOO" # for csv files names (not SQL queries)
Year <- "2010"  # YYYY format for folder names and table/graph titles
Yr <- "10"          # YY format for file names

#Enter your user name (uid) and password for the database
uid <- "nick"
password <- "haring"

##########################  Output directories and file names #################################################
#Output directories
OutputDir <- paste("Y:\\EMTS\\41.Sections\\MBOO\\70.Work_Folders\\R_Translation\\R_Functions\\Infauna\\", sep = "") 
#Output file names
Outfile <- "CSD_historical_BRI.csv"
##########################  Raw Data ##########################################################################
#call the function benthic.extract to extract infauna data from oracle
source("Y:\\EMTS\\41.Sections\\MBOO\\70.Work_Folders\\R_Translation\\R_Functions\\Infauna\\Final_Functions\\InfaunaChapterFunctions.r")

Data1 <- benthic.extract(uid, password, OutputDir, Outfile, SqlStartDate, SqlEndDate, SqlProject)  #main dataset
Data <- subset(Data1, station %in% c("E14", "E17", "E11", "A15", "A16"))


#################################   Calculate univariate parameters ############################################


# Function to calculate BRI
calculate.BRI <- function(data,uid,password) {
                # BRI calculation
                # bri.calc
                # Author: JPS jpettisschal@sandiego.gov, RNH nharing@sandiego.gov
                # Created 10/2010, edited 11/4/2011
                                
                library(reshape2)
                
                # Import Reference table
                source("Y:\\EMTS\\41.Sections\\MBOO\\70.Work_Folders\\R_Translation\\R_Functions\\Infauna\\Final_Functions\\InfaunaChapterFunctions.R")
                reference <- pcode.extract(uid,password) 
                colnames(reference) <- tolower(colnames(reference))
                
                # Selecting necessary columns
                data <- data[,c("csdspcode","group","depth","abun")]  

                # Creating depth masks                
                mask.shallow <- data$depth >= 10 & data$depth < 25    
                mask.shallowmid <- data$depth >=25 & data$depth <= 35
                mask.mid <- data$depth > 35 & data$depth <110
                mask.middeep <- data$depth >=110 & data$depth <= 130
                mask.deep <- data$depth >130 & data$depth<=324

                # Assigning depth bin values
                data$depth.bin1 <- NA
                data$depth.bin2 <- NA
                data$depth.bin1[mask.shallow | mask.shallowmid] <- "shallow"
                data$depth.bin1[mask.mid | mask.middeep] <- "mid"
                data$depth.bin1[mask.deep] <- "deep"
                data$depth.bin2[mask.shallowmid] <- "mid"
                data$depth.bin2[mask.middeep] <- "deep"

                # Melt the data again to calculate the BRI for both depths in those that have two depths
                data.melt <- melt(data, c("csdspcode","group","depth","abun"),na.rm=T)
                colnames(data.melt)[5] <- "bin"
                colnames(data.melt)[6] <-"depth_zone"
                
                # Merge the reference table with the melted data table to get the pvalues
                merge.data <- merge(data.melt,reference)
                merge.data <- merge.data[!is.na(merge.data$p_value) & merge.data$abun>0,]
                
                # Aggregate to get the sum of all species abundance by station by depth by quarter by grab (make sure there is no duplicate pcodes listed in a sample - sometimes more than one spp per spcode?)
                abun.sum <- aggregate(merge.data$abun,by=list(merge.data$csdspcode,merge.data$depth_zone,merge.data$group, merge.data$depth, merge.data$bin, merge.data$p_code, merge.data$p_value),sum)
                colnames(abun.sum) <- c("csdspcode","depth_zone","group","depth","bin","p_code","p_value","abun")
                
                # Sum across numerators
                abun.sum$denominator <- abun.sum$abun^0.3333
                abun.sum$numerator<-abun.sum$p_value*(abun.sum$abun^0.3333)
                
                # Summing all numerator and denominator values by station, quarter grab, and bin (depth bin 1 and depth bin 2 for those that fall in between two depth bins)
                bri.sums <- aggregate(cbind(abun.sum$numerator,abun.sum$denominator),by=list(abun.sum$group, abun.sum$bin),sum)
                colnames(bri.sums) <- c("group","bin","numerator","denominator")
                
                # Calculate BRI
                bri.sums$bri <- bri.sums$numerator/bri.sums$denominator
                
                # Using aggregate to get a mean BRI between depth bin 1 and depth bin 2 (for those that fall in between two depth bins) - for those in one depth zone they will only have one BRI value so that value will be cast
                bri.depth.agg <- aggregate(bri.sums$bri, by=list(bri.sums$group),mean)
                colnames(bri.depth.agg) <- c("group","BRI") 
                return(bri.depth.agg)

} 





    #Use melt function to get grab1 and grab2 vertical
    data.melt <- melt(Data,id.vars=c("csdspcode","name","bri.name","phylum","taxa.group","class","order","family","site","station","date","quarter","depth","project"))
    colnames(data.melt)[15]<- "grab"
    colnames(data.melt)[16]<- "abun"
    data.melt$grab <- as.character(data.melt$grab)

    #Getting rid of grabs (the animal only occurred in one grab) with an abundance of "0" 
    if(any(is.na(data.melt$abun))) cat ("NAs in data.melt$abun")
    univariate <- data.melt[data.melt$abun!=0 & !is.na(data.melt$abun),]
    if(any(is.na(univariate$abun))) cat ("NAs in univariate$abun")  
    univariate$group <- paste(univariate$station, univariate$quarter, univariate$grab, sep = "_")

    BRI <- calculate.BRI (univariate, uid, password) # runs the function

    unique.depth <- data.frame(aggregate(univariate$depth, list(univariate$group), unique))

    parameters <- data.frame(BRI, unique.depth$x)
      colnames(parameters) <- c("group", "BRI", "depth")
      split <- unlist(strsplit(as.character(parameters$group), "_", fixed = TRUE)) #splits the character vector at the "_"
      parameters$station <- split[seq(1,length(split),3)] # assigns station as the first character, then every third from there
      parameters$quarter <- split[seq(2,length(split),3)] # assigns quarter as the second character, then every third from there
      parameters$grab <- split[seq(3,length(split),3)] # assigns grab as the third character, then every third from there
      parameters$year <- substring(parameters$quarter, 1, 4)

   
    parameters$BRI <- round(parameters$BRI, digits = 3)

    ########################################## Export data ##############################################################################################
     write.csv(parameters, paste(OutputDir, Outfile, sep = ""), row.names=FALSE)               #writes the annual data to a .csv file
    #####################################################################################################################################################
