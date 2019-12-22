# Annual Report Data aqusition ROBDC
# Created: 1/12/11
# Author: RNH nharing@sandiego.gov
# Description: Pulls BRI Pcodes and species names
# Modifications:
#######################################################################################################

#Clear the workspace
rm(list=ls(all=TRUE))          
#load Libraries
library(RODBC)
library(reshape2)

##########################  Output directories and file names #######################
#Output directories
OutputDir <- paste("I:\\Work_Infauna\\BRI\\", sep = "") 

#Output file names
OutfileBRI <- paste("Pcodes.csv", sep = "")
############################ SQL queries and initial data frames #######################

#odbcDataSources(type=c("all")) #uncomment to view data source information - data source should be called critter9

#Create the connection between R and Oracle; login to Oracle
channel <- odbcConnect("critter9",uid="nick",pwd="haring")

#BRI SQL Statement 
statementBRI <- paste(
"SELECT 
  Substr(M.KEY_TAXA_NAME, 1,50),
  P.CSDSPCODE,
  P.P_CODE,
  P.P_VALUE,
  P.DEPTH_ZONE
FROM MASTER_TAXA M,
     XR_BRI_PCODE P
WHERE M.CSDSPCODE = P.CSDSPCODE
ORDER BY P_CODE, CSDSPCODE, DEPTH_ZONE;"
  ,sep="") 

#Submit a SQL query to Oracle, and create a data frame
BRIReference <- sqlQuery(channel, statementBRI, errors=FALSE, rows_at_time=1024)
colnames(BRIReference) <- c("name", "spcode", "p.code", "p.value", "depth.zone") #renames the columns from SQL
BRIReference$spcode <- factor(BRIReference$spcode) # converts spcode to factor

# Taxa name, Pcodes, Pvalues

dcastPvalues <- dcast(BRIReference, name + spcode + p.code ~ depth.zone, value_var="p.value")
Pvalues <- dcastPvalues[,c("name","p.code","spcode","shallow","mid","deep")]

########################################## Export data ##########################################
#write.csv(Pvalues,paste(OutputDir,OutfileBRI,sep=""),row.names=F)                                # Writing the output file




