#
# Shelf BRI calculation tool
# Created: 4-Mar-2011
# Authors: Nick Haring nharing@sandiego.gov, JPS jpettisschal@sandiego.gov, 
# Description: Calculates Shelf BRI from infaunal grab data. Will calculate and output BRI values per station for one or two grabs. 
# NOTE: The latest instructions, P-value assingments, literature, and instructions can be found at www.SCAMIT.org or www.SCCWRP.org
#       Please review the BRI conventions documentation before calculating BRI.
#
# Modifications:
#
#
#######################################################################################################


#Clear the workspace 
rm(list=ls(all=TRUE))


####################################### Begin User Input #######################################
# The following path names and file names should be altered by the user here.  Note the R format for path names.

#  Set input directories and filenames
inputDir <- "C:\\Documents and Settings\\nharing\\Desktop\\BRI\\input\\"                 # Enter directory to where your INPUT file is located, for example: "C:/Desktop/" or "C:\\Desktop\\"
refFile <- "Pcodes.csv"   # Add the file name for the reference data here.  This is the list of taxa with corresponding P values for each depth zone
dataFile <- "BATMAN_BRI_Test dataset.csv"                        # Add the file name of the abundance data here.  This file has taxa names, abundance, station, and depth data

# Set output directories and filenames
OutputDir <- "C:\\Documents and Settings\\nharing\\Desktop\\BRI\\output\\"                 # Enter directory to where your OUTPUT file is located, for example: "C:/Desktop/" or "C:\\Desktop\\"
Outfile <- "BRI.csv" 
Outfile2 <- "BRIData.csv"# You can change the name of your output file if you prefer
####################################### End User Input #######################################

# Inputing file names

# Input Raw Data
# Suggested reference data headers: taxa.name, PCode, PDefinition, shallow, mid, deep
# Suggested abundance data headers: taxa.name, grab1, [grab2], station, depth)

openRef <- paste(inputDir, refFile, sep = "")
openData <- paste(inputDir, dataFile, sep = "")

Pvalues <- read.table(openRef, header = TRUE, sep = ",")
Data <- read.table(openData, header = TRUE, sep = ",")
colnames(Pvalues)<-c("taxa.name", "PCode","PDefinition","shallow", "mid", "deep") # reanames columns

####################################### Calculate BRI (if 2 grabs) #######################################
if(length(Data[1,])==5) {

  colnames(Data) <- c("taxa.name","grab1","grab2","station","depth")

  #Summing abundances across BRI_Species for Grab 1 and Grab 2 (NA's are omitted)
  BRIData <- aggregate(cbind(Data$grab1, Data$grab2), by=list(Data$taxa.name, Data$station, Data$depth), FUN=sum, simplify=T)
  colnames(BRIData) <- c("taxa.name", "station", "depth", "grab1", "grab2")

  #Setting up to get pvalues
  nameMatch <- match(BRIData$taxa.name, Pvalues$taxa.name)

  # Getting pvalues for stations which the depth overlaps two categories
  BRIData$pvalue.zone1 <- array(NA,dim=length(nameMatch))
  BRIData$pvalue.zone2 <- array(NA,dim=length(nameMatch))
  

  # Making a series of masks to define the p-values for each data value
  mid1<-which(BRIData$depth >=25 & BRIData$depth <=35)  
   if(length(mid1)>0) {
	BRIData$pvalue.zone1[mid1]<-Pvalues$shallow[nameMatch[mid1]]
	BRIData$pvalue.zone2[mid1]<-Pvalues$mid[nameMatch[mid1]]
    }
					
  mid2<-which(BRIData$depth >=110 & BRIData$depth <=130)
   if(length(mid2)>0) {
	BRIData$pvalue.zone1[mid2]<-Pvalues$mid[nameMatch[mid2]]
	BRIData$pvalue.zone2[mid2]<-Pvalues$deep[nameMatch[mid2]]
   } 

  # Getting pvalue for samples that do not overlap 2 depth bins 
  bin1<-which(BRIData$depth >= 10 & BRIData$depth < 25) 
  if(length(bin1)>0) BRIData$pvalue.zone1[bin1] <- Pvalues$shallow[nameMatch[bin1]]
					
  bin2<-which(BRIData$depth > 35 & BRIData$depth < 110)
  if(length(bin2)>0) BRIData$pvalue.zone1[bin2] <- Pvalues$mid[nameMatch[bin2]]
					
  bin3<-which(BRIData$depth >130 & BRIData$depth <= 324) 
  if(length(bin3)>0) BRIData$pvalue.zone1[bin3] <- Pvalues$deep[nameMatch[bin3]]

	
  # Making BRI Numerator and Denominators for Grabs 1 and 2 along with each depth category
  BRIData$numG1.zone1 <- BRIData$pvalue.zone1 * (BRIData$grab1^0.3333) 
  BRIData$numG1.zone2 <- BRIData$pvalue.zone2 * (BRIData$grab1^0.3333) 
  BRIData$DenomG1 <- BRIData$grab1^0.3333

  BRIData$numG2.zone1 <- BRIData$pvalue.zone1*(BRIData$grab2^0.3333)
  BRIData$numG2.zone2 <- BRIData$pvalue.zone2*(BRIData$grab2^0.3333)  
  BRIData$DenomG2 <- BRIData$grab2^0.3333
	
  #Getting out "na" for zone 1 and zone 2
  BRIDataZ1 <- BRIData[!is.na(BRIData$numG1.zone1),]
  BRIDataZ2 <- BRIData[!is.na(BRIData$numG1.zone2),]

  # Summing across species for Grabs 1 and 2 Numerators and denominators by date/quarter/station
  resultsZone1 <- aggregate(cbind(BRIDataZ1$numG1.zone1, BRIDataZ1$numG2.zone1, BRIDataZ1$DenomG1, BRIDataZ1$DenomG2), by=list(BRIDataZ1$station, BRIDataZ1$depth), FUN=sum, simplify=T)
  colnames(resultsZone1) <- c("station", "depth", "numG1.zone1", "numG2.zone1", "denomG1.zone1", "denomG2.zone1")
	
  resultsZone2 <- aggregate(cbind(BRIDataZ2$numG1.zone2, BRIDataZ2$numG2.zone2, BRIDataZ2$DenomG1, BRIDataZ2$DenomG2), by=list(BRIDataZ2$station, BRIDataZ2$depth), FUN=sum, simplify=T)
  colnames(resultsZone2) <- c("station", "depth", "numG1.zone2", "numG2.zone2", "denomG1.zone2", "denomG2.zone2")

  # Calculating the BRI for each
  #zone 1
  resultsZone1$BRI.g1z1 <- resultsZone1$numG1.zone1 / resultsZone1$denomG1.zone1
  resultsZone1$BRI.g2z1 <- resultsZone1$numG2.zone1 / resultsZone1$denomG2.zone1

  #zone 2
  resultsZone2$BRI.g1z2 <- resultsZone2$numG1.zone2 / resultsZone2$denomG1.zone2
  resultsZone2$BRI.g2z2 <- resultsZone2$numG2.zone2 / resultsZone2$denomG2.zone2

  #Merging results to calculate BRI average where they are across two depth zones
  resultsAll <- merge(resultsZone1, resultsZone2, all.x=T, all.y=T)

  resultsAll$BRI.avgG1 <- (resultsAll$BRI.g1z1 + resultsAll$BRI.g1z2) / 2 
  resultsAll$BRI.avgG2 <- (resultsAll$BRI.g2z1 + resultsAll$BRI.g2z2) / 2 

  #Getting final BRI for grab 1
  resultsAll$BRI.grab1 <- array(NA,length(resultsAll$station))
  resultsAll$BRI.grab1[is.na(resultsAll$numG1.zone2)] <- resultsAll$BRI.g1z1[is.na(resultsAll$numG1.zone2)]
  resultsAll$BRI.grab1[!is.na(resultsAll$numG1.zone2)] <- resultsAll$BRI.avgG1[!is.na(resultsAll$numG1.zone2)]

  #Getting final BRI for grab 2
  resultsAll$BRI.grab2 <- array(NA,length(resultsAll$station))
  resultsAll$BRI.grab2[is.na(resultsAll$numG2.zone2)] <- resultsAll$BRI.g2z1[is.na(resultsAll$numG2.zone2)]
  resultsAll$BRI.grab2[!is.na(resultsAll$numG2.zone2)] <- resultsAll$BRI.avgG2[!is.na(resultsAll$numG2.zone2)]

  # Getting only wanted columns
  BRI <- round(resultsAll[,c("station", "depth","BRI.grab1","BRI.grab2")], digits=3)
 
} #end if grabs==2

################################################# Calculate BRI (if 1 grab) ########################################
if(length(Data[1,])==4) {
  colnames(Data) <- c("taxa.name","grab1","station","depth")
 
  #Summing abundances across BRI_Species for Grab 1 and Grab 2 (NA's are omitted)
  BRIData <- aggregate(cbind(Data$grab1), by=list(Data$taxa.name, Data$station, Data$depth), FUN=sum, simplify=T)
  colnames(BRIData) <- c("taxa.name", "station", "depth", "grab1")

  #Setting up to get pvalues
  nameMatch <- match(BRIData$taxa.name, Pvalues$taxa.name)

  # Getting pvalues for stations which the depth overlaps two categories
  BRIData$pvalue.zone1 <- array(NA,dim=length(nameMatch))
  BRIData$pvalue.zone2 <- array(NA,dim=length(nameMatch))
  

  # Making a series of masks to define the p-values for each data value
  mid1<-which(BRIData$depth >=25 & BRIData$depth <=35)  
   if(length(mid1)>0) {
	BRIData$pvalue.zone1[mid1]<-Pvalues$shallow[nameMatch[mid1]]
	BRIData$pvalue.zone2[mid1]<-Pvalues$mid[nameMatch[mid1]]
    }
					
  mid2<-which(BRIData$depth >=110 & BRIData$depth <=130)
   if(length(mid2)>0) {
	BRIData$pvalue.zone1[mid2]<-Pvalues$mid[nameMatch[mid2]]
	BRIData$pvalue.zone2[mid2]<-Pvalues$deep[nameMatch[mid2]]
   } 

  # Getting pvalue for samples that do not overlap 2 depth bins 
  bin1<-which(BRIData$depth >= 10 & BRIData$depth < 25) 
  if(length(bin1)>0) BRIData$pvalue.zone1[bin1] <- Pvalues$shallow[nameMatch[bin1]]
					
  bin2<-which(BRIData$depth > 35 & BRIData$depth < 110)
  if(length(bin2)>0) BRIData$pvalue.zone1[bin2] <- Pvalues$mid[nameMatch[bin2]]
					
  bin3<-which(BRIData$depth >130 & BRIData$depth <= 324) 
  if(length(bin3)>0) BRIData$pvalue.zone1[bin3] <- Pvalues$deep[nameMatch[bin3]]

	
  # Making BRI Numerator and Denominators for each depth category
  BRIData$numG1.zone1 <- BRIData$pvalue.zone1 * (BRIData$grab1^0.3333) 
  BRIData$numG1.zone2 <- BRIData$pvalue.zone2 * (BRIData$grab1^0.3333) 
  BRIData$DenomG1 <- BRIData$grab1^0.3333
	
  #Getting out "na" for zone 1 and zone 2
  BRIDataZ1 <- BRIData[!is.na(BRIData$numG1.zone1),]
  BRIDataZ2 <- BRIData[!is.na(BRIData$numG1.zone2),]

  # Summing across species' numerators and denominators by date/station
  resultsZone1 <- aggregate(cbind(BRIDataZ1$numG1.zone1, BRIDataZ1$DenomG1), by=list(BRIDataZ1$station, BRIDataZ1$depth), FUN=sum, simplify=T)
  colnames(resultsZone1) <- c("station", "depth", "numG1.zone1", "denomG1.zone1")
	
  resultsZone2 <- aggregate(cbind(BRIDataZ2$numG1.zone2,BRIDataZ2$DenomG1), by=list(BRIDataZ2$station, BRIDataZ2$depth), FUN=sum, simplify=T)
  colnames(resultsZone2) <- c("station", "depth", "numG1.zone2", "denomG1.zone2")

  # Calculating the BRI for each
  #zone 1
  resultsZone1$BRI.g1z1 <- resultsZone1$numG1.zone1 / resultsZone1$denomG1.zone1
 
  #zone 2
  resultsZone2$BRI.g1z2 <- resultsZone2$numG1.zone2 / resultsZone2$denomG1.zone2
  
  #Merging results to calculate BRI average where they are across two depth zones
  resultsAll <- merge(resultsZone1, resultsZone2, all.x=T, all.y=T)

  resultsAll$BRI.avgG1 <- (resultsAll$BRI.g1z1 + resultsAll$BRI.g1z2) / 2 
 

  #Getting final BRI for grab 1
  resultsAll$BRI.grab1 <- array(NA,length(resultsAll$station))
  resultsAll$BRI.grab1[is.na(resultsAll$numG1.zone2)] <- resultsAll$BRI.g1z1[is.na(resultsAll$numG1.zone2)]
  resultsAll$BRI.grab1[!is.na(resultsAll$numG1.zone2)] <- resultsAll$BRI.avgG1[!is.na(resultsAll$numG1.zone2)]

  # Getting only wanted columns
  BRI <- resultsAll[,c("station", "depth","BRI.grab1")]
}# End the 'if 1 grabs' bracket 


########################################## Export BRI results ##########################################
# Writing the output file
#write.csv(BRI,paste(OutputDir,Outfile,sep=""),row.names=F) 
#write.csv(BRIData,paste(OutputDir,Outfile2,sep=""),row.names=F) #outputs raw data for QA/QC










