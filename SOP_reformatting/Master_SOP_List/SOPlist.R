#
# EMTS SOP list
# Created: Mar-2018
# Authors: Nick Haring nharing@sandiego.gov, Zoe Scott zscott@sandiego.gov, and Connie Xiong 
# Description: EMTS has >300 standard operating procedures in .docx format. The code scrapes the title text from the  SOPs
#              identifies keywords, and renames the files in a standardized format
# Modifications:
#
########################################################################################################

# Load Packages----
rm(list = ls(all = TRUE))

if (!require(officer)) {
  install.packages("officer")
  library(officer)
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(qdapTools)) {
  install.packages("qdapTools")
  library(qdapTools)
}
if (!require(magrittr)) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require(docxtractr)) {
  install.packages("docxtractr")
  library(docxtractr)
}

# Bring in TNI-SOPs----
# change working directory to spot in QA folder (outside project)
# Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/
# 6.0_Divisional_SOPs_and_procedures/SOPs/TNIformattedSOPs
setwd("Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/6.0_Divisional_SOPs_and_procedures/SOPs/TNIformattedSOPs")

IWCP <- list.files(
  path = "./IWCP - Formatted",pattern = ".docx", full.names = FALSE
)
MM <- list.files(
  path = "./MarineMicro - Formatted",pattern = ".docx", full.names = FALSE
)
MBOO <- list.files(
  path = "./MBOO - Formatted",pattern = ".docx", full.names = FALSE
)
TOX <- list.files(
  path = "./Toxicology - Formatted",pattern = ".docx", full.names = FALSE
)
WQCS <- list.files(
  path = "./WQCS - Formatted",pattern = ".docx", full.names = FALSE
)
ECS <- list.files(
  path = "./ECS - Formatted",pattern = ".docx", full.names = FALSE
)


# change working directory back
setwd("H:/2.0 R/RProjects/MasterSOPList_ZS")
getwd() # check that it worked 

# Lists to DF----
iwcp <- plyr::ldply(IWCP, data.frame) %>%
 dplyr::mutate(., section = "IWCP")
mm <- plyr::ldply(MM, data.frame) %>%
  dplyr::mutate(., section = "MM")
mboo <- plyr::ldply(MBOO, data.frame) %>%
  dplyr::mutate(., section = "MBOO")
tox <- plyr::ldply(TOX, data.frame) %>%
  dplyr::mutate(., section = "TOX")
tox$X..i.. <-  stringr::str_replace_all(tox$X..i.., "^\\d+\\.","")
wqcs <- plyr::ldply(WQCS, data.frame) %>%
  dplyr::mutate(., section0 = "WQCS")
wqcs <- dplyr::mutate(wqcs, section = # adjust section names to include DWM as section
                  dplyr::case_when(
                    grepl("^MIC|PRP|RES|SAM|WSM", wqcs$X..i..) ~ "DWM",
                    grepl("WQCS", wqcs$section0) ~ "WQCS")) %>%
  select(., X..i.., section)
wqcs$X..i.. <- stringr::str_replace_all(wqcs$X..i.., "WQL", "WQCS")
ecs <- plyr::ldply(ECS, data.frame) %>%
  dplyr::mutate(., section = "ECS")

ALL.of <- c(IWCP, MM, MBOO, TOX, WQCS, ECS) # combine lists from earlier to have reference doc name
all.of <- plyr::ldply(ALL.of, data.frame)

# Combine to one DF----
Asops <- rbind(iwcp, mm, mboo, tox, wqcs, ecs) %>%
  dplyr::rename(., sop0 = X..i..) %>% # renames column to sop0; base for further column recreation
  dplyr::mutate( # column with running number of all SOPs
    sopNumber = row_number()
  ) %>%
  dplyr::mutate(
    ofn = ALL.of
  ) %>%
  dplyr::mutate(.,  # creates column where all rows are "SOP", helps with naming standardization
               doc = "SOP") %>%
  group_by(section) %>%
    dplyr::mutate(.,  # creates unique number per SOP
                 number0 = row_number()) %>%
  dplyr::mutate(.,  # creates unique number per SOP
                pad = 
                  dplyr::case_when(
                    number0 < 10 ~ "00",
                    number0 > 9 & number0 <100 ~ "0",
                    number0 >99 ~ "")
                ) %>%
  tidyr::unite(., "number", c("pad", "number0"), sep = "", remove = TRUE) %>%
  dplyr::mutate(., # remove _formatted.docx from end of file name
    oldName =
      dplyr::case_when(
        grepl("_formatted.docx$", sop0) ~ stringr::str_replace_all(sop0, "_formatted.docx$", ""),
        grepl("-formatted.docx$", sop0) ~ stringr::str_replace_all(sop0, "-formatted.docx$", ""),
        grepl("_fomatted.docx$", sop0) ~ stringr::str_replace_all(sop0, "_fomatted.docx$", ""),
        grepl("_TNI.docx$", sop0) ~ stringr::str_replace_all(sop0, "_TNI.docx$", ""),
        grepl("\\.docx$", sop0) ~ stringr::str_replace_all(sop0, "\\.docx$", "")
        )) %>%
  # regex to the rescue
  dplyr::mutate(.,
                slug00 = 
                  dplyr::case_when(
                    section %in% "WQCS" & grepl("^MET\\-\\d+\\-\\d+\\s|MET0507_|MIC\\-\\d+\\-\\d+\\s|ORG\\-\\d+\\-\\d+\\s|
                                                PC\\-\\d+\\-\\d+\\s|PRP\\-\\d+\\-\\d+\\s|SAM0205\\_|^WQCS\\-\\SOP\\-\\d+\\s", oldName)
                    ~ stringr::str_replace_all(oldName, "^MET\\-\\d+\\-\\d+\\s|MET0507_|MIC\\-\\d+\\-\\d+\\s|ORG\\-\\d+\\-\\d+\\s|
                                               PC\\-\\d+\\-\\d+\\s|PRP\\-\\d+\\-\\d+\\s|SAM0205\\_|^WQCS\\-\\SOP\\-\\d+\\s", ""),
                    section %in% "WQCS" & !grepl("^MET\\-\\d+\\-\\d+\\s|MET0507_|MIC\\-\\d+\\-\\d+\\s|ORG\\-\\d+\\-\\d+\\s|
                                                PC\\-\\d+\\-\\d+\\s|PRP\\-\\d+\\-\\d+\\s|SAM0205\\_", oldName)
                    ~paste0(oldName)
                  )) %>%
  # In case you want to add back in filtering for EPA Methods
  # EPA\\d+\\.\\d+\\s|EPA\\d+\\s|EPA\\s\\d+\\.\\d+\\s|EPA\\s\\d+\\s|
  
  dplyr::mutate( # create column for Reference Method (SM..., EPA...) if it is part of the name
    refMethod0 =
      dplyr::case_when(
        grepl("EPA200\\.8", slug00) ~ paste0(slug00),
        grepl("SM\\d+[[:alpha:]]|SM\\d+|EPA\\s\\d+\\.\\d+|EPA\\d+\\.\\d+|EPA\\s\\d+|EPA\\s\\d+\\s\\d|SRL\\d+\\w", slug00)
        ~ stringr::str_extract(slug00, "^SM\\d+[[:alpha:]]|EPA\\d+\\.\\d+|SM\\d+|EPA\\s\\d+\\.\\d+|EPA\\s\\d+|EPA\\s\\d+\\s\\d|SRL\\d+\\w"),
        grepl("^EPA\\d+[[:alpha:]]|EPA\\d+\\s\\d|EPA\\d+|EPA", slug00)
        ~ stringr::str_extract(slug00, "^EPA\\d+[[:alpha:]]|EPA\\d+\\s\\d|EPA\\d+|EPA"),
        grepl("^EPA\\d+\\.\\d+", oldName)
        ~ stringr::str_extract(oldName, "^EPA\\d+\\.\\d+")
      )
  ) %>%
  dplyr::mutate( #add a space between EPA and the number, whereas SM1234 is correct.
    refMethod = 
      dplyr::case_when(
        grepl("SM", refMethod0) ~ paste0(refMethod0), # copy over SM methods
        grepl("^EPA\\s", refMethod0) ~ paste0(refMethod0),
        grepl("^EPA", refMethod0) ~ stringr::str_replace_all(refMethod0, "^EPA","EPA "),
        grepl("SRL", refMethod0) ~ paste0(refMethod0)
      ) 
  ) %>%

  dplyr::mutate(., # Cleaning up the names in "oldName" by group
                slug0 = 
                  dplyr::case_when(
                    section %in% "IWCP" & grepl("\\_IWL\\_SOP\\_\\d+\\.\\d+", oldName) 
                    ~ stringr::str_replace_all(oldName, "\\_IWL\\_SOP\\_\\d+\\.\\d+", ""),
                    section %in% "IWCP" & grepl("\\_IWCP\\-SOP\\-\\d+\\.\\d+", oldName)
                    ~ stringr::str_replace_all(oldName, "\\_IWCP\\-SOP\\-\\d+\\.\\d+", ""),
                    section %in% "MBOO" & grepl("^SOP\\_", oldName) 
                    ~ stringr::str_replace_all(oldName, "^SOP\\_", ""),
                    section %in% "MBOO" & grepl("^SOP\\_", oldName) 
                    ~ stringr::str_replace_all(oldName, "^SOP\\_", ""),
                    section %in% "MM" ~ paste0(oldName),
                    section %in% "ECS" & grepl("\\_", oldName)
                    ~ stringr::str_replace_all(oldName, "\\_", "-"),
                    section %in% "ECS" & !grepl("\\_", oldName)
                    ~ paste0(oldName),
                    section %in% "TOX" & grepl("\\-TX\\d+\\-SOP\\-V\\d\\w\\-\\d+\\w+\\d+", oldName) 
                    ~ stringr::str_replace_all(oldName, "\\-TX\\d+\\-SOP\\-V\\d\\w\\-\\d+\\w+\\d+", ""),
                    section %in% "TOX" & grepl("\\_2017\\sUpdated", oldName) 
                    ~ stringr::str_replace_all(oldName, "\\_2017\\sUpdated", ""),
                    section %in% "TOX" & grepl ("\\s\\d+\\_MF", oldName)
                    ~ stringr::str_replace_all(oldName, "\\s\\d+\\_MF", ""),
                    section %in% "TOX" & grepl("\\_revised\\s\\d+\\w", oldName) 
                    ~ stringr::str_replace_all(oldName, "\\_revised\\s\\d+\\w", ""),
                    section %in% "TOX" & grepl("[[:alpha:]]", oldName) 
                    ~ paste0(oldName),
                    section %in% "DWM" & grepl("^MIC\\-\\d+\\-\\d+|^PRP\\-\\d+\\-\\d+|^SAM0205\\_", oldName)
                    ~ stringr::str_replace_all(oldName, "^MIC\\-\\d+\\-\\d+|^PRP\\-\\d+\\-\\d+|^SAM0205\\_", ""),
                    section %in% "WQCS" & grepl("^PC\\-\\d+\\-\\d+|^WQL\\-SOP\\s010\\sEPA\\s548.1", oldName)
                    ~ stringr::str_replace_all(oldName, "^PC\\-\\d+\\-\\d+|^WQL\\-SOP\\s010\\sEPA\\s548.1", ""),
                    grepl("EPA200\\.8", slug00) ~ paste0(slug00),
                    grepl("^SM\\d+[[:alpha:]]\\s|SM\\d+\\s|SM\\d+\\w\\s|EPA\\s\\d+\\.\\d+\\s|EPA\\d+\\.\\d+|EPA\\s\\d+\\s|EPA\\s\\d+\\s\\d\\s|SRL\\d+\\w", slug00)
                    ~ stringr::str_remove_all(slug00, "^SM\\d+[[:alpha:]]\\s|SM\\d+\\s|SM\\d+\\w\\s|EPA\\d+\\.\\d+|EPA\\d+\\.\\d+|EPA\\s\\d+\\.\\d+\\s|EPA\\s\\d+\\s|EPA\\s\\d+\\s\\d\\s|SRL\\d+\\w"),
                    grepl("^EPA\\d+[[:alpha:]]|^EPA\\d+[[:alpha:]]\\.\\d+|EPA\\d+|EPA|^SM\\d+\\_", slug00)
                    ~ stringr::str_remove_all(slug00, "^EPA\\d+[[:alpha:]]|^EPA\\d+[[:alpha:]]\\.\\d+|EPA\\d+\\s\\d|EPA\\d+|EPA|^SM\\d+\\_"),
                    !grepl("^EPA\\d+\\.\\d+\\s|EPA\\d+\\s\\d|SM\\d+[[:alpha:]]\\s|SM\\d+\\s|EPA\\s\\d+\\.\\d+\\s|EPA\\s\\d+\\s|EPA\\s\\d+\\s\\d\\s|SRL\\d+\\w", slug00)
                    ~ paste0(slug00)
                  )
  ) %>%
    
  dplyr::mutate(., # removing spaces and _ from names in title
                slug1 = str_replace_all(slug0, "\\_\\s|\\s\\-\\s|\\s|\\_", "-")
  ) %>%
  dplyr::mutate(.,
                group = 
                  dplyr::case_when(
                    grepl("ORG|PRP|PC\\-|SAM", oldName) 
                    ~ stringr::str_extract(oldName, "ORG|PRP|PC|SAM")
                  )) %>%
tidyr::unite(., "slug2", c("group", "slug1"), sep = "-", remove = FALSE) %>%
  dplyr::mutate(.,
                slug3 = stringr::str_replace_all(slug2, "NA\\-\\-|NA\\-", "")) %>%
 dplyr::mutate(., # replace all double hyphens
                slug = stringr::str_replace_all(slug3, "\\-\\-", "")) %>%

  # __Create newName----
  tidyr::unite(., "docControl", c("section", "doc", "number"), sep = "-", remove = FALSE) %>%
  tidyr::unite(., "newName", c("docControl", "slug"), sep = "_", remove = FALSE) %>%
  # add the rest of the columns, can code some of these in later
  tibble::add_column(., revisionDate = "", author = "", TNI = "Y", StdLang = "")%>%
select(., ofn, sopNumber, section, group, revisionDate, author, oldName, newName, TNI, refMethod, StdLang)

  # Create the csv----
write.csv(Asops, file = "EMTSMasterSOPList.csv", row.names = FALSE)
getwd()

# Rename files----
# need to create a df containing full file paths for the SOPs in question, and a matching 
ALL.of <- c(IWCP, MM, MBOO, TOX, WQCS, ECS) # combine lists from earlier to have reference doc name

# change working directory
setwd("Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/6.0_Divisional_SOPs_and_procedures/SOPs/TNIformattedSOPs")

# __create lookup----

name_lookup <- plyr::ldply(Asops$ofn, data.frame) %>%
  dplyr::mutate(
    nfn0 = Asops$newName
  ) %>%
  dplyr::mutate(
    nfn1 = ".docx"
  ) %>%
  tidyr::unite(., "nfn", c("nfn0", "nfn1"), sep = "", remove = T)

# __create df ofn/nfn----
all <- plyr::ldply(list.files( # create df with old file path (=Asops$sop0)
  path = "Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/6.0_Divisional_SOPs_and_procedures/SOPs/TNIformattedSOPs/ALL",pattern = ".docx", full.names = T), data.frame) %>%
  dplyr::rename(., ofp0 = X..i..) %>% # rename column to Old File path
  dplyr::mutate(
    ofp = as.character(ofp0) 
  ) %>% 
  dplyr::mutate(
    ofn = ALL.of
  ) %>%
  dplyr::mutate(
    nfp0 = "Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/6.0_Divisional_SOPs_and_procedures
    /SOPs/TNIformattedSOPs/ALL"
  ) %>%
  dplyr::mutate(
    nfn = qdapTools::lookup(ofn, name_lookup)
  ) %>%
  tidyr::unite(., "nfp", c("nfp0", "nfn"), sep = "", remove = F) %>%
  select(., ofp, ofn, nfp, nfn)


# change working directory back to ALL
setwd("Y:/EMTS/41.Sections/DMQA/QA/Quality_Systems/1.0 TNI/6.0_Divisional_SOPs_and_procedures/SOPs/TNIformattedSOPs/ALL")
getwd()
# mapply runs file.rename using the directory above, and matches the columns to change file names-SICK!
mapply(file.rename, all$ofn, all$nfn)
newpath <- gsub()
