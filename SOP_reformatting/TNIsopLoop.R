#
# SOP reformatting
# Created: Mar-2018
# Authors: Nick Haring nharing@sandiego.gov, Zoe Scott zscott@sandiego.gov, and Connie Xiong 
# Description: EMTS has >300 standard operating procedures in .docx format. This code scrapes the text from the SOPs
#              identifies keywords and key sections then renames and reoganizes the text to standardize the format
# Modifications:
#
######################################################################################################### Step 0: Set up the packages and directory----

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
if (!require(flextable)) {
  install.packages("flextable")
  library(flextable)
}
if (!require(rmarkdown)) {
  install.packages("rmarkdown")
  library(rmarkdown)
}


rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE, show.error.locations = T)

# bring in EMTS v. TNI section title lookup tables ----
section_title_xrefrence <- read.csv("SectionTitlesLookupTable.csv")
section_title_lookup <- section_title_xrefrence %>%
  select(., EMTS_title, text)
section_number_lookup <- read.csv("SectionNumbersLookupTable.csv")
species_lookup <- read.csv("SpeciesLookupTable.csv")
TNI_titles <- section_title_xrefrence %>%
  # creates a data frame used to complete missing sections in SOPs
  select(., content_type, style_name, text, section, section_number) %>%
  distinct(., content_type, style_name, text, section, section_number, keep_all = FALSE)

##  Step 1: Load SOPs----
# create a list of file names based on a file directory
# if full.names = FALSE, may not be able to find all the SOPs you're looking for

SOPfiles <- list.files(
  path = "./ReadyToKnit", pattern = ".docx", full.names = TRUE
)
# NH changed full.names to = FALSE, still gives error but seems to work
filenames <- list.files(
  path = "./ReadyToKnit", pattern = ".docx", full.names = FALSE
) %>% # remove .docx from filename
  str_replace_all(., ".docx", "")
# NH changed full.names to = FALSE, still gives error but seems to work
SOPlist <- lapply(SOPfiles, function(x) officer::docx_summary(officer::read_docx(x)))
# apply filenames to each doc you bring in and run through officer's docx_summary function
# replace names in SOPtest with those from filenames
names(SOPlist) <- filenames

## Step 2: Tidy data----
# use package plyr and function ldply to lump all SOPs from SOPtest into one dataframe
sops0 <- plyr::ldply(SOPlist, data.frame) %>%
  dplyr::rename(.,
    sop = .id, line_number = doc_index, paragraph_level = level, text0 = text,
    paragraph_num_id = num_id
    , table_row = row_id, is_table_header = is_header, table_cell = cell_id
  ) %>%
  dplyr::filter(., !(style_name %in% c("toc 1", "toc 2", "toc 3", "toc 4"))) %>%
  # removes the toc rows.
  # Note: filter drops NAs so use %in% to retain them
  dplyr::mutate(
    text1 = trimws(text0)
  ) %>%
  dplyr::mutate(
    text2 =
      dplyr::case_when(
        # Identifies/deletes hand-typed/formatted numbering/lettering
        content_type %in% "paragraph" & grepl("^\\d+\\.\\d+\\.\\d+", text1) ~
        stringr::str_replace_all(text1, "^\\d+\\.\\d+\\.\\d+", ""),
        # any text with  0.0.0
        content_type %in% "paragraph" & grepl("^\\d+\\.\\d+", text1) ~
        stringr::str_replace_all(text1, "^\\d+\\.\\d+", ""),
        # "^\\d+\\.0" = any string that begins with a digit and ".0"
        content_type %in% "paragraph" & grepl("^\\d+\\.", text1) ~
        stringr::str_replace_all(text1, "^\\d+\\.", ""),
        # "^\\d+\\." = any string that begins with a digit and "."
        content_type %in% "paragraph" & grepl("^\\w\\.\\s", text1) ~
        stringr::str_replace_all(text1, "^\\w\\.\\s", ""),
        # "^\\w+\\." = any string that begins with a letter and "."
        content_type %in% "paragraph" & grepl("^\\w\\)", text1) ~
        stringr::str_replace_all(text1, "^\\w\\)", ""),
        # "^\\w+\\)" = any string that begins with a letter and ")"
        content_type %in% "paragraph" & grepl("^\\-", text1) ~
        stringr::str_replace_all(text1, "^\\-", "")
        # "^\\-\\s" = any string that begins with "-" and whitespace
        # content_type %in% "paragraph" & grepl("^|$\\s", text1) ~
        # stringr::str_replace_all(text1, "^|$\\s", "")
      )
  ) %>%
  dplyr::mutate(
    text3 =
      dplyr::case_when(
        # Identifies normal text and moves over
        content_type %in% "paragraph" & is.na(text2) & grepl("\\:$", text1) ~
          # For all text with ":" at the end, and doesn't start with number
          stringr::str_replace_all(text1, "\\:$", ""), 
        content_type %in% "paragraph" & grepl("[spores/]", text1) ~
        paste0(text1),
        content_type %in% "paragraph" & grepl("^[[:alpha:]]+", text1) ~
        paste0(text1), # For all normal text
        content_type %in% "paragraph" & grepl("^\\(+", text1) ~
        paste0(text1), # For all text that starts with ()
        content_type %in% "paragraph" & grepl("^\\d+\\sml", text1) ~
        paste0(text1), # For text that starts with a number of ml (e.g. 100 ml)
        content_type %in% "paragraph" & grepl("^\\d+L", text1) ~
        paste0(text1), # For text that starts with a number of L, no space (e.g.)
        content_type %in% "paragraph" & grepl("^\\d/\\d.", text1) ~
        paste0(text1), # for text that starts with number/fraction
        content_type %in% "paragraph" & grepl("^\\d+-\\w", text1) ~
        paste0(text1), # for text that starts with number of gallons
        content_type %in% "paragraph" & grepl("^\\d+.gallon", text1) ~
        paste0(text1), # for text that starts with number of gallons
        content_type %in% "paragraph" & grepl("^\\d+.cm", text1) ~
        paste0(text1), # for text that starts with a number of cm
        content_type %in% "paragraph" & grepl("^\\d+%", text1) ~
        paste0(text1), # for text that starts with a percent
        content_type %in% "paragraph" & grepl("^\\d\\.\\d.mm", text1) ~
        paste0(text1), # for text that starts with a number of mm
        content_type %in% "paragraph" & grepl("^\\.[[:alpha:]]", text1) ~
        paste0(text1),
        content_type %in% "paragraph" & grepl("^\\d+\\sh", text1) ~
        paste0(text1),
        content_type %in% "paragraph" & grepl("^\\#", text1) ~
        paste0(text1),
        content_type %in% "paragraph" & grepl("^\\d.mm", text1) ~
        paste0(text1) # for text that starts with a number of mm
      )
  ) %>%
  tidyr::unite(Ctext, text2, text3, remove = FALSE) %>%
  # Combines the two section title columns into one
  dplyr::mutate(
    text0 =
      stringr::str_replace_all(Ctext, c("NA_NA" = NA, 
                                        "NA_" = "", 
                                        "_NA" = "", 
                                        "N/A" = "Not Applicable"))
    # need to remove artifacts created by unite so lookup identifies the
    # correct section titles
  ) %>%
  dplyr::mutate(.,
    section =
      dplyr::case_when(
        # focuses only on those with paragraph, allows for subsections later
        content_type %in% "paragraph" ~ qdapTools::lookup(
          text0, section_title_lookup)
      )
  ) %>%
  dplyr::mutate(
    section_number1 =
      qdapTools::lookup(section, section_number_lookup)
    # matches TNI section title with section number and returns section number
  ) %>%
  dplyr::mutate(.,
    style_name2 =
      dplyr::case_when(
        # looks for things recognized as headers that don't have heading 1
        # as style and assigns it to a new column
        !is.na(section) & is.na(style_name) ~ "heading 1",
        !is.na(section) & style_name == "List Paragraph" ~ "heading 1",
        style_name == "Style1" ~ "heading 1",
        !is.na(section) ~ "heading 1"
      )
  ) %>%
  tidyr::unite(style_comb, style_name, style_name2, remove = FALSE) %>%
  # Combines the two section title columns into one
  dplyr::mutate(
    style_name0 =
      stringr::str_replace_all(style_comb, c(
        "NA_NA" = NA, 
        "NA_" = "", 
        "_NA" = "", 
        "Style1_" = "", 
        "List Paragraph_" = "",
        "^\\w+\\[\\d\\]\\_" = "",
        "heading 1_" = "",
        "Level 1_" = ""))
    # need to remove artifacts created by unite so lookup identifies the
    # correct section titles
  ) %>%
#   #-------------------------------CHECK HEADINGS---------------------------------
# select(., sop, line_number, content_type, style_name0, text1, text0, section)
# View(sops0[sops0$content_type!="table cell" & !is.na(sops0$text0), ])
# order by style_name0 to check matching
# ALSO compare text1 (original text) to text0 (product of unite) to see that all text from
# doc was properly knitted in

dplyr::group_by(., sop) %>%
  tidyr::fill(section) %>%
  tidyr::fill(section_number1) %>%
  dplyr::ungroup(., sop) %>% # ungroup by SOP
  # dplyr::select(., -(section_title:section_title3)) %>%
  dplyr::select(., sop:content_type, text0, style_name0, section, section_number1) %>%
  dplyr::filter(., !is.na(section)) %>%
  dplyr::mutate(.,
    text =
      dplyr::case_when( # Replaces the text of the incorrect heading with TNI formatted heading
        style_name0 == "heading 1" ~ section,
        style_name0 != "heading 1" | is.na(style_name0) ~ text0 #
      )
  ) %>%
  dplyr::group_by(., sop) %>%
  dplyr::mutate(.,
    section_number =
      dplyr::case_when(
        # Identifies cases when content_type = table cell and changes the section number to "20"
        content_type == "table cell" | style_name0 == "Table Grid" ~ 20,
        content_type != "table cell" ~ section_number1,
        is.na(content_type) ~ section_number1 # leaves non-table cells as is
      )
  ) %>%
  select(., sop:content_type, style_name0, text, section, section_number) %>%
  arrange(., sop, section_number) %>%
  dplyr::rename(., style_name = style_name0)

# append missing section titles to each SOP ----
split_sops <- split(sops0, f = sops0$sop) # create a list of SOP dfs split by sop
appended_sops <- lapply(split_sops, function(x) right_join(x, TNI_titles, by = "section"))
# append complete list of section titles to account for any missing sections in each sop

# recombine the list into a single dataframe
sops1 <- plyr::ldply(appended_sops, data.frame) %>%
  select(
    sop = .id, line_number, content_type = content_type.x, style_name = style_name.x,
    text = text.x, section, section_number = section_number.y
  ) %>% # clean up columns
  # , paragraph_level, paragraph_num_id, table_row, is_table_header, table_cell) %>%
  # clean up the columns
  arrange(., sop, section_number) %>%
  dplyr::mutate(.,
    text =
      dplyr::case_when(
        is.na(line_number) & is.na(text) ~ section,
        !is.na(line_number) & !is.na(text) ~ text
      )
  ) %>%
  dplyr::mutate(.,
    style_name =
      dplyr::case_when(
        is.na(style_name) & content_type == "paragraph" ~ "text",
        is.na(line_number) & is.na(content_type) ~ "heading 1",
        !is.na(line_number) & !is.na(content_type) ~ style_name
      )
  ) %>%
  dplyr::mutate(.,
    content_type =
      dplyr::case_when(
        is.na(line_number) & is.na(content_type) ~ "paragraph",
        !is.na(line_number) & !is.na(content_type) ~ content_type
      )
  ) 


# Repalce NAs ----
na_index <- sort(which(is.na(sops1$line_number)), decreasing = TRUE)
for (i in na_index) {
  sops1 <- sops1 %>%
    tibble::add_row(.,
      line_number = NA_integer_,
      text = "Not Applicable",
      style_name = "text",
      .after = i
    ) %>%
    tidyr::fill(section) %>%
    tidyr::fill(section_number) %>%
    tidyr::fill(sop) %>%
    tidyr::fill(content_type) %>%
    tidyr::fill(line_number)
}

# # Subscript/Superscript------------
# Subscript
sops1$text <- gsub("([^ [:digit:][:punct:]])([[:digit:]]{1,100})", "\\1~\\2~", sops1$text)
# Superscript
# sops1$text <- gsub("(?<=10)([[:digit:]]{1,100})", "^\\1^", sops1$text, perl = T)


# Create Final DF-----------
sops <- sops1 %>%
  filter(., text != "") %>% # ignores empty cells
  filter(., !is.na(style_name)) %>% # ignores all table cells
  filter(., !style_name == "Table Grid") %>% # sometimes shows up as style name?
  dplyr::mutate(., # create new column therein with markd
    markd =
      dplyr::case_when(
        # identify the various style names and mark them appropriately for markdown
        style_name == "heading 1" ~ paste("#", text, sep = " "),
        !style_name == "heading 1" ~ paste0(text)
      )
  ) %>%
  select(., sop, content_type, style_name, markd, text, section)

# LOOP Function------
for (i in unique(sops$sop)) {
  sop <- sops %>%
    filter(., sop == i)
  rmarkdown::render(
    input = paste0(getwd(), "/TNIsopReport.Rmd"),
    output_file = paste0(i, "_TNI.docx", sep = ""),
    # Must be routed to a folder in the same direcory as the Rmd
    output_dir = paste0(getwd(), "/TNIformattedSOPs") 
    # Once you've formatted outputted SOPs, move to
    # Y:\EMTS\41.Sections\P&C\Quality_Systems_Management\R\SOPs\TNIformattedSOPs
  )
}
