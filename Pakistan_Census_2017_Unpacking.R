# AUTHOR: Colin Cookman
# CONTACT: ccookman at gmail dot com
# DATE: May 23 2018
#
library(XML)
library(pdftools)
library(readr)
library(tidyverse)
library(stringr)
#
# SCRAPING -------------------------------------------------------------------- 
# extract links to detailed blockwise data from the PBS website
# data available as of May 15 2018; alternatively see https://web.archive.org/web/20180515014953/http://www.pbscensus.gov.pk/
#
url <- "http://www.pbscensus.gov.pk/"
links <- getHTMLLinks(url)
links_data <- links[str_detect(links, "BLOCKWISE.pdf")]
links_data <- gsub(" ", "%20", links_data)
#
for(i in seq_along(links_data)) {
  url <- paste0("http://www.pbscensus.gov.pk", links_data[i])
#
# optional - also download all files to current working directory  
# download.file(url, destfile = basename(url))
# Sys.sleep(1)
#  
if(i == 1) {
    file_list <- url
      } else {
    file_list <- rbind(file_list, url)
  }
}
# EXTRACT TEXT FROM PDF -------------------------------------------------------
# file_list <- list.files() 
# use this instead of previous section if files are already downloaded to a working directory (much faster once downloaded)

for(i in 1:length(file_list)) {
  
# import files on list and convert to text lines
target <- file_list[i]
district_import <- pdf_text(target)
cat("Loading", target, "...")
pdf_text <- toString(district_import)
pdf_text <- read_lines(pdf_text)

# locate headers and footers in text
header_row_1 <- grep("POPULATION AND HOUSEHOLD DETAIL FROM BLOCK TO DISTRICT LEVEL", pdf_text) 
header_row_2 <- as.integer(header_row_1 + 1)
header_row_3 <- grep("ADMIN UNIT", pdf_text)
footer_row <- grep("Page ", pdf_text)
#
# pull out province for loaded district
province <- pdf_text[header_row_2]
province <- province[1]
province <- regmatches(province, regexpr("^[^\\(]+", province))[[1]]
province <- trimws(province)
#
# remove headers and footers from document
pdf_text <- pdf_text[- c(header_row_1, header_row_2, header_row_3, footer_row)]
#
# split strings that have more than two units of whitespace, trim the rest
data <- Reduce(rbind, strsplit(trimws(pdf_text), "\\s{2,}"))
# add a rownumber to first row
rownames(data) <- 1:dim(data)[1]
#
# create key lists of census block rows and administrative area subtotals
census_blocks <- grep("\\d{9}", data)
census_blocks_key <- data[census_blocks]
#
# convert key to df
block_df <- as.data.frame(census_blocks_key)
block_df <- block_df %>% mutate(type = "census_block")
block_df <- block_df %>% dplyr::rename(unit = census_blocks_key)
#
# convert data to dataframe
data_df <- as.data.frame(data)
data_df <- data_df %>%
  mutate(id = row_number())
data_df <- data_df %>%
  dplyr::rename(
    unit = V1,
    population = V2,
    households = V3
  )
#
# convert to numeric, stripping commas
data_df$population <- as.numeric(gsub(",", "", data_df$population))
data_df$households <- as.numeric(gsub(",", "", data_df$households))
#
# recombine
data_df <- left_join(data_df, block_df, by = "unit")
data_df$type <- ifelse(is.na(data_df$type), "subtotal_area", "census_block")
#
# for census blocks where data was missing, remove duplicates created by merges and replace with NA value
data_df$population <- ifelse(data_df$unit == data_df$population | data_df$unit == paste0(0, data_df$population), NA, data_df$population)
data_df$households <- ifelse(data_df$unit == data_df$households| data_df$unit == paste0(0, data_df$households), NA, data_df$households)
data_df <- unique( data_df[ , 1:5 ] )
#
# this part figures out subdivision parent-child relationships by relative positions
subdivisions <- filter(data_df, type == "subtotal_area")
#
subdivisions <- subdivisions %>%
    mutate(child_04 = ifelse(id - (lead(id, n = 1)-1) != 0, "child", "parent"))
subdivisions$child_04[is.na(subdivisions$child_04)] <- "child"
#
temp <- subdivisions %>%
  filter(child_04 == "parent") %>%
  mutate(child_03 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
temp$child_03[is.na(temp$child_03)] <- "child"
subdivisions <- left_join(subdivisions, temp)
#
temp <- subdivisions %>%
  filter(child_04 == "parent" & child_03 == "parent") %>%
  mutate(child_02 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
temp$child_02[is.na(temp$child_02)] <- "child"
subdivisions <- left_join(subdivisions, temp)
#
temp <- subdivisions %>%
  filter(child_04 == "parent" & child_03 == "parent" & child_02 == "parent") %>%
  mutate(child_01 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
temp$child_01[is.na(temp$child_01)] <- "child"
subdivisions <- left_join(subdivisions, temp)
#
data_df$child_05 <- ifelse(data_df$type == "census_block", "child", "parent")
#
data_df <- left_join(data_df, subdivisions)
#
data_df$child_01 <- ifelse(data_df$child_01 == "parent", data_df$unit, NA)
data_df$child_02 <- ifelse(data_df$child_02 == "parent" & is.na(data_df$child_01), data_df$unit, NA)
data_df$child_03 <- ifelse(data_df$child_03 == "parent" & is.na(data_df$child_01) & is.na(data_df$child_02), data_df$unit, NA)
data_df$child_04 <- ifelse(data_df$child_04 == "parent" & is.na(data_df$child_01) & is.na(data_df$child_02) & is.na(data_df$child_03), data_df$unit, NA)
data_df$child_05 <- ifelse(data_df$child_05 == "parent" & is.na(data_df$child_01) & is.na(data_df$child_02) & is.na(data_df$child_03) & is.na(data_df$child_04), data_df$unit, NA)
#
# fill forward missing names
data_df <- data_df %>% fill(c(child_01, child_02, child_03, child_04, child_05))
data_df$province <- province
#
# first pass cleanup
data_df <- data_df %>%
  rename(
    district = child_01,
    sublvl_01 = child_02,
    sublvl_02 = child_03,
    sublvl_03 = child_04,
    sublvl_04 = child_05
  ) %>%
  dplyr::select(province, district, sublvl_01, sublvl_02, sublvl_03, sublvl_04, unit, population, households, type)
#
# remove area subtotals and leave only tidy census block rows
temp <- data_df %>% filter(type == "census_block")
temp <- temp %>%
  rename(
    census_block = unit
  )
#
# add PBS data reported date
temp <- temp %>% 
  mutate(report_date = "January 03 2018")
#         
temp <- temp %>%
  dplyr::select(report_date, province, district, sublvl_01, sublvl_02, sublvl_03, sublvl_04, census_block, population, households)
#
# extract district name for log reporting purposes
district_name <- data[1]
#
# combine district results with other districts
if(i == 1) {
  out <- temp
  log <- district_name
  } else {
    out <- rbind(out, temp)
    log <- rbind(log, district_name)
    cat("Processed", district_name, "...")
    }
#
# ending file list runthrough
} 
#
# CHECKS ----------------------------------------------------------------------
# log
# length(unique(out$district))
# length(file_list)
# breaks <- out %>% filter(is.na(district))
# errors <- out %>% filter(unit == population)
# repeat_blocks <- out %>% filter(duplicated(unit))
#
# FINAL ORGANIZATION --------------------------------------------------------
#
census_data <- out
census_data <- census_data %>% 
  mutate(avg_hh_size = population / households)
#
# output as csv if desired
# write.csv(census_data, "Pakistan_2017_Census_Blocks.csv")
#
# DATA SUMMARY, RE-AGGREGATION, AND OTHER OBSERVATIONS ------------------------
#
tehsil_data <- census_data %>% group_by(report_date, province, district, sublvl_01) %>%
  summarize(
    total_blocks = n(),
    missing_blocks = sum(is.na(population)),
    population = sum(population, na.rm = TRUE),
    households = sum(households, na.rm = TRUE),
    avg_hh_size = population / households
  )

