library(pdftools)
library(tidyverse)
# https://www.pbs.gov.pk/content/final-results-census-2017
# download tehsil-level pdfs
#

final_files <- list.files(path = "final_results_raw", pattern = ".pdf")

natl_table <- tibble()

for(i in 1:length(final_files)) {
  target <- paste0("final_results_raw/", final_files[i])
  province_import <- pdf_text(target)
  province_name <- str_to_upper(str_split(final_files[i], "_")[[1]][1])
  pdf_text <- toString(province_import)
  pdf_text <- read_lines(pdf_text)
  
  # remove headers and footers from document
  table_start <- grep("TABLE -", pdf_text)
  table_end <- as.integer(table_start + 1)
  header_start <- grep("POPULATION - 2017", pdf_text)
  header_end <- grep("SIZE", pdf_text)[-1]
  header_columns <- as.integer(header_end + 1)
  headers <- list()
  for(j in 1:length(header_start)){
    distance <- header_start[j]:header_columns[j]
    headers <- c(headers, distance)
  }
  
  headers <- unlist(headers)
  
  pdf_trimmed <- pdf_text[- c(table_start, table_end, headers)]
  
  # find the breaks for each administrative area - each is a total, rural, and urban row
  
  area_start <- as.integer(grep("RURAL", pdf_trimmed) - 1)
  area_end <- as.integer(grep("RURAL", pdf_trimmed) + 1)
  
  # break down each administrative area
  table_out <- tibble()
  for(k in 1:length(area_start)){
    row_extract <- pdf_trimmed[area_start[k]:area_end[k]]
    row_total <- strsplit(trimws(row_extract[1]), "\\s{2,}")

    row_out <- tibble(
      province_name = province_name,
      area_name = row_total[[1]][1],
      area_type = "TOTAL",
      area_sq_km = row_total[[1]][2],
      total_pop = row_total[[1]][3],
      total_male = row_total[[1]][4],
      total_female = row_total[[1]][5],
      total_trans = row_total[[1]][6],
      sex_ratio = row_total[[1]][7],
      pop_density = row_total[[1]][8],
      pct_urban = row_total[[1]][9],
      avg_hh_size = row_total[[1]][10],
      total_pop_98 = row_total[[1]][11],
      avg_annual_growth = row_total[[1]][12]
    )
    
    row_rural <- strsplit(trimws(row_extract[2]), "\\s{2,}")
    rural_out <- tibble(
      province_name = province_name,
      area_name = row_total[[1]][1],
      area_type = "RURAL",
      area_sq_km = NA,
      total_pop = row_rural[[1]][2],
      total_male = row_rural[[1]][3],
      total_female = row_rural[[1]][4],
      total_trans = row_rural[[1]][5],
      sex_ratio = row_rural[[1]][6],
      pop_density = NA,
      pct_urban = NA,
      avg_hh_size = row_rural[[1]][7],
      total_pop_98 = row_rural[[1]][8],
      avg_annual_growth = row_rural[[1]][9]
    )

    row_urban <- strsplit(trimws(row_extract[3]), "\\s{2,}")
    urban_out <- tibble(
      province_name = province_name,      
      area_name = row_total[[1]][1],
      area_type = "URBAN",
      area_sq_km = NA,
      total_pop = row_urban[[1]][2],
      total_male = row_urban[[1]][3],
      total_female = row_urban[[1]][4],
      total_trans = row_urban[[1]][5],
      sex_ratio = row_urban[[1]][6],
      pop_density = NA,
      pct_urban = NA,
      avg_hh_size = row_urban[[1]][7],
      total_pop_98 = row_urban[[1]][8],
      avg_annual_growth = row_urban[[1]][9]
    )
    
    row_out <- rbind(row_out, rural_out) %>%
      rbind(urban_out)
    
    table_out <- rbind(table_out, row_out)
  }

  # combine all province tables
  natl_table <- rbind(natl_table, table_out)
}

# manual corrections to account for cell overruns causing break errors

natl_table$area_name[natl_table$area_name == "2,008"] <- "TRIBAL AREA ADJ. DERA ISMAIL KHAN"
natl_table[532,3:14] <- natl_table[529,3:14] # replace misaligned row

natl_table$area_name[natl_table$area_name == "DISTRICT"] <- "TRIBAL AREA ADJ. LAKKI MARWAT DISTRICT"
natl_table[544,3:14] <- natl_table[541,3:14] # replace misaligned row

natl_table$area_name[natl_table$area_name == "DE-EXCLUDED AREA D.G KHAN TEHSIL 5,339"] <- "DE-EXCLUDED AREA D.G KHAN TEHSIL"
natl_table$area_sq_km[natl_table$area_name == "DE-EXCLUDED AREA D.G KHAN TEHSIL" & natl_table$area_type == "TOTAL"] <- 5339
natl_table[1060,5:14] <- natl_table[1061,5:14] # replace misaligned row

natl_table[44,4:14] <- natl_table[43,4:14] # replace misaligned row

# fix a couple of cases where tables ommitted household size calculations, requiring addition of an empty cell
natl_table[685:687, 13:14] <- natl_table[685:687, 12:13]
natl_table[685:687, 12] <- NA

natl_table[706:708, 13:14] <- natl_table[706:708, 12:13]
natl_table[706:708, 12] <- NA

natl_table[733:735, 13:14] <- natl_table[733:735, 12:13]
natl_table[733:735, 12] <- NA

natl_table[799:801, 13:14] <- natl_table[799:801, 12:13]
natl_table[799:801, 12] <- NA

natl_table[832:834, 13:14] <- natl_table[832:834, 12:13]
natl_table[832:834, 12] <- NA

natl_table[862:864, 13:14] <- natl_table[862:864, 12:13]
natl_table[862:864, 12] <- NA

natl_table[895:897, 13:14] <- natl_table[895:897, 12:13]
natl_table[895:897, 12] <- NA

natl_table[979:981, 13:14] <- natl_table[979:981, 12:13]
natl_table[979:981, 12] <- NA

natl_table[979:981, 13:14] <- natl_table[979:981, 12:13]
natl_table[979:981, 12] <- NA

natl_table[785,5:14] <- natl_table[784,5:14] # replace misaligned row

natl_table[787,4:14] <- c(NA, 228350, 123541, 104801, 8, 117.88, NA, NA, 8.28, 137519, 2.70) # replace misaligned row
natl_table[788,4:14] <- c(NA, 228350, 123541, 104801, 8, 117.88, NA, NA, 8.28, 137519, 2.70) # replace misaligned row

natl_table[790,5:14] <- natl_table[790,4:13] # replace misaligned row
natl_table[790,4] <- NA
natl_table[791,5:14] <- natl_table[790,5:14] # replace misaligned row

natl_table[793,4:14] <- c(NA, 276325, 149477, 126843, 5, 117.84, NA, NA, 7.64, 166403, 2.70) # replace misaligned row
natl_table[794,4:14] <- c(NA, 276325, 149477, 126843, 5, 117.84, NA, NA, 7.64, 166403, 2.70) # replace misaligned row

natl_table[796,4:14] <- c(NA, 202935, 109826, 93109, NA, 117.95, NA, NA, 7.70, 12211, 2.70) # replace misaligned row
natl_table[797,4:14] <- c(NA, 202935, 109826, 93109, NA, 117.95, NA, NA, 7.70, 12211, 2.70) # replace misaligned row

natl_table[1567,8:14] <- c("5,954", 108.29, 339.60, 51.89, 5.55, "30,439,893", 2.41)
natl_table[1568,8:14] <- c("2,147", 107.54, NA, NA, 5.47, "15,600,031", 2.07)
natl_table[1569,8:14] <- c("3,807", 108.99, NA, NA, 5.62, "14,839,862", 2.74)

natl_table[1812,4:14] <- NA

natl_table[163,11:14] <- natl_table[164,11:14]

natl_table[1411,10:14] <- c("302.96585", "21.05229", 5.60, 475500, 1.71)
natl_table[1414,10:14] <- c("300.08879", "15.90368", 5.63, 105256, 1.58)
natl_table[1417,10:14] <- c("177.58447", "25.49191", 5.29, 122548, 1.71)
natl_table[1420,10:14] <- c("138.20879", "12.39962", 5.54, 95626, 1.45)

final_table <- natl_table

# add division and district columns where available

district_rows <- grep("DISTRICT", final_table$area_name)
district_starts <- district_rows[seq(from = 1, to = length(district_rows), by = 3)]

district_names <- final_table[district_starts, ] %>%
  rowwise %>%
  mutate(
    district_name = trimws(str_split(area_name, "DISTRICT")[[1]][1])
  )

final_table <- final_table %>% 
  left_join(district_names) %>%
  fill(district_name) %>%
  dplyr::select(province_name, district_name, area_name, everything())

# FATA is a separate case, and fill will inadvertently add district names to province rows, so remove these and correct manually
final_table$district_name[final_table$province_name == "FATA"] <- NA
final_table$district_name[final_table$area_name == "BAJAUR AGENCY"] <- "BAJAUR AGENCY"
final_table$district_name[final_table$area_name == "FR BANNU"] <- "FR BANNU"
final_table$district_name[final_table$area_name == "FR D.I.KHAN"] <- "FR D.I.KHAN"
final_table$district_name[final_table$area_name == "FR KOHAT"] <- "FR KOHAT"
final_table$district_name[final_table$area_name == "FR LAKKI MARWAT"] <- "FR LAKKI MARWAT"
final_table$district_name[final_table$area_name == "FR PESHAWAR"] <- "FR PESHAWAR"
final_table$district_name[final_table$area_name == "FR TANK"] <- "FR TANK"
final_table$district_name[final_table$area_name == "KHYBER AGENCY"] <- "KHYBER AGENCY"
final_table$district_name[final_table$area_name == "KURRAM AGENCY"] <- "KURRAM AGENCY"
final_table$district_name[final_table$area_name == "MOHMAND AGENCY"] <- "MOHMAND AGENCY"
final_table$district_name[final_table$area_name == "NORTH WAZIRISTAN AGENCY"] <- "NORTH WAZIRISTAN AGENCY"
final_table$district_name[final_table$area_name == "ORAKZAI AGENCY"] <- "ORAKZAI AGENCY"
final_table$district_name[final_table$area_name == "SOUTH WAZIRISTAN AGENCY"] <- "SOUTH WAZIRISTAN AGENCY"
final_table <- final_table %>%
  fill(district_name)

final_table$district_name[final_table$area_name == "FATA"] <- NA
final_table$district_name[final_table$area_name == "KHYBER PAKHTUNKHWA"] <- NA
final_table$district_name[final_table$area_name == "PUNJAB"] <- NA
final_table$district_name[final_table$area_name == "SINDH"] <- NA

division_rows <- grep("DIVISION", final_table$area_name)
# we don't want subdvisions, as these appear to be administratively equivalent to tehsils in most cases
sub_divisions <- grep("SUB-DIVISION", final_table$area_name)
only_division_rows <- division_rows[!(division_rows %in% sub_divisions)]

division_starts <- only_division_rows[seq(from = 1, to = length(only_division_rows), by = 3)]

division_names <- final_table[division_starts, ] %>%
  rowwise %>%
  mutate(
    division_name = trimws(str_split(area_name, "DIVISION")[[1]][1])
  )

final_table <- final_table %>% 
  left_join(division_names) %>%
  fill(division_name) %>%
  dplyr::select(province_name, division_name, district_name, area_name, everything())

# only punjab and kp have division subtotals
final_table$division_name[final_table$province_name == "BALOCHISTAN"] <- NA
final_table$division_name[final_table$province_name == "SINDH"] <- NA
final_table$division_name[final_table$province_name == "FATA"] <- NA

# division rows shouldn't have district names
final_table$district_name[only_divisions_rows] <- NA

final_table$adm_lvl_4 <- final_table$area_name
final_table$adm_lvl_4[district_rows] <- NA
final_table$adm_lvl_4[grep("TRIBAL AREA ADJ", final_table$area_name)] <- final_table$area_name[grep("TRIBAL AREA ADJ", final_table$area_name)]
final_table$adm_lvl_4[only_division_rows] <- NA
final_table$adm_lvl_4[final_table$area_name == "FATA"] <- NA
final_table$adm_lvl_4[final_table$province_name == "FATA" & final_table$district_name == final_table$area_name] <- NA
final_table$adm_lvl_4[final_table$area_name == "BALOCHISTAN"] <- NA
final_table$adm_lvl_4[final_table$area_name == "KHYBER PAKHTUNKHWA"] <- NA
final_table$adm_lvl_4[final_table$area_name == "PUNJAB"] <- NA
final_table$adm_lvl_4[final_table$area_name == "SINDH"] <- NA

final_table <- final_table %>% 
  dplyr::select(province_name, division_name, district_name, adm_lvl_4, area_name, everything()) %>%
  rename(
    adm_lvl_1 = province_name,
    adm_lvl_2 = division_name,
    adm_lvl_3 = district_name
  )

# final fixes
final_table$adm_lvl_2[final_table$area_name == "PUNJAB"] <- NA

final_table$adm_lvl_3[985:993] <- "MALAKAND PROTECTED AREA"
final_table$adm_lvl_4[985:987] <- NA

# clean up numerics

final_table$area_sq_km <- as.numeric(gsub(",", "", final_table$area_sq_km))
final_table$total_pop <- as.numeric(gsub(",", "", final_table$total_pop))
final_table$total_male <- as.numeric(gsub(",", "", final_table$total_male))
final_table$total_female <- as.numeric(gsub(",", "", final_table$total_female))
final_table$total_trans <- as.numeric(gsub(",", "", final_table$total_trans))
final_table$sex_ratio <- as.numeric(final_table$sex_ratio)
final_table$pop_density <- as.numeric(final_table$pop_density)
final_table$pct_urban <- as.numeric(final_table$pct_urban)/100
final_table$avg_hh_size <- as.numeric(final_table$avg_hh_size)
final_table$total_pop_98 <- as.numeric(gsub(",", "", final_table$total_pop_98))
final_table$avg_annual_growth <- as.numeric(final_table$avg_annual_growth)/100

# add Islamabad data from national table, not included in other provincial tables
# https://www.pbs.gov.pk/sites/default/files//population_census/National.pdf

islamabad <- tibble(
  adm_lvl_1 = c("ISLAMABAD", "ISLAMABAD", "ISLAMABAD"),
  adm_lvl_2 = c(NA, NA, NA),
  adm_lvl_3 = c(NA, NA, NA),
  adm_lvl_4 = c("ISLAMABAD", "ISLAMABAD", "ISLAMABAD"),
  area_name = c("ISLAMABAD", "ISLAMABAD", "ISLAMABAD"),
  area_type = c("TOTAL", "RURAL", "URBAN"),
  area_sq_km = c(906, NA, NA),
  total_pop = c(2003368, 994365, 1009003),
  total_male = c(1052328, 516723, 535605),
  total_female = c(950760, 477518, 473242),
  total_trans = c(280, 124, 156),
  sex_ratio = c(110.68, 108.21, 113.18),
  pop_density = c(2211.22, NA, NA),
  pct_urban = c(50.37, NA, NA),
  avg_hh_size = c(5.86, 5.94, 5.78),
  total_pop_98 = c(805235, 276055, 529180),
  avg_annual_growth = c(4.90, 6.96, 3.45)
)

final_table <- rbind(final_table, islamabad)
# math check

# math_check <- final_table %>% 
#  filter(area_type == "TOTAL" & !is.na(adm_lvl_4)) %>%
#  group_by(adm_lvl_3) %>%
#  summarize(pop_sum = sum(total_pop, na.rm = T)) %>%
#  left_join(final_table %>% filter(area_type == "TOTAL" & is.na(adm_lvl_4)) %>% dplyr::select(adm_lvl_3, total_pop)) %>%
#  mutate(difference = pop_sum - total_pop,
#         error = ifelse(difference != 0, "YES", "NO"))

write_csv(final_table, "Pakistan_2017_Census_Final.csv")

# ----------------------------------------------------------------
# check for changes between preliminary and final results

prelim_census <- read_csv("Pakistan_2017_Census_Blocks.csv")
final_census <- read_csv("Pakistan_2017_Census_Blocks_Final.csv")

prelim_census_aggregated <- 
  prelim_census %>% group_by(province, district, sublvl_01) %>%
  summarize(prelim_total = sum(population, na.rm = T))

prelim_list <- prelim_census_aggregated$sublvl_01

final_census_aggregated <- 
  final_census %>% filter(area_type == "TOTAL" & !is.na(adm_lvl_4)) %>%
  group_by(adm_lvl_1, adm_lvl_2, adm_lvl_3, adm_lvl_4) %>%
  summarize(final_total = sum(total_pop, na.rm = T))

final_list <- final_census_aggregated$adm_lvl_4

final_not_in_prelim <- final_list[!(final_list %in% prelim_list)]
prelim_not_in_final <- prelim_list[!(prelim_list %in% final_list)]

# standardize names to match between prelim and final

final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "HURRAM ZAI SUB-TEHSIL"] <- "HARAM ZAI SUB-TEHSIL"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "SHAHGORI SUB-TEHSIL"] <- "SHAHOO GARHI SUB-TEHSIL"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "TRIBAL AREA ADJ. DERA ISMAIL KHAN"] <- "TRIBAL AREA ADJ. DERA ISMAIL KHAN DISTRICT"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "ISLAMABAD"] <- "ISLAMABAD TEHSIL"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "GULBERG SUB-DIVISION"] <- "GULBERG"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "LIAQUATABAD SUB-DIVISION"] <- "LIAQUATABAD"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "NAZIMABAD SUB-DIVISION"] <- "NAZIMABAD"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "NEW KARACHI SUB-DIVISION"] <- "NEW KARACHI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "NORTH NAZIMABAD SUB-DIVISION"] <- "NORTH NAZIMABAD"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "FEROZABAD SUB-DIVISION"] <- "FEROZABAD"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "GULSHAN-E-IQBAL SUB-DIVISION"] <- "GULSHAN-E-IQBAL"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "GULZAR-E-HIJRI SUB-DIVISION"] <- "GULZAR-E-HIJRI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "JAMSHED QUARTERS SUB-DIVISION"] <- "JAMSHED QUARTERS"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "ARAM BAGH SUB-DIVISION"] <- "ARAM BAGH"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "CIVIL LINES SUB-DIVISION"] <- "CIVIL LINES"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "GARDEN SUB-DIVISION"] <- "GARDEN"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "LYARI SUB-DIVISION"] <- "LYARI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "SADDAR SUB-DIVISION"] <- "SADDAR"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "BALDIA SUB-DIVISION"] <- "BALDIA"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "HARBOUR SUB-DIVISION"] <- "HARBOUR"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "MANGHOPIR SUB-DIVISION"] <- "MANGHOPIR"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "MAURIPUR SUB-DIVISION"] <- "MAURIPUR"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "MOMINABAD SUB-DIVISION"] <- "MOMINABAD"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "ORANGI SUB-DIVISION"] <- "ORANGI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "SITE SUB-DIVISION"] <- "SITE"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "KASHMORE TALUKA"] <- "KASHMOR TALUKA"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "KORANGI SUB-DIVISION"] <- "KORANGI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "LANDHI SUB-DIVISION"] <- "LANDHI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "MODEL COLONY SUB-DIVISION"] <- "MODEL COLONY"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "SHAH FAISAL SUB-DIVISION"] <- "SHAH FAISAL"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "AIRPORT SUB-DIVISION"] <- "AIRPORT"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "IBRAHIM HYDRI SUB-DIVISION"] <- "IBRAHIM HYDRI"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "MURAD MEMON SUB-DIVISION"] <- "MURAD MEMON"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_4 == "SHAH MUREED SUB-DIVISION"] <- "SHAH MUREED"

# note that final results combines Loralai and Bori tehsils, which are separate in preliminary

# there are also two Tamboo tehsils (one in Kohlu district, one in Nasirabad district), 
# two Sahiwal tehsils (one in Sahiwal district, one in Sargodha district), 
# two Nowsheras (one in Khushab district Punjab, one in Nowshera district KPK)

final_census_aggregated$adm_lvl_1[final_census_aggregated$adm_lvl_1 == "KP"] <- "KHYBER PAKHTUNKHWA"
final_census_aggregated$adm_lvl_1[final_census_aggregated$adm_lvl_1 == "ISLAMABAD"] <- "FEDERAL CAPITAL"
final_census_aggregated$adm_lvl_3[final_census_aggregated$adm_lvl_1 == "ISLAMABAD"] <- "ISLAMABAD DISTRICT"
final_census_aggregated$adm_lvl_4[final_census_aggregated$adm_lvl_1 == "ISLAMABAD"] <- "ISLAMABAD DISTRICT"

adm_lvl_4_check <- prelim_census_aggregated %>%
  filter(!(sublvl_01 %in% c("BORI TEHSIL", "LORALAI TEHSIL"))) %>%
  full_join(
    tibble(province = "BALOCHISTAN",
           district = "LORALAI DISTRICT",
           sublvl_01 = "LORALAI\\BORI TEHSIL",
           prelim_total = 203562
           )
  ) %>%
  rename(adm_lvl_1 = province,
         adm_lvl_3 = district,
         adm_lvl_4 = sublvl_01) %>%
  left_join(final_census_aggregated[-3]) %>%
  mutate(difference = final_total - prelim_total) %>%
  dplyr::select(
    adm_lvl_1, adm_lvl_2, adm_lvl_3, adm_lvl_4,
    prelim_total, final_total, difference
  ) %>%
  filter(!(adm_lvl_4 %in% c("TAMBOO TEHSIL", "SAHIWAL TEHSIL"))) %>%
  full_join(
    tibble(
      adm_lvl_1 = c("PUNJAB", "PUNJAB", "BALOCHISTAN", "BALOCHISTAN"),
      adm_lvl_3 = c("SAHIWAL", "SARGODHA", "NASIRABAD", "KOHLU"),
      adm_lvl_4 = c("SAHIWAL TEHSIL", "SAHIWAL TEHSIL", "TAMBOO TEHSIL", "TAMBOO TEHSIL"),
      prelim_total = c(1491553, 341247, 131168, 36420),
      final_total = c(1488831, 340695, 131056, 36405),
      difference = c(-2722, -552, -112, -15)
    )
  ) %>%
  arrange(
    adm_lvl_1, adm_lvl_2, adm_lvl_3, adm_lvl_4
  )

write_csv(adm_lvl_4_check, "provisional_to_final_adm_lvl_4_comparison.csv")

# Karachi cantonments don't have matches in final data
# Unclear how population in these areas has been distributed to other sub-divisions within the city (or not)
# This may produce over- or under-stated differences between provisional and final results in other areas of Karachi