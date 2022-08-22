setwd("~/Google Drive/GitHub/pakistan_census")
library(pdftools)
library(XML)
library(rvest)
library(tidyverse)
rm(list = ls())

#
# Download district tables ----------------------------------
#
url <- read_html("https://www.pbs.gov.pk/content/district-wise-results-tables-census-2017")
district_links <- url %>% html_nodes("a") %>% html_attr("href")
district_links <- district_links[str_detect(district_links, "node")]
district_links <- district_links[!is.na(district_links)]

download_error_log <- list()
# Cycle through district pages to download tables for each district
for(n in 1:length(district_links)){
  district_link <- paste0("https://www.pbs.gov.pk", district_links[n])
  district_number <- str_split(district_link, "=")[[1]][2]
  district_page <- read_html(district_link)
  district_name <- district_page %>% html_nodes("h1") %>% html_text()
  district_name <- trimws(district_name[2])
  
  district_path <- paste0("./district_tables_raw/", district_name, "/")
  
  # create the appropriate subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(district_path)), dir.create(file.path(district_path)), FALSE)

  # load the district page and get links to all tables
  table_links <- district_page %>% html_nodes("a") %>% html_attr("href")
  table_links <- table_links[str_detect(table_links, ".pdf")]
  table_links <- table_links[!is.na(table_links)]
  
  # download the tables into district folders
  for(i in 1:length(table_links)){
    tryCatch(
      download.file(paste0("https://www.pbs.gov.pk", table_links[i]), file.path(district_path, destfile = basename(table_links[i]))),
      error = function(e){download_error_log <- c(download_error_log, table_links[i])} # this isn't logging properly
    )
    Sys.sleep(1.5)
  }
}

# Process the pdfs ---------------------------------------------
rm(list = ls())

table_numbers <- str_pad(c(1:40), side = "left", width = 2, pad = "0")
district_subdirs <- list.files(path = "./district_tables_raw/")
missing_files <- tibble(
  district_number = NA,
  district_name = NA,
  missing_table = NA
)

# province groupings based on district codes taken from PBS pdfs

kpk_codes <- tibble(
  province_name = "KHYBER PAKTUNKHWA",
  district_code = str_pad(1:25, width = 3, side = "left", pad = "0")
)

fata_codes <- tibble(
  province_name = "FATA",
  district_code = str_pad(26:38, width = 3, side = "left", pad = "0")
)

punjab_codes <- tibble(
  province_name = "PUNJAB",
  district_code = str_pad(39:74, width = 3, side = "left", pad = "0")
)

sindh_codes <- tibble(
  province_name = "SINDH",
  district_code = str_pad(75:103, width = 3, side = "left", pad = "0")
)

balochistan_codes <- tibble(
  province_name = "BALOCHISTAN",
  district_code = str_pad(104:134, width = 3, side = "left", pad = "0")
)

islamabad_codes <- tibble(
  province_name = "ISLAMABAD CAPITAL TERRITORY",
  district_code = str_pad(135, width = 3, side = "left", pad = "0")
)

all_codes <- full_join(kpk_codes, fata_codes) %>% full_join(punjab_codes) %>%
  full_join(sindh_codes) %>% full_join(balochistan_codes) %>% full_join(islamabad_codes)

natl_table_01 <- tibble()
natl_table_02 <- tibble()
natl_table_03 <- tibble()
natl_table_04 <- tibble()
natl_table_05 <- tibble()
natl_table_06 <- tibble()
natl_table_07 <- tibble()
natl_table_08 <- tibble()
natl_table_09 <- tibble()
natl_table_10 <- tibble()
natl_table_11 <- tibble()
natl_table_12 <- tibble()
natl_table_13 <- tibble()
natl_table_14 <- tibble()
natl_table_15 <- tibble()
natl_table_16 <- tibble()
natl_table_17 <- tibble()
natl_table_18 <- tibble()
natl_table_19 <- tibble()
natl_table_20 <- tibble()
natl_table_21 <- tibble()
natl_table_22_literacy <- tibble()
natl_table_22_marital <- tibble()
natl_table_22_religion <- tibble()
natl_table_22_work <- tibble()
natl_table_23 <- tibble()
natl_table_24 <- tibble()
natl_table_25 <- tibble()
natl_table_26 <- tibble()
natl_table_27 <- tibble()
natl_table_28 <- tibble()
natl_table_29 <- tibble()
natl_table_30 <- tibble()
natl_table_31 <- tibble()
natl_table_32 <- tibble()
natl_table_33 <- tibble()
natl_table_34 <- tibble()
natl_table_35 <- tibble()
natl_table_36 <- tibble()
natl_table_37 <- tibble()
natl_table_38 <- tibble()
natl_table_39 <- tibble()
natl_table_40 <- tibble()

# identify administrative summary areas from the provisional results data

Pakistan_2017_Census_Provisional <- read_csv("~/Google Drive/GitHub/pakistan_census/Pakistan_2017_Census_Provisional.csv")

census_blocks <- unique(Pakistan_2017_Census_Provisional$census_block)
sublvl_01_list <- unique(Pakistan_2017_Census_Provisional$sublvl_01)
sublvl_02_list <- unique(Pakistan_2017_Census_Provisional$sublvl_02)
sublvl_03_list <- unique(Pakistan_2017_Census_Provisional$sublvl_03)
provisional_areas <- Pakistan_2017_Census_Provisional %>% dplyr::select(district, sublvl_01, sublvl_02, sublvl_03, sublvl_04) %>% unique()
administrative_areas <- unique(c(unique(provisional_areas$district), 
                          unique(provisional_areas$sublvl_01), unique(provisional_areas$sublvl_02), unique(provisional_areas$sublvl_03)))

# following areas missing from provisional results list
administrative_areas <- c(administrative_areas, 
                          "CHAUDHAWAN QH", "MANGHOPIR SUB-DIVISION", "DINO MAKO TC", "KALEKEY QH",
                          "SHADI PALI STC", "CHAK NO 004/R.D. PC", "IBRAHIM HYDRI SUB-DIVISION", "ORNACH SUB-TEHSIL", "SHAH MUREED SUB-DIVISION",
                          "SHAHGORI SUB-TEHSIL", "KASHMOR TC", "KHANO WALA PC", "ODERO LAL STC", "MURAD MEMON SUB-DIVISION",
                          "MULTAN CANTONMENT", "MAURIPUR SUB-DIVISION", "LORALAI\\BORI TEHSIL", "HURRAM ZAI SUB-TEHSIL", "SHORKOT CANTONMENT",
                          "ALIPUR MC", "AWARAN MC", "BARKHAN MC", "BARRI KOT MC", "BATKHELA MC",	"BEHRAIN MC",	"BELA MC",	"BHAG MC",	"BHAWANA MC",	"BULAIDA MC",	"CHAK JHUMRA MC",
                          "CHAUBARA MC",	"CHITKAN MC", "CHITRAL MC",	"DALBANDIN MC",	"DAULTALA MC", "DERA BUGTI MC",	"DERA DIN PANAH MC", "DERA ISMAIL KHAN MC",
                          "DERA MURAD JAMALI MC",	"DHADAR MC",	"DIJKOT MC	DINGA MC",	"DUKI MC	DUREJI MC", "FATEH PUR MC",	"FEROZEWALA MC",	"GADDANI MC",	"GANDAWA MC",
                          "GHUR GHUSHTI MC",	"GUJAR MASHKAI MC",	"GWADAR MC",	"HANGU MC",	"HARIPUR MC",	"HARNAI MC",	"HUB MC",	"HURRAM ZAI MC",
                          "JAND MC",	"JIWANI MC",	"KABAL MC",	"KALAT MC",	"KALLAR SYEDAN MC",	"KAMBAR MC",	"KARAK MC",	"KAROR LAL ESAN MC",
                          "KHALABAT MC",	"KHANGARH MC",	"KHANOZAI MC",	"KHANQAH DOGRAN MC",	"KHARAN MC", "KHARIAN MC",	"KHAWAZA KHELA MC", "KHURIANWALA MC",
                          "KILLA ABDULLAH MC",	"KILLA SAIFULLAH MC",	"KOHAT MC",	"KOHLU MC",	"KOT ABDUL MALIK MC",	"KULACHI MC",	"KUNJAH MC",	
                          "LAKKI MARWAT MC",	"LORALAI MC",	"MACH MC",	"MALAKWAL MC",	"MAMU KANJAN MC",	"MANDI BAHAUDDIN MC",	"MANSEHRA MC",	"MASTUNG MC",
                          "MATTA MC",	"MINGORA MC",	"MUSAKHEL MC",	"MUZAFFARGARH MC", "NAL MC",	"NARANG MANDI MC",	"NUSHKI MC",	"ORMARA MC",	
                          "PAHARPUR MC",	"PASNI MC",	"PROA MC",	"QASIMABAD MC",	"SAFDARABAD MC",	"SAMMUNDRI MC",	"SARAI ALAMGIR MC",	"SARANAN MC",	"SHAHRIG MC",	
                          "SHARQPUR MC",	"SIBI MC",	"SOHBATPUR MC",	"SUI MC",	"SURAB MC",	"TAKHT BHAI MC", "TALL MC",	"TANK MC",	"TASP MC",	"TAUNSA MC",
                          "THUL MC",	"TIMARGARA MC",	"TUMP MC",	"UTHAL MC",	"WADH MC",	"WASHUK MC",	"WINDER MC",	"ZEHRI MC",	"ZIARAT MC",
                          "ATTOCK CANTONMENT", "KAMRA CANTONMENT", "SANJWAL CANTONMENT", "DERA GHAZI KHAN MUNICIPAL CORPORATION", "GUJRAT MUNICIPAL CORPORATION", "DINGA MC",
                          "KHARIAN CANTONMENT", "HYDERABAD CANTONMENT (PART OF HYDERABAD CITY TALUKA)", "HYDERABAD MUNICIPAL CORPORATION (PART OF HYDERABAD	CITY TALUKA)", 
                          "HYDERABAD CANTONMENT (PART OF LATIFABAD TALUKA)", "HYDERABAD MUNICIPAL CORPORATION (PART OF LATIFABAD TALUKA)",
                          "HYDERABAD CANTONMENT (PART OF QASIMABAD TALUKA)", "ISLAMABAD METROPOLITAN CORPORATION", "GARHI KHAIRO TC", "MIRPUR BURRIRO TC",
                          "GULBERG SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF GULBERG SUB-DIVISION)", "LIAQUATABAD SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI	CENTRAL (PART OF LIAQUATABAD SUB-DIVISION)", "NAZIMABAD SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI	CENTRAL (PART OF NAZIMABAD SUB-DIVISION)", "NEW KARACHI SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI	CENTRAL (PART OF NEW KARACHI SUB-DIVISION)", "NORTH NAZIMABAD SUB-DIVISION", 
                          "DISTRICT MUNICIPAL CORPORATION KARACHI	CENTRAL (PART OF NORTH NAZIMABAD SUB-DIVISION)", "FEROZABAD SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION	KARACHI EAST (PART OF FEROZABAD SUB-DIVISION)", "GULSHAN-E-IQBAL SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI	EAST (PART OF GULSHAN-E-IQBAL	SUB-DIVISION)",
                          "FAISAL CANTONMENT (PART OF GULSHAN-E-IQBAL	SUB-DIVISION)", "GULZAR-E-HIJRI SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF GULZAR-E-HIJRI SUB-DIVISION)",
                          "MALIR CANTONMENT (PART OF GULZAR-E-HIJRI SUB-DIVISION)", "JAMSHED QUARTERS SUB-DIVISION", 
                          "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF JAMSHED QUARTERS SUB-DIVISION)",
                          "KARACHI CANTONMENT (PART OF JAMSHED QUARTERS SUB-DIVISION)",
                          "ARAM BAGH SUB-DIVISION", "DISTRICT MUNICIPAL	CORPORATION KARACHI	SOUTH (PART OF ARAM BAGH SUB-DIVISION)", "CIVIL LINES SUB-DIVISION",
                          "CLIFTON CANTONMENT (PART	OF CIVIL LINES SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF CIVIL LINES SUB-DIVISION)",
                          "KARACHI CANTONMENT (PART	OF CIVIL LINES SUB-DIVISION)", "GARDEN SUB-DIVISION", "DISTRICT MUNICIPAL	CORPORATION KARACHI	SOUTH (PART OF GARDEN SUB-DIVISION)",
                          "LYARI SUB-DIVISION", "DISTRICT MUNICIPAL	CORPORATION KARACHI	SOUTH (PART OF LYARI SUB-DIVISION)", "SADDAR SUB-DIVISION", "CLIFTON CANTONMENT (PART	OF SADDAR SUB-DIVISION)",
                          "DISTRICT MUNICIPAL	CORPORATION KARACHI	SOUTH (PART OF SADDAR SUB-DIVISION)", "KARACHI CANTONMENT (PART	OF SADAR SUB-DIVISION)", "BALDIA SUB-DIVISION",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF BALDIA SUB-DIVISION)", "HARBOUR SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF HARBOUR SUB-DIVISION)",
                          "MANORA CANTONMENT (PART OF HARBOUR	SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MANGHOPIR)", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MAURIPUR SUB-DIVISION)",
                          "MOMINABAD SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MOMINABAD SUB-DIVISION)", "ORANGI SUB-DIVISION", 
                          "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF ORANGI SUB-DIVISION)", "SITE SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION DISTRICT WEST (PART OF S.I.T.E. SUB-DIVISION)",
                          "KHUZDAR MUNICIPAL CORPORATION", "JAMRUD TC", "LANDI KOTAL TC", "CHAMAN MUNICIPAL CORPORATION", "KOHAT CANTONMENT", "LACHI TC", "SHAKARDARA TC", "KORANGI SUB-DIVISION",
                          "CLIFTON CANTONMENT (PART OF KORANGI SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF KORANGI SUB-DIVISION)", "KORANGI CREEK", "LANDHI SUB-DIVISION",
                          "DISTRICT MUNICIPAL	CORPORATION KORANGI (PART	LANDHI SUB-DIVISION)", "MODEL COLONY SUB-DIVISION", "DISTRICT MUNICIPAL	CORPORATION KORANGI (PART	OF MODEL COLONY SUB-DIVISION)",
                          "SHAH FAISAL SUB-DIVISION", "DISTRICT MUNICIPAL	CORPORATION KORANGI (PART	OF SHAH FAISAL SUB-DIVISION)", "FAISAL CANTONMENT (PART OF SHAH FAISAL SUB-DIVISION)",
                          "SADDA TC", "PARACHINAR TC", "LAHORE CANTONMENT", "LAHORE METROPOLITAN CORPORATION (PART OF LAHORE CANTONMENT TEHSIL)", "WALTON CANTONMENT",
                          "LAHORE METROPOLITAN CORPORATION (PART OF LAHORE CITY TEHSIL)", "LAHORE METROPOLITAN CORPORATION (PART OF MODEL TOWN TEHSIL)", "LAHORE METROPOLITAN CORPORATION	(PART OF RAIWIND TEHSIL)",
                          "LAHORE METROPOLITAN CORPORATION (PART OF SHALIMAR TEHSIL)", "DUKI MC", "SWAT RANI ZAI SUB-DIVISION", "AIRPORT SUB-DIVISION", "DISTRICT MUNICIPAL CORPORATION	MALIR (PART OF AIRPORT SUB-DIVISION)",
                          "FAISAL CANTONMENT (PART OF AIRPORT	SUB-DIVISION)", "MALIR CANTONMENT (PART OF AIRPORT SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF IBRAHIM HYDRI SUB-DIVISION)",
                          "KORANGI CREEK CANTONMENT (PART OF IBRAHIM HYDRI SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF MURAD MEMON SUB-DIVISION)",
                          "MALIR CANTONMENT (PART OF MURAD MEMON SUB-DIVISION)", "BAFFA TC", "MARDAN CANTONMENT", "TAKHT BHAI MC", "MIRAN SHAH TC", "PESHAWAR CANTONMENT", "PESHAWAR MUNICIPAL CORPORATION",
                          "SUKKUR MUNICIPAL	CORPORATION (PART OF NEW SUKKUR TALUKA)", "PANO AQIL CANTONMENT", "SUKKUR MUNICIPAL	CORPORATION (PART OF SUKKUR CITY TALUKA)", "DIR TC",
                          "SAZEEN U.C", "SIGLO U.C", "KUZPARO U.C", "MADA KHEL U.C", "CHAWADARA U.C", "BAN KHAD U.C", "JANDWALA PC",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF FEROZABAD SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF GULSHAN-E-IQBAL SUB-DIVISION)",
                          "FAISAL CANTONMENT (PART OF GULSHAN-E-IQBAL SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF JAMSHED QUARTERS)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF ARAM BAGH SUB-DIVISION)", "CLIFTON CANTONMENT (PART OF CIVIL LINES SUB-DIVISION)",
                          "KARACHI CANTONMENT (PART OF CIVIL LINES SUB-DIVISION)", "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF GARDEN SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF LYARI SUB-DIVISION)", "CLIFTON CANTONMENT (PART OF SADDAR SUB-DIVISION)", 
                          "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF SADDAR SUB-DIVISION)", "KARACHI CANTONMENT (PART OF SADAR SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF AIRPORT SUB-DIVISION)", "FAISAL CANTONMENT (PART OF AIRPORT SUB-DIVISION)",
                          "LUDHEWALA WARAICH MC", "QILA DIDAR SINGH MC", "GUJRANWALA CANTONMENT", "GUJRANWALA MUNICIPAL CORPORATION", "NOWSHERA VIRKAN MC",
                          "LARKANA MUNICIPAL CORPORATION", "AMANGARH TC", "CHERAT CANTONMENT", "NOWSHERA CANTONMENT", "RISALPUR CANTONMENT",
                          "LILLIANI MC", "SARGODHA CANTONMENT", "SARGODHA MUNICIPAL CORPORATION", "SUKKUR MUNICIPAL CORPORATION (PART OF NEW SUKKUR TALUKA)",
                          "SUKKUR MUNICIPAL CORPORATION (PART OF SUKKUR CITY TALUKA)", "COL. SHER KILLI/NAWAN KILLI TC",
                          "ABBOTTABAD MC", "AGRA TC", "AHMADPUR SIAL MC", "BAHAWALPUR CANTONMENT", "BAHAWALPUR MUNICIPAL CORPORATION", "BANNU MC",
                          "CHAKLALA CANTONMENT", "CHAWINDA TC", "CHOA SAIDAN SHAH MC", "CHUNIAN MC", "DARYA KHAN MARRI TC", "DHONKAL MC", "DIJKOT MC",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF LIAQUATABAD SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NAZIMABAD SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NEW KARACHI SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NORTH NAZIMABAD SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MANGHOPIR SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF S.I.T.E SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF LANDHI SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF MODEL COLONY SUB-DIVISION)",
                          "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF SHAH FAISAL SUB-DIVISION)", "DOABA TC", "DONGA BONGA MC", "DUREJI MC",
                          "FAISALABAD MUNICIPAL CORPORATION", "FAZALPUR MC", "FORT ABBAS MC", "GARH MAHARAJA MC", "HAVELIAN MC",
                          "HYDERABAD MUNICIPAL CORPORATION (PART OF HYDERABAD CITY TALUKA)", "JALALPUR BHATTIAN MC", "JHELUM CANTONMENT", "JHELUM MC",
                          "KALEKEY TC", "KAMALIA MC", "KANGANPUR MC", "KHADIAN MC", "KHAIR PUR NATHAN SHAH MC", "KHEWRA MC", "KINGRI (PIRJO GOTH) MC",
                          "KOT MITHAN MC", "KOT RADHA KISHAN MC", "KOT SAMABA MC", "LAHORE METROPOLITAN CORPORATION (PART OF RAIWIND TEHSIL)", "LIAQATABAD MC",
                          "LIAQUATPUR MC", "MANGLA CANTONMENT", "MANORA CANTONMENT (PART OF HARBOUR SUB-DIVISION)", "MITHI MC", "MULTAN MUNICIPAL CORPORATION",
                          "MURREE HILLS CANTONMENT", "MURREE MUNICIPAL CORPORATION", "MUSLIM BAGH MC", "MUSTAFABAD MC", "NASIRPUR TC", "NAURANG TC",
                          "NAWAN SHEHR TC", "NEW SAEEDABAD TC", "NOORPUR MC", "ODERO LAL STATION TC", "PANYALA TC", "PHOOL NAGAR MC", "PIND DADAN KHAN MC",
                          "PINDI BHATTIAN MC", "PIR MAHAL MC", "PISHIN MUNICIPAL CORPORATION", "QUETTA METROPOLITAN CORPORATION", "RAJA JANG MC", "RAJANPUR MC",
                          "RAWALPINDI CANTONMENT", "RAWALPINDI MUNICIPAL CORPORATION", "ROJHAN MC", "SAHIWAL MUNICIPAL CORPORATION", "SHAHKOT MC",
                          "SHORKOT MC", "SIALKOT CANTONMENT", "SIALKOT MUNICIPAL CORPORATION", "SOBHO DERO TC", "SOHAWA MC", "SUKHEKE MC", "TANDO BAGO MC",
                          "TAXILA CANTONMENT", "THARI MOHABAT TC", "THERHI-I TC", "TRINDA SAWAI KHAN MC", "TULAMBA MC", "TURBAT MUNICIPAL CORPORATION",
                          "UCH SHARIF MC", "USTA MUHAMMAD MC", "WAH CANTONMENT", "WARBURTON MC", "ZAHIRPIR MC", "ZHOB MC",
                          "KARACHI CANTONMENT (PART OF SADDAR SUB-DIVISION)", "KORANGI CREEK CANTONMENT (PART OF KORANGI SUB-DIVISION)",
                          "NILI PC"
                          )

# iterate through all district subfolders to extract data from available pdfs
for(d in 1:length(district_subdirs)){
  # get the list of files in the district subdirectory
  table_files <- list.files(path = paste0("./district_tables_raw/", district_subdirs[d], "/"))
  table_count <- str_sub(table_files, 4, 5)
  missing_tables <- table_numbers[!(table_numbers %in% table_count)]
  district_name <- district_subdirs[d]
  district_number <- str_sub(table_files[1], 1, 3)
  
  # Process Table 1 ---
  
  # make sure the pdf is present before proceeding, if not add it to the missing tables log
  if("01" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "01.pdf"))
  } else {

    # load the pdf file
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "01.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_text, perl = T)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 2){
      pdf_text[18] <-  "RURAL          1,395,470          688,077     707,315             78      97.28                               6.08   1,003,843   1.75"
      pdf_text[19] <- "URBAN                     490,908         250,573     240,282             53     104.28                               6.13   271,092     3.17"
    }
    if(d == 26){
      pdf_text[13] <- "TRIBAL AREA ADJ. LAKKI MARWAT DISTRICT        132     26,394      13,685     12,709                 -     107.68      199.95         -        7.84     6,987       7.23"
      pdf_text[14:15] <- ""
      pdf_text <- pdf_text[pdf_text != ""]
    }
    if(d == 65){
      pdf_text[15] <- "RURAL        784711     424643      360055              13       117.94          -       -        7.84    472570             2.70"  
      pdf_text[19] <- "DASSU SUB-DIVISION           -              223436     121177      102259               -        118.50                -          -            8.28   137519             2.58"  
      pdf_text[20] <- "RURAL             223436     121177      102259               -        118.50                -          -            8.28   137519             2.58"  
      pdf_text[24] <- "KANDIA SUB-DIVISION          -               83850      45597       38245               8       119.22                -          -            7.74    47227             3.06"  
      pdf_text[29] <-  "PALAS SUB-DIVISION          -               274923     149104      125814               5       118.51                -          -            7.64   165613             2.70"
      pdf_text[34] <- "PATTAN SUB-DIVISION          -              202502     108765       93737               0       116.03                -          -            7.70   122211             2.69"  
    }
    if(d == 99){
      pdf_text <- pdf_text[1:27]
    }
    if(d == 118){
      pdf_text[15] <- ""
      pdf_text[16] <- "SOUTH WAZIRISTAN AGENCY              6,620   675,215   355,611   319,554     50    111.28    102.00        -      7.98   429,841                2.4"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area - each is a total, rural, and urban row
    area_start <- as.integer(grep("RURAL", pdf_trimmed) - 1)
    area_end <- as.integer(grep("RURAL", pdf_trimmed) + 1)
    
    # break down each administrative area
    table_01 <- tibble()
    for(k in 1:length(area_start)){
      row_extract <- pdf_trimmed[area_start[k]:area_end[k]]
      row_total <- strsplit(trimws(row_extract[1]), "\\s{2,}")
      if(d == 37 & k %in% c(4,5)){
        row_total <- list(c(row_total[[1]][1:9], "-", row_total[[1]][10:11]))
      }
      if(d == 55 & k %in% c(2)){
        row_total <- list(c(row_total[[1]][1:8], "-", row_total[[1]][9:11]))
      }

      row_out <- tibble(
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
      # try to find the positions of the sex ratio, hh size and growth rate to ensure columns land correctly
      # Lahore correction for change to rural area designations
      if(d == 69){
        sr_pos <- NA
        hh_pos <- NA
        gr_pos <- NA
      } else {
      sr_pos <- ifelse(any(grepl("\\.", row_rural[[1]])),
                       grep("\\.", row_rural[[1]])[[1]],
                       NA)
      hh_pos <- ifelse(any(grepl("\\.", row_rural[[1]])),
                       grep("\\.", row_rural[[1]])[[2]],
                       NA)
      gr_pos <- ifelse(any(grepl("\\.", row_rural[[1]])),
                       grep("\\.", row_rural[[1]])[[3]],
                       NA)
      }
      rural_out <- tibble(
        area_name = row_total[[1]][1],
        area_type = "RURAL",
        area_sq_km = NA,
        total_pop = row_rural[[1]][2],
        total_male = row_rural[[1]][3],
        total_female = row_rural[[1]][4],
        total_trans = row_rural[[1]][5],
        sex_ratio = ifelse(!is.na(sr_pos), row_rural[[1]][sr_pos], NA),
        pop_density = NA,
        pct_urban = NA,
        avg_hh_size = ifelse(!is.na(hh_pos), row_rural[[1]][hh_pos], NA),
        total_pop_98 = ifelse(d == 69,
                              row_rural[[1]][8],
                              ifelse(!is.na(gr_pos), row_rural[[1]][gr_pos - 1], NA)),
        avg_annual_growth = ifelse(d == 69,
                              row_rural[[1]][9],
                              ifelse(!is.na(gr_pos), row_rural[[1]][gr_pos], NA))
      )
  
      row_urban <- strsplit(trimws(row_extract[3]), "\\s{2,}")
      row_urban <- gsub("0\\.00", "-", row_urban[[1]])
      sr_pos <- ifelse(any(grepl("\\.", row_urban)),
                       grep("\\.", row_urban)[[1]],
                       NA)
      hh_pos <- ifelse(any(grepl("\\.", row_urban)),
                       grep("\\.", row_urban)[[2]],
                       NA)
      gr_pos <- ifelse(any(grepl("\\.", row_urban)),
                       ifelse(length(grep("\\.", row_urban)) == 3,
                       grep("\\.", row_urban)[[3]], NA),
                       NA)
      urban_out <- tibble(
        area_name = row_total[[1]][1],
        area_type = "URBAN",
        area_sq_km = NA,
        total_pop = row_urban[2],
        total_male = row_urban[3],
        total_female = row_urban[4],
        total_trans = row_urban[5],
        sex_ratio = ifelse(!is.na(sr_pos), row_urban[sr_pos], NA),
        pop_density = NA,
        pct_urban = NA,
        avg_hh_size = ifelse(!is.na(hh_pos), row_urban[hh_pos], NA),
        total_pop_98 = ifelse(!is.na(gr_pos), row_urban[gr_pos - 1], NA),
        avg_annual_growth = ifelse(!is.na(gr_pos), row_urban[gr_pos], NA)
      )
      
      row_out <- rbind(row_out, rural_out) %>%
        rbind(urban_out)
      
      table_01 <- rbind(table_01, row_out)
    }
    
    # cleanup numerics
    table_01 <- cbind(table_01 %>% dplyr::select(1:2),
          table_01 %>% dplyr::select(3:13) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    table_01 <- table_01 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())

    prov_path <- paste0("./processed_forms/", unique(table_01$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_01, paste0(dist_path, "table_01.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_01 <- rbind(natl_table_01, table_01)
    
  } # end else

  # Process Table 2, Urban Localities ---
  if("02" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "02.pdf"))
    } else {

    # load the pdf file
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "02.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_text, perl = T)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("TURBAT MUNICIPAL CORPORATION KECH", "TURBAT MUNICIPAL CORPORATION     KECH", pdf_text)
    pdf_text <- gsub("100,000  - 199,999", "100,000 - 199,999", pdf_text)
    
    if(d == 18){
      pdf_text[10] <- "DADU DISTRICT"
    }
    if(d == 21){
      pdf_text[19] <- "DERA ISMAIL KHAN CANTONMENT    DERA ISMAIL KHAN TEHSIL           5,694          3,519          2,175              -           5,145            0.53          5.20"
      pdf_text <- pdf_text[1:19]
    }
    if(d == 22){
      pdf_text[16] <- ""
      pdf_text[pdf_text != ""]
    }
    if(d == 38){
      pdf_text[13] <- "ISLAMABAD METROPOLITAN CORPORATION    ISLAMABAD TEHSIL   1,009,003   535,605    473,242            156       529,180            3.45           5.78"
      pdf_text <- pdf_text [1:13]
    }
    if(d == 48){
      pdf_text[9] <- "KARACHI CENTRAL DISTRICT"
    }
    if(d == 49){
      pdf_text[8] <- "KARACHI EAST DISTRICT"
    }
    if(d == 50){
      pdf_text[8] <- "KARACHI SOUTH DISTRICT"
    }
    if(d == 52){
      pdf_text[8] <- "KARAK DISTRICT"
    }
    if(d == 59){
      pdf_text[11] <- "KHUSHAB DISTRICT"
      pdf_text <- gsub("RATE", "", pdf_text)
    }
    if(d == 69){
      pdf_text[10] <- "LAHORE DISTRICT"
    }
    if(d == 71){
      pdf_text[9] <- "LARKANA DISTRICT"
    }
    if(d == 86){
      pdf_text[9] <- ""
    }
    if(d == 120){
      pdf_text <- gsub("SUKKUR MUNICIPAL CORPORATION ", "SUKKUR MUNICIPAL CORPORATION    ", pdf_text)
    }
    if(d == 133){
      pdf_text[11] <- "WASHUK MC        WASHUK SUB-TEHSIL     21,835              11,407        10,428           -         -            -                5.45"
    }


    if(d %in% c(28, 65)){
      table_02 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          area_name = NA, locality_name = NA, locality_bucket = NA, pop_98 = NA, avg_annual_growth_98_17 = NA, avg_hh_size = NA, gender = NA, total_population = NA
        )
      
      natl_table_02 <- rbind(natl_table_02, table_02)
    } else {
    
    if(!(any(grepl(district_name, pdf_text)))){
      
      table_02 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          area_name = NA, locality_name = NA, locality_bucket = NA, pop_98 = NA, avg_annual_growth_98_17 = NA, avg_hh_size = NA, gender = NA, total_population = NA
        )
      
      natl_table_02 <- rbind(natl_table_02, table_02)
    } else {
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    locality_sizes <- c("500,000 AND ABOVE", "200,000 - 499,999", "100,000 - 199,999", "100,000 - 199,999",
                    "50,000 - 99,999", "25,000 - 49,999", "10,000 - 24,999", "5,000 - 9,999", "BELOW 5,000")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    locality_buckets <- unlist(str_split(trimws(pdf_trimmed), "\\s{2,}"))[unlist(str_split(trimws(pdf_trimmed), "\\s{2,}")) %in% locality_sizes]
    
    table_02 <- tibble()

    for(k in 1:length(locality_buckets)){
        total_out <- tibble()
        bucket_start <- as.integer(grep(locality_buckets[k], pdf_trimmed)) + 1
        bucket_end <- ifelse(k != length(locality_buckets), (as.integer(grep(locality_buckets[k+1], pdf_trimmed)-1)), length(pdf_trimmed))
        bucket_extract <- pdf_trimmed[bucket_start:bucket_end]
        bucket_split <- strsplit(trimws(bucket_extract), "\\s{2,}")
        bucket_split <- bucket_split[lengths(bucket_split) > 0L]

        for(c in 1:length(bucket_split)){
            for(g in 1:length(gender_types)){
                row_out <- tibble(
                district_name = district_name,
                area_name = bucket_split[[c]][2],
                locality_name = bucket_split[[c]][1],
                locality_bucket = locality_buckets[k],
                pop_98 = ifelse(length(bucket_split[[c]]) == 7, NA, bucket_split[[c]][7]), # if 98 pop data is ommitted
                avg_annual_growth_98_17 = ifelse(length(bucket_split[[c]]) == 7, NA, bucket_split[[c]][8]),
                avg_hh_size = ifelse(length(bucket_split[[c]]) == 7, bucket_split[[c]][7], bucket_split[[c]][9]),
                gender = gender_types[g],
                total_population = bucket_split[[c]][g+2]
                )
                
            total_out <- rbind(total_out, row_out)
            }
        }
          
        table_02 <- rbind(table_02, total_out)  
      }

    # cleanup numerics

    table_02 <- cbind(
          table_02 %>% dplyr::select(1:4),
          table_02 %>% dplyr::select(pop_98, avg_annual_growth_98_17, avg_hh_size) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_02 %>% dplyr::select(gender),
          table_02 %>% dplyr::select(c(total_population)) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_02$total_population[is.na(table_02$total_population)] <- 0
    
    table_02 <- table_02 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    if(d %in% c(48, 49, 50, 51, 67, 78)){
      table_02$locality_name <- gsub("DISTRICT MUNICIPAL CORPORATION", paste0("DISTRICT MUNICIPAL CORPORATION ", district_name), table_02$locality_name)
    }
    
    table_02$locality_name <- gsub("LAHORE METROPOLITAN CORPORATIO", "LAHORE METROPOLITAN CORPORATION", table_02$locality_name)
    
    prov_path <- paste0("./processed_forms/", unique(table_02$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_02, paste0(dist_path, "table_02.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_02 <- rbind(natl_table_02, table_02)
    }
    } # end else
  } # end else
  
  # Process Table 3, Rural Localities ---
  if("03" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "03.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "03.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    
    if(d == 65){
      pdf_text[9] <- "KOHISTAN DISTRICT                          1495         784711        424643         360055           13"
      pdf_text[18] <- "DASSU SUB-DIVISION                           318          223436         121177     102259              -"
      pdf_text[27] <- "KANDIA SUB-DIVISION                           194        83850             45597      38245              8"
      pdf_text[36] <- "PALAS SUB-DIVISION                          549       274923            149104     125814              5"
      pdf_text[45] <- "PATTAN SUB-DIVISION                           434       202502            108765      93737              0"
    }
    if(d == 113){
      pdf_text[8] <- "SHERANI DISTRICT"
      pdf_text[9] <- "5,000 AND ABOVE                                  181               152,952                84,390                 68,561                 1"
      pdf_text[15] <- "SHERANI SUB-DIVISION"
      pdf_text[16] <- "5,000 AND ABOVE                              181               152,952                84,390                 68,561                 1"
      pdf_text <- c(pdf_text[1:14], "UN-INHABITED                                        4                     -                     -                      -                 -", pdf_text[15:22])
    }
    
    # for cases where there are no rural localities
    if(!(any(grepl(district_name, pdf_text)))){
      
      table_03 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          area_name = NA, locality_size = NA, locality_count = NA, gender = NA, total_population = NA
        )
      
      natl_table_03 <- rbind(natl_table_03, table_03)
    } else {
      
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("5000 AND ABOVE", "5,000 AND ABOVE", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area
    area_start <- as.integer(grep("5,000 AND ABOVE", pdf_trimmed) - 1)
    area_end <- as.integer(grep("UN-INHABITED", pdf_trimmed))
    
    locality_sizes <- c("TOTAL", "5,000 AND ABOVE", "2,000 - 4,999", "1,000 - 1,999", "500 - 999", "200 - 499", "LESS THAN 200", "UN-INHABITED")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    
    table_03 <- tibble()
    
    for(k in 1:length(area_start)){
      area_extract <- pdf_trimmed[area_start[k]:area_end[k]]
      area_split <- strsplit(trimws(area_extract), "\\s{2,}")
      total_out <- tibble()
      
      for(c in 1:length(locality_sizes)){
        for(g in 1:length(gender_types)){
                row_out <- tibble(
                area_name = area_split[[1]][1],
                locality_size = ifelse(c == 1, "TOTAL", area_split[[c]][1]),
                locality_count = area_split[[c]][2],
                gender = gender_types[g],
                total_population = area_split[[c]][g+2]
                )
                
            total_out <- rbind(total_out, row_out)
            }
      }
      table_03 <- rbind(table_03, total_out)  
    }

    # cleanup numerics

    table_03 <- cbind(table_03 %>% dplyr::select(1:4),
          table_03 %>% dplyr::select(total_population) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_03$total_population[is.na(table_03$total_population)] <- 0
    
    table_03 <- table_03 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_03$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_03, paste0(dist_path, "table_03.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_03 <- rbind(natl_table_03, table_03)
    }
  } # end else
    
  # Process Table 4, Age Distribution ---
  if("04" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "04.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "04.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    
    if(d == 20){
      pdf_text[102] <- "All Ages            212,652      -         103,213      10   212,652   109,429   103,213      10         -          -           -           -"
      pdf_text[109] <- "05 - 09              39,295    20,776      18,519       -    39,295    20,776    18,519        -        -          -            -         -"
    }
    if(d == 25){
      pdf_text <- c(pdf_text[1:4], "FR KOHAT", pdf_text[5:192])
    }
    if(d == 65){
      pdf_text <- gsub("ALL", "ALL AGES", pdf_text)
    }
    if(d == 88){
      pdf_text[5] <- "MUSAKHEL DISTRICT"
    }
    if(d == 96){
      pdf_text[6] <- "NUSHKI DISTRICT"
      pdf_text[100] <- "All Ages                  178,947               -               -               3       -           -           -            3           46,396         24,281            22,115                 -"
    }
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]    

    more_headers_starts <- grep("POPULATION BY SINGLE YEAR", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("TOTAL\\s*RURAL\\s*URBAN", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}12\\s{6,}13", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*11\\s*12\\s*13", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
        
    # find the breaks for each administrative area
    area_start <- as.integer(grep("ALL AGES", pdf_trimmed) - 1)
    area_end <- as.integer(grep("75 & ABOVE", pdf_trimmed))
    
    table_04 <- tibble()
    # break down each administrative area
    for(k in 1:length(area_start)){
      row_extract <- pdf_trimmed[area_start[k]:area_end[k]]
      row_split <- strsplit(trimws(row_extract), "\\s{2,}")
  
      total_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "TOTAL",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][2:5],
        group_00_04 = row_split[[3]][2:5],
        age_below_1 = row_split[[4]][2:5],
        age_01 = row_split[[5]][2:5],
        age_02 = row_split[[6]][2:5],
        age_03 = row_split[[7]][2:5],
        age_04 = row_split[[8]][2:5],
        group_05_09 = row_split[[9]][2:5],
        age_05 = row_split[[10]][2:5],
        age_06 = row_split[[11]][2:5],
        age_07 = row_split[[12]][2:5],
        age_08 = row_split[[13]][2:5],
        age_09 = row_split[[14]][2:5],
        group_10_14 = row_split[[15]][2:5],
        age_10 = row_split[[16]][2:5],
        age_11 = row_split[[17]][2:5],
        age_12 = row_split[[18]][2:5],
        age_13 = row_split[[19]][2:5],
        age_14 = row_split[[20]][2:5],
        group_15_19 = row_split[[21]][2:5],
        age_15 = row_split[[22]][2:5],
        age_16 = row_split[[23]][2:5],
        age_17 = row_split[[24]][2:5],
        age_18 = row_split[[25]][2:5],
        age_19 = row_split[[26]][2:5],
        group_20_24 = row_split[[27]][2:5],
        age_20 = row_split[[28]][2:5],
        age_21 = row_split[[29]][2:5],
        age_22 = row_split[[30]][2:5],
        age_23 = row_split[[31]][2:5],
        age_24 = row_split[[32]][2:5],
        group_25_29 = row_split[[33]][2:5],
        age_25 = row_split[[34]][2:5],
        age_26 = row_split[[35]][2:5],
        age_27 = row_split[[36]][2:5],
        age_28 = row_split[[37]][2:5],
        age_29 = row_split[[38]][2:5],
        group_30_34 = row_split[[39]][2:5],
        age_30 = row_split[[40]][2:5],
        age_31 = row_split[[41]][2:5],
        age_32 = row_split[[42]][2:5],
        age_33 = row_split[[43]][2:5],
        age_34 = row_split[[44]][2:5],
        group_35_39 = row_split[[45]][2:5],
        age_35 = row_split[[46]][2:5],
        age_36 = row_split[[47]][2:5],
        age_37 = row_split[[48]][2:5],
        age_38 = row_split[[49]][2:5],
        age_39 = row_split[[50]][2:5],
        group_40_44 = row_split[[51]][2:5],
        age_40 = row_split[[52]][2:5],
        age_41 = row_split[[53]][2:5],
        age_42 = row_split[[54]][2:5],
        age_43 = row_split[[55]][2:5],
        age_44 = row_split[[56]][2:5],
        group_45_49 = row_split[[57]][2:5],
        age_45 = row_split[[58]][2:5],
        age_46 = row_split[[59]][2:5],
        age_47 = row_split[[60]][2:5],
        age_48 = row_split[[61]][2:5],
        age_49 = row_split[[62]][2:5],
        group_50_54 = row_split[[63]][2:5],
        age_50 = row_split[[64]][2:5],
        age_51 = row_split[[65]][2:5],
        age_52 = row_split[[66]][2:5],
        age_53 = row_split[[67]][2:5],
        age_54 = row_split[[68]][2:5],
        group_55_59 = row_split[[69]][2:5],
        age_55 = row_split[[70]][2:5],
        age_56 = row_split[[71]][2:5],
        age_57 = row_split[[72]][2:5],
        age_58 = row_split[[73]][2:5],
        age_59 = row_split[[74]][2:5],
        group_60_64 = row_split[[75]][2:5],
        age_60 = row_split[[76]][2:5],
        age_61 = row_split[[77]][2:5],
        age_62 = row_split[[78]][2:5],
        age_63 = row_split[[79]][2:5],
        age_64 = row_split[[80]][2:5],
        group_65_69 = row_split[[81]][2:5],
        age_65 = row_split[[82]][2:5],
        age_66 = row_split[[83]][2:5],
        age_67 = row_split[[84]][2:5],
        age_68 = row_split[[85]][2:5],
        age_69 = row_split[[86]][2:5],
        group_70_74 = row_split[[87]][2:5],
        age_70 = row_split[[88]][2:5],
        age_71 = row_split[[89]][2:5],
        age_72 = row_split[[90]][2:5],
        age_73 = row_split[[91]][2:5],
        age_74 = row_split[[92]][2:5],
        group_75_and_up = row_split[[93]][2:5]
      )
      
      rural_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "RURAL",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][6:9],
        group_00_04 = row_split[[3]][6:9],
        age_below_1 = row_split[[4]][6:9],
        age_01 = row_split[[5]][6:9],
        age_02 = row_split[[6]][6:9],
        age_03 = row_split[[7]][6:9],
        age_04 = row_split[[8]][6:9],
        group_05_09 = row_split[[9]][6:9],
        age_05 = row_split[[10]][6:9],
        age_06 = row_split[[11]][6:9],
        age_07 = row_split[[12]][6:9],
        age_08 = row_split[[13]][6:9],
        age_09 = row_split[[14]][6:9],
        group_10_14 = row_split[[15]][6:9],
        age_10 = row_split[[16]][6:9],
        age_11 = row_split[[17]][6:9],
        age_12 = row_split[[18]][6:9],
        age_13 = row_split[[19]][6:9],
        age_14 = row_split[[20]][6:9],
        group_15_19 = row_split[[21]][6:9],
        age_15 = row_split[[22]][6:9],
        age_16 = row_split[[23]][6:9],
        age_17 = row_split[[24]][6:9],
        age_18 = row_split[[25]][6:9],
        age_19 = row_split[[26]][6:9],
        group_20_24 = row_split[[27]][6:9],
        age_20 = row_split[[28]][6:9],
        age_21 = row_split[[29]][6:9],
        age_22 = row_split[[30]][6:9],
        age_23 = row_split[[31]][6:9],
        age_24 = row_split[[32]][6:9],
        group_25_29 = row_split[[33]][6:9],
        age_25 = row_split[[34]][6:9],
        age_26 = row_split[[35]][6:9],
        age_27 = row_split[[36]][6:9],
        age_28 = row_split[[37]][6:9],
        age_29 = row_split[[38]][6:9],
        group_30_34 = row_split[[39]][6:9],
        age_30 = row_split[[40]][6:9],
        age_31 = row_split[[41]][6:9],
        age_32 = row_split[[42]][6:9],
        age_33 = row_split[[43]][6:9],
        age_34 = row_split[[44]][6:9],
        group_35_39 = row_split[[45]][6:9],
        age_35 = row_split[[46]][6:9],
        age_36 = row_split[[47]][6:9],
        age_37 = row_split[[48]][6:9],
        age_38 = row_split[[49]][6:9],
        age_39 = row_split[[50]][6:9],
        group_40_44 = row_split[[51]][6:9],
        age_40 = row_split[[52]][6:9],
        age_41 = row_split[[53]][6:9],
        age_42 = row_split[[54]][6:9],
        age_43 = row_split[[55]][6:9],
        age_44 = row_split[[56]][6:9],
        group_45_49 = row_split[[57]][6:9],
        age_45 = row_split[[58]][6:9],
        age_46 = row_split[[59]][6:9],
        age_47 = row_split[[60]][6:9],
        age_48 = row_split[[61]][6:9],
        age_49 = row_split[[62]][6:9],
        group_50_54 = row_split[[63]][6:9],
        age_50 = row_split[[64]][6:9],
        age_51 = row_split[[65]][6:9],
        age_52 = row_split[[66]][6:9],
        age_53 = row_split[[67]][6:9],
        age_54 = row_split[[68]][6:9],
        group_55_59 = row_split[[69]][6:9],
        age_55 = row_split[[70]][6:9],
        age_56 = row_split[[71]][6:9],
        age_57 = row_split[[72]][6:9],
        age_58 = row_split[[73]][6:9],
        age_59 = row_split[[74]][6:9],
        group_60_64 = row_split[[75]][6:9],
        age_60 = row_split[[76]][6:9],
        age_61 = row_split[[77]][6:9],
        age_62 = row_split[[78]][6:9],
        age_63 = row_split[[79]][6:9],
        age_64 = row_split[[80]][6:9],
        group_65_69 = row_split[[81]][6:9],
        age_65 = row_split[[82]][6:9],
        age_66 = row_split[[83]][6:9],
        age_67 = row_split[[84]][6:9],
        age_68 = row_split[[85]][6:9],
        age_69 = row_split[[86]][6:9],
        group_70_74 = row_split[[87]][6:9],
        age_70 = row_split[[88]][6:9],
        age_71 = row_split[[89]][6:9],
        age_72 = row_split[[90]][6:9],
        age_73 = row_split[[91]][6:9],
        age_74 = row_split[[92]][6:9],
        group_75_and_up = row_split[[93]][6:9]
      )
  
      urban_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "URBAN",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][10:13],
        group_00_04 = row_split[[3]][10:13],
        age_below_1 = row_split[[4]][10:13],
        age_01 = row_split[[5]][10:13],
        age_02 = row_split[[6]][10:13],
        age_03 = row_split[[7]][10:13],
        age_04 = row_split[[8]][10:13],
        group_05_09 = row_split[[9]][10:13],
        age_05 = row_split[[10]][10:13],
        age_06 = row_split[[11]][10:13],
        age_07 = row_split[[12]][10:13],
        age_08 = row_split[[13]][10:13],
        age_09 = row_split[[14]][10:13],
        group_10_14 = row_split[[15]][10:13],
        age_10 = row_split[[16]][10:13],
        age_11 = row_split[[17]][10:13],
        age_12 = row_split[[18]][10:13],
        age_13 = row_split[[19]][10:13],
        age_14 = row_split[[20]][10:13],
        group_15_19 = row_split[[21]][10:13],
        age_15 = row_split[[22]][10:13],
        age_16 = row_split[[23]][10:13],
        age_17 = row_split[[24]][10:13],
        age_18 = row_split[[25]][10:13],
        age_19 = row_split[[26]][10:13],
        group_20_24 = row_split[[27]][10:13],
        age_20 = row_split[[28]][10:13],
        age_21 = row_split[[29]][10:13],
        age_22 = row_split[[30]][10:13],
        age_23 = row_split[[31]][10:13],
        age_24 = row_split[[32]][10:13],
        group_25_29 = row_split[[33]][10:13],
        age_25 = row_split[[34]][10:13],
        age_26 = row_split[[35]][10:13],
        age_27 = row_split[[36]][10:13],
        age_28 = row_split[[37]][10:13],
        age_29 = row_split[[38]][10:13],
        group_30_34 = row_split[[39]][10:13],
        age_30 = row_split[[40]][10:13],
        age_31 = row_split[[41]][10:13],
        age_32 = row_split[[42]][10:13],
        age_33 = row_split[[43]][10:13],
        age_34 = row_split[[44]][10:13],
        group_35_39 = row_split[[45]][10:13],
        age_35 = row_split[[46]][10:13],
        age_36 = row_split[[47]][10:13],
        age_37 = row_split[[48]][10:13],
        age_38 = row_split[[49]][10:13],
        age_39 = row_split[[50]][10:13],
        group_40_44 = row_split[[51]][10:13],
        age_40 = row_split[[52]][10:13],
        age_41 = row_split[[53]][10:13],
        age_42 = row_split[[54]][10:13],
        age_43 = row_split[[55]][10:13],
        age_44 = row_split[[56]][10:13],
        group_45_49 = row_split[[57]][10:13],
        age_45 = row_split[[58]][10:13],
        age_46 = row_split[[59]][10:13],
        age_47 = row_split[[60]][10:13],
        age_48 = row_split[[61]][10:13],
        age_49 = row_split[[62]][10:13],
        group_50_54 = row_split[[63]][10:13],
        age_50 = row_split[[64]][10:13],
        age_51 = row_split[[65]][10:13],
        age_52 = row_split[[66]][10:13],
        age_53 = row_split[[67]][10:13],
        age_54 = row_split[[68]][10:13],
        group_55_59 = row_split[[69]][10:13],
        age_55 = row_split[[70]][10:13],
        age_56 = row_split[[71]][10:13],
        age_57 = row_split[[72]][10:13],
        age_58 = row_split[[73]][10:13],
        age_59 = row_split[[74]][10:13],
        group_60_64 = row_split[[75]][10:13],
        age_60 = row_split[[76]][10:13],
        age_61 = row_split[[77]][10:13],
        age_62 = row_split[[78]][10:13],
        age_63 = row_split[[79]][10:13],
        age_64 = row_split[[80]][10:13],
        group_65_69 = row_split[[81]][10:13],
        age_65 = row_split[[82]][10:13],
        age_66 = row_split[[83]][10:13],
        age_67 = row_split[[84]][10:13],
        age_68 = row_split[[85]][10:13],
        age_69 = row_split[[86]][10:13],
        group_70_74 = row_split[[87]][10:13],
        age_70 = row_split[[88]][10:13],
        age_71 = row_split[[89]][10:13],
        age_72 = row_split[[90]][10:13],
        age_73 = row_split[[91]][10:13],
        age_74 = row_split[[92]][10:13],
        group_75_and_up = row_split[[93]][10:13]
      )      

      row_out <- rbind(total_out, rural_out) %>%
        rbind(urban_out)
      
      table_04 <- rbind(table_04, row_out)
    }

    # cleanup numerics
    table_04 <- cbind(table_04 %>% dplyr::select(1:3),
          table_04 %>% dplyr::select(4:95) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_04[is.na(table_04)] <- 0
    
    table_04 <- table_04 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    table_04$gender[table_04$gender == "ALL"] <- "ALL SEXES"
    
    prov_path <- paste0("./processed_forms/", unique(table_04$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_04, paste0(dist_path, "table_04.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_04 <- rbind(natl_table_04, table_04)
    
  } # end else 
  
  # Process Table 5 ---
  if("05" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "05.pdf"))
    } else {

    # load the pdf file
    # RAHIM YAR KHAN, RAJANPUR, AND RAWALPINDI ARE ALL MIXED UP
    if(!d %in% c(104, 105, 106)){
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "05.pdf")      
    } else {
      if(d == 104){
        target <- "./district_tables_raw/RAWALPINDI DISTRICT/04205.pdf"
      }
      if(d == 105){
        target <- "./district_tables_raw/RAHIM YAR KHAN DISTRICT/06605.pdf"
      }
      if(d == 106){
        target <- "./district_tables_raw/RAJANPUR DISTRICT/06805.pdf"
      }
    }

    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- pdf_text[pdf_text != ""]
    if(d == 65){
      pdf_text <- gsub("ALL", "ALL AGES", pdf_text)
    }
    if(d == 88){
      pdf_text[8] <- "MUSAKHEL DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area
    area_start <- as.integer(grep("ALL AGES", pdf_trimmed) - 1)
    area_end <- as.integer(grep("65 & ABOVE", pdf_trimmed))

    table_05 <- tibble()
    # break down each administrative area
    for(k in 1:length(area_start)){
      row_extract <- pdf_trimmed[area_start[k]:area_end[k]]
      row_split <- strsplit(trimws(row_extract), "\\s{2,}")
    # KASUR, NANKANA SAHIB, SAHIWAL, SARGODHA, SIALKOT, TT SINGH, and VEHARI DISTRICTS OMIT ROW FOR AGE 15-64
      if(d %in% c(54, 90, 107, 109, 112, 115, 128, 132)){
        row_split <- c(row_split[1:10], list(rep(NA, 13)), row_split[11:14])
      }
  
      total_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "TOTAL",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][2:5],
        group_under_01 = row_split[[3]][2:5],
        group_under_05 = row_split[[4]][2:5],
        group_under_10 = row_split[[5]][2:5],
        group_under_16 = row_split[[6]][2:5],
        group_05_24 = row_split[[7]][2:5],
        group_15_24 = row_split[[8]][2:5],
        group_15_29 = row_split[[9]][2:5],
        group_15_49 = row_split[[10]][2:5],
        group_15_64 = row_split[[11]][2:5],
        group_18_60 = row_split[[12]][2:5],
        group_18_and_up = row_split[[13]][2:5],
        group_60_and_up = row_split[[14]][2:5],
        group_65_and_up = row_split[[15]][2:5]
      )

      rural_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "RURAL",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][6:9],
        group_under_01 = row_split[[3]][6:9],
        group_under_05 = row_split[[4]][6:9],
        group_under_10 = row_split[[5]][6:9],
        group_under_16 = row_split[[6]][6:9],
        group_05_24 = row_split[[7]][6:9],
        group_15_24 = row_split[[8]][6:9],
        group_15_29 = row_split[[9]][6:9],
        group_15_49 = row_split[[10]][6:9],
        group_15_64 = row_split[[11]][6:9],
        group_18_60 = row_split[[12]][6:9],
        group_18_and_up = row_split[[13]][6:9],
        group_60_and_up = row_split[[14]][6:9],
        group_65_and_up = row_split[[15]][6:9]
      )      

      urban_out <- tibble(
        area_name = row_split[[1]][1],
        area_type = "URBAN",
        gender = c("ALL", "MALE", "FEMALE", "TRANSGENDER"),
        group_all_ages = row_split[[2]][10:13],
        group_under_01 = row_split[[3]][10:13],
        group_under_05 = row_split[[4]][10:13],
        group_under_10 = row_split[[5]][10:13],
        group_under_16 = row_split[[6]][10:13],
        group_05_24 = row_split[[7]][10:13],
        group_15_24 = row_split[[8]][10:13],
        group_15_29 = row_split[[9]][10:13],
        group_15_49 = row_split[[10]][10:13],
        group_15_64 = row_split[[11]][10:13],
        group_18_60 = row_split[[12]][10:13],
        group_18_and_up = row_split[[13]][10:13],
        group_60_and_up = row_split[[14]][10:13],
        group_65_and_up = row_split[[15]][10:13]
      ) 
      
      row_out <- rbind(total_out, rural_out) %>%
        rbind(urban_out)
      
      table_05 <- rbind(table_05, row_out)
    }

    # cleanup numerics
    table_05 <- cbind(table_05 %>% dplyr::select(1:3),
          table_05 %>% dplyr::select(4:17) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_05[is.na(table_05)] <- 0
    if(d %in% c(54, 90, 107, 109, 112, 115, 128, 132)){
      table_05$group_15_64 <- NA
    }
            
    table_05 <- table_05 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())

    table_05$gender[table_05$gender == "ALL"] <- "ALL SEXES"
        
    prov_path <- paste0("./processed_forms/", unique(table_05$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_05, paste0(dist_path, "table_05.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_05 <- rbind(natl_table_05, table_05)
    
  } # end else 
  
  # Process Table 6, District Population by Marital Status ---
  if("06" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "06.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "06.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text <- gsub("ALL", "ALL SEXES", pdf_text)
    }
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    if(d == 112){
      pdf_text[6] <- "SHEIKHUPURA DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area
    area_start <- as.integer(grep("ALL SEXES", pdf_trimmed) - 1)
    area_end <- as.integer(grep("TRANSGENDER", pdf_trimmed) + 14)
    area_types <- c("TOTAL", "RURAL", "URBAN")

    table_06 <- tibble()
    # break down each administrative area
    for(k in 1:length(area_start)){
      area_extract <- pdf_trimmed[area_start[k]:area_end[k]]
      area_split <- strsplit(trimws(area_extract), "\\s{2,}")
  
      all_start <- as.integer(grep("ALL SEXES", area_split))
      male_start <- as.integer(grep("MALE", area_split)[[1]][1])
      fem_start <- as.integer(grep("FEMALE", area_split))
      trans_start <- as.integer(grep("TRANSGENDER", area_split))
      gender_starts <- c(all_start, male_start, fem_start, trans_start)
      gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
      age_types = c("15 AND ABOVE", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75 AND ABOVE")
      marital_types <- c("TOTAL", "NEVER MARRIED", "MARRIED", "WIDOWED", "DIVORCED")
      total_out <- tibble()
      
      for(g in 1:4){
        gender_extract <- area_split[gender_starts[g]:(gender_starts[g]+14)]
        for(m in 1:5){
          for(a in 1:14){
              row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              gender = gender_types[g],
              marital_status = marital_types[m],
              age_group = age_types[a],
              count = gender_extract[[a+1]][m+1]
              )
              
              total_out <- rbind(total_out, row_out)
              }
            }
          }
      table_06 <- rbind(table_06, total_out)  
    }

    # cleanup numerics
    table_06 <- cbind(table_06 %>% dplyr::select(1:5),
          table_06 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_06$count[is.na(table_06$count)] <- 0
            
    table_06 <- table_06 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_06$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_06, paste0(dist_path, "table_06.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_06 <- rbind(natl_table_06, table_06)
    
  } # end else  
          
  # Process Table 7, District Population by Relationship to Head of Household and Marital Status ---
  if("07" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "07.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "07.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d == 67){
      pdf_text[5] <- "KORANGI DISTRICT"
    }
    if(d == 85){
      pdf_text[34] <- "GRAND SON / GRAND DAUGHTER                       3,327                   2,835                        477               12                    3"
      pdf_text[35] <- ""
      pdf_text[79] <- "GRAND SON / GRAND DAUGHTER                         1,501                   1,179                        316                4                    2"
      pdf_text[80] <- ""
      pdf_text[124] <- "GRAND SON / GRAND DAUGHTER                       1,826                   1,656                        161                8                    1"
      pdf_text[125] <- ""
      pdf_text[153] <- ""
    }
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    if(d == 122){
       pdf_text[10] <- "MALE                                       -                   6,611            224,437             3,969               40"
       pdf_text[11] <- "FEMALE                                     -                     415             27,637             8,249               61"
       pdf_text[12] <- "TRANSGENDER                                -                      13                  -                 -                -"
       pdf_text[14] <- "MALE                                       -                       -              8,687                 -                -"
       pdf_text[15] <- "FEMALE                                     -                       -            229,515                 -                -"
       pdf_text[17] <- "MALE                                       -                 167,680            103,540               234               67"
       pdf_text[18] <- "FEMALE                                     -                  99,315              4,217               647              305"
       pdf_text[19] <- "TRANSGENDER                                -                       8                  -                 -                -"
       pdf_text[21] <- "MALE                                       -                       -              1,837                10                1"
       pdf_text[22] <- "FEMALE                                     -                       -            134,981               732               34"
       pdf_text[24] <- "MALE                                       -                  13,164              1,817                 1                3"
       pdf_text[25] <- "FEMALE                                     -                   8,775                920                19                4"
       pdf_text[26] <- "TRANSGENDER                                -                       2                  -                 -                -"
       pdf_text[28] <- "MALE                                       -                       -             11,126             2,025                1"
       pdf_text[29] <- "FEMALE                                     -                       -             16,632            18,343               27"
       pdf_text[31] <- "MALE                                       -                  22,702             29,467               185               31"
       pdf_text[32] <- "FEMALE                                     -                  15,923              1,621               730              121"
       pdf_text[33] <- "TRANSGENDER                                -                      16                  -                 -                -"
       pdf_text[35] <- "MALE                                       -                  11,087              8,549               399               16"
       pdf_text[36] <- "FEMALE                                     -                   8,599             46,810             3,047               38"
       pdf_text[37] <- "TRANSGENDER                                -                      62                  -                 -                -"
       pdf_text[39] <- "MALE                                       -                   5,203              7,429                68                4"
       pdf_text[40] <- "FEMALE                                     -                   1,906              5,888               304                9"
       pdf_text[41] <- "TRANSGENDER                                -                      32                  -                 -                -"
       pdf_text[42] <- "RURAL"
       pdf_text[44] <- "MALE                                       -                   4,508            153,361             2,726               25"
       pdf_text[45] <- "FEMALE                                     -                     301             20,675             5,388               34"
       pdf_text[46] <- "TRANSGENDER                                -                       4                  -                 -                -"
       pdf_text[48] <- "MALE                                       -                       -              6,019                 -                -"
       pdf_text[49] <- "FEMALE                                     -                       -            157,103                 -                -"
       pdf_text[51] <- "MALE                                       -                 111,736             70,649               150               36"
       pdf_text[52] <- "FEMALE                                     -                  66,812              2,787               380              179"
       pdf_text[53] <- "TRANSGENDER                                -                       3                  -                 -                -"
       pdf_text[55] <- "MALE                                       -                      -              1,268                 5                1"
       pdf_text[56] <- "FEMALE                                     -                       -             95,558               519               27"
       pdf_text[58] <- "MALE                                       -                   8,922              1,341                 1                3"
       pdf_text[59] <- "FEMALE                                     -                   5,992                635                15                3"
       pdf_text[60] <- "TRANSGENDER                                -                       -                  -                 -                -"
       pdf_text[62] <- "MALE                                       -                       -              8,145             1,460                1"
       pdf_text[63] <- "FEMALE                                     -                       -             12,059            12,719               18"
       pdf_text[65] <- "MALE                                       -                  15,597             20,082               121               23"
       pdf_text[66] <- "FEMALE                                     -                  11,141              1,101               444               77"
       pdf_text[67] <- "TRANSGENDER                                -                      11                  -                 -                -"
       pdf_text[69] <- "MALE                                       -                   7,364              5,852               286                9"
       pdf_text[70] <- "FEMALE                                     -                   5,951             33,684             2,197               25"
       pdf_text[71] <- "TRANSGENDER                                -                      44                  -                 -                -"
       pdf_text[73] <- "MALE                                       -                   2,732              4,190                54                2"
       pdf_text[74] <- "FEMALE                                     -                   1,300              4,426               217                6"
       pdf_text[75] <- "TRANSGENDER                                -                       3                  -                 -                -"
       pdf_text[76] <- "URBAN"
       pdf_text[78] <- "MALE                                       -                   2,103             71,076             1,243               15"
       pdf_text[79] <- "FEMALE                                     -                     114              6,962             2,861               27"
       pdf_text[80] <- "TRANSGENDER                                -                       9                  -                 -                -"
       pdf_text[82] <- "MALE                                       -                       -              2,668                 -                -"
       pdf_text[83] <- "FEMALE                                     -                       -             72,412                 -                -"
       pdf_text[85] <- "MALE                                       -                  55,944             32,891                84               31"
       pdf_text[86] <- "FEMALE                                     -                  32,503              1,430               267              126"
       pdf_text[87] <- "TRANSGENDER                                -                       5                  -                 -                -"
       pdf_text[89] <- "MALE                                       -                       -                569                 5                -"
       pdf_text[90] <- "FEMALE                                     -                       -             39,423               213                7"
       pdf_text[92] <- "MALE                                       -                   4,242                476                 -                -"
       pdf_text[93] <- "FEMALE                                     -                   2,783                285                 4                1"
       pdf_text[94] <- "TRANSGENDER                                -                       2                  -                 -                -"
       pdf_text[96] <- "MALE                                       -                       -              2,981               565                -"
       pdf_text[97] <- "FEMALE                                     -                       -              4,573             5,624                9"
       pdf_text[99] <- "MALE                                       -                   7,105              9,385                64                8"
       pdf_text[100] <- "FEMALE                                     -                   4,782                520               286               44"
       pdf_text[101] <- "TRANSGENDER                                -                       5                  -                 -                -"
       pdf_text[103] <- "MALE                                       -                   3,723              2,697               113                7"
       pdf_text[104] <- "FEMALE                                     -                   2,648             13,126               850               13"
       pdf_text[105] <- "TRANSGENDER                                -                      18                  -                 -                -"
       pdf_text[107] <- "MALE                                       -                   2,471              3,239                14                2"
       pdf_text[108] <- "FEMALE                                     -                     606              1,462                87                3"
       pdf_text[109] <- "TRANSGENDER                                -                      29                  -                 -                -"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    
    more_headers_starts <- grep("TABLE 7 - POPULATION", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("RELATIONSHIP TO HEAD", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}7\\s{6,}8", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*1\\s*2\\s*3\\s*4\\s*5", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}

    # in at least some cases, a blank row is throwing off the following
    pdf_trimmed <- pdf_trimmed[lengths(pdf_trimmed) > 0L]
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]

    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)

    area_types <- c("TOTAL", "RURAL", "URBAN")
    # make sure the starting total row is there
    if(!("TOTAL" %in% pdf_trimmed)){
      pdf_trimmed[[1]] <- "TOTAL"} else {}
    
    marital_types <- c("TOTAL", "NEVER MARRIED", "MARRIED", "WIDOWED", "DIVORCED")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    household_head_relations <- c("HEAD", "SPOUSE", "SON/DAUGHTER", "DAUGHTER/SON IN LAW", "GRAND SON/GRAND DAUGHTER", "FATHER/MOTHER", "BROTHER/SISTER", "OTHER RELATIVE", "NON-RELATIVE")
    
    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- gsub("NON- RELATIVE ", "NON-RELATIVE", pdf_trimmed)
    missing_areas <- area_types[!(area_types %in% pdf_trimmed)]
    
    table_07 <- tibble()
    # break down each administrative area
    for(k in 1:length(area_types)){
      if((area_types[k] %in% missing_areas) & k == 3){
        
        for(m in 1:length(marital_types)){
        for(a in 1:length(household_head_relations)){
          for(g in 1:length(gender_types)){
            row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              marital_status = marital_types[m],
              household_head_relationship = household_head_relations[a],
              gender = gender_types[g],
              count = NA
              )
              
              total_out <- rbind(total_out, row_out)
          }
        }
        }
            
      } else {
        
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- grep(area_types[k], area_split)
      
      if("URBAN" %in% missing_areas & k == 2){
        area_end <- length(area_split)
      } else {
        area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
        }
      
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(m in 1:length(marital_types)){
        for(a in 1:length(household_head_relations)){
          relation_start <- grep(household_head_relations[a], area_extract)
          relation_end <- ifelse(a != 9, (as.integer(grep(household_head_relations[a+1], area_extract)-1)), length(area_extract))
          relation_rows <- relation_start:relation_end
          relation_extract <- area_extract[relation_rows]
          relation_extract[[1]][1] <- "ALL SEXES"

          # check to make sure rows for all genders are present
          missing_genders <- gender_types[!(gender_types %in% unlist(relation_extract))]

          for(g in 1:length(gender_types)){
              row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              marital_status = marital_types[m],
              household_head_relationship = household_head_relations[a],
              gender = gender_types[g],
              count = ifelse(gender %in% missing_genders, NA, relation_extract[[g]][m+1])
              )
              
              total_out <- rbind(total_out, row_out)
        }
      }
      }
      }
    
      table_07 <- rbind(table_07, total_out)  
    }

    # cleanup numerics

    table_07 <- cbind(table_07 %>% dplyr::select(1:5),
          table_07 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )

    table_07 <- table_07 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    if(d == 113){
      table_07$count <- abs(table_07$count)
    }
    
    prov_path <- paste0("./processed_forms/", unique(table_07$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_07, paste0(dist_path, "table_07.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_07 <- rbind(natl_table_07, table_07)
    
  } # end else   

  # Process Table 8, District Population by Sex, Age, and Relationship to Head of Household ---
  if("08" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "08.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "08.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d == 65){
      pdf_text <- gsub("ALL", "ALL AGES", pdf_text)
      pdf_text <- gsub("ALL AGES SEX", "ALL SEXES", pdf_text)
      pdf_text <- c(pdf_text[1:82], "ALL SEXES", pdf_text[83:153])
    }
    if(d == 88){
      pdf_text[8] <- "MUSAKHEL DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("--", "-", pdf_trimmed)
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    household_head_relations <- c("TOTAL", "HEAD", "SPOUSE", "SON/DAUGHTER", "DAUGHTER/SON IN LAW", "GRAND SON/GRAND DAUGHTER", "FATHER/MOTHER", "BROTHER/SISTER", "OTHER RELATIVE", "NON-RELATIVE")
    age_groups <- c("ALL AGES", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")
    
    missing_areas <- area_types[!(area_types %in% pdf_trimmed)]
    table_08 <- tibble()

    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- gsub("NON- RELATIVE ", "NON-RELATIVE", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]

    # break down each administrative area
    for(k in 1:length(area_types)){
      if((area_types[k] %in% missing_areas) & k == 3){
        
        for(h in 1:length(household_head_relations)){
        for(a in 1:length(age_groups)){
          for(g in 1:length(gender_types)){
            row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              gender = gender_types[g],
              household_head_relationship = household_head_relations[h],
              age_group = age_groups[a],
              count = NA
              )
              
              total_out <- rbind(total_out, row_out)
          }
        }
        }
            
      } else {
        
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- grep(area_types[k], area_split)

      if("URBAN" %in% missing_areas & k == 2){
        area_end <- length(area_split)
      } else {
        area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      }

      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(g in 1:length(gender_types)){
          gender_start <- grep(gender_types[g], area_extract) # this is also catching the 'Male' in 'Female' but R is only using the first element so managed to get away with it still working
          gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
          gender_rows <- gender_start:gender_end
          gender_extract <- area_extract[gender_rows]

        for(a in 1:length(age_groups)){
          for(h in 1:length(household_head_relations)){
              row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              gender = gender_types[g],
              household_head_relationship = household_head_relations[h],
              age_group = age_groups[a],
              count = gender_extract[[a+1]][h+1]
              )
              
              total_out <- rbind(total_out, row_out)
          }
        }
      }
      }
    
      table_08 <- rbind(table_08, total_out)  
    }

    # cleanup numerics
    table_08 <- cbind(table_08 %>% dplyr::select(1:5),
          table_08 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_08$count[is.na(table_08$count)] <- 0
            
    table_08 <- table_08 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_08$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_08, paste0(dist_path, "table_08.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_08 <- rbind(natl_table_08, table_08)
    
  } # end else   
  
  # Process Table 9, Population by Religion ---
  if("09" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "09.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "09.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text[1:8], "OVERALL", pdf_text[9:12], 
                    "RURAL", pdf_text[14:18], 
                    "URBAN", rep("-    -   -   -   -   -   -   -", 4),
                    pdf_text[19], "OVERALL", pdf_text[20:24],
                    "RURAL", pdf_text[26:30], 
                    "URBAN", rep("-    -   -   -   -   -   -   -", 4),
                    pdf_text[31], "OVERALL", pdf_text[32:36],
                    "RURAL", pdf_text[38:42],
                    "URBAN", rep("-    -   -   -   -   -   -   -", 4),
                    pdf_text[43], "OVERALL", pdf_text[44:48],
                    "RURAL", pdf_text[50:54], 
                    "URBAN", rep("-    -   -   -   -   -   -   -", 4),
                    pdf_text[55], "OVERALL", pdf_text[56:60],
                    "RURAL", pdf_text[62:65],
                    "URBAN", rep("-    -   -   -   -   -   -   -", 4)
                    )
    }
    if(d == 88){
      pdf_text[7] <- "MUSAKHEL DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    more_headers_starts <- grep(" SCHEDULED", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("AREA/ SEX", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}7\\s{6,}8", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*4\\s*5\\s*6\\s*7\\s*8", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
        
    pdf_trimmed <- gsub("ALL SEXES", "ALL SEXES ", pdf_trimmed)
    pdf_trimmed <- gsub("TRANSGENDER", "TRANSGENDER ", pdf_trimmed)

    # find the breaks for each administrative area
    area_start <- as.integer(grep("TOTAL", pdf_trimmed) - 1)
    area_end <- as.integer(grep("URBAN", pdf_trimmed) + 4)
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    religious_affiliations <- c("TOTAL", "MUSLIM", "CHRISTIAN", "HINDU", "AHMADI", "SCHEDULED CASTES", "OTHERS")
    table_09 <- tibble()

    # break down each administrative area
    for(k in 1:length(area_start)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_extract <- area_split[area_start[k]:area_end[k]]
      total_out <- tibble()

      total_start <- grep("TOTAL", area_extract)
      rural_start <- grep("RURAL", area_extract)
      urban_start <- grep("URBAN", area_extract)
      area_starts <- c(total_start, rural_start, urban_start)
      
      for(a in 1:length(area_types)){
        area_sub <- area_extract[area_starts[a]:ifelse(a != 3, area_starts[a+1]-1, length(area_extract))]
        for(g in 1:length(gender_types)){
          for(r in 1:length(religious_affiliations)){
                row_out <- tibble(
                area_name = area_extract[[1]][1],
                area_type = area_types[a],
                gender = gender_types[g],
                religious_affiliation = religious_affiliations[r],
                count = area_sub[[g+1]][r+1]
                )
                
                total_out <- rbind(total_out, row_out)
            
          }
        }
        
      }
    
      table_09 <- rbind(table_09, total_out)  
    }

    # cleanup numerics

    table_09 <- cbind(table_09 %>% dplyr::select(1:4),
          table_09 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_09$count[is.na(table_09$count)] <- 0
    if(d == 65){
      table_09$count[table_09$area_type == "URBAN"] <- NA
    }

    table_09 <- table_09 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_09$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_09, paste0(dist_path, "table_09.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_09 <- rbind(natl_table_09, table_09)
    
  } # end else   

  # Process Table 10, District Population by Nationality, Age, and Gender ---
    if("10" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "10.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "10.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text[1:7], "ALL SEXES",
      "ALL AGES                   784711           784254          457               784711           784254          457              -               -               -",
      pdf_text[9:26],
      "ALL AGES                   424643           424366          277               424643           424366          277              -               -               -",
      pdf_text[28:45],
      "ALL AGES                  360055           359875          180               360055           359875          180              -               -               -",
      pdf_text[47:64],
      "ALL AGES                        13               13               -                13               13               -           -               -               -",
      pdf_text[66:82]
      )
    }
    if(d == 88){
      pdf_text[5] <- "MUSAKHEL DISTRICT"
    }
    if(d == 109){
      pdf_text[5] <- "SARGODHA DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("--", "-", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)  
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    nationality_types <- c("TOTAL", "PAKISTANI", "NON-PAKISTANI")
    age_groups <- c("ALL AGES", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE", "18 AND ABOVE")
    table_10 <- tibble()

    # break down each administrative area
    for(g in 1:length(gender_types)){
      gender_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      gender_start <- grep(gender_types[g], gender_split)
      gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], gender_split)-1)), length(gender_split))
      gender_extract <- gender_split[gender_start:gender_end]
      total_out <- tibble()
      area_cols <- list(c(2:4), c(5:7), c(8:10))
      
      for(a in 1:length(age_groups)){
        age_extract <- gender_extract[[a+1]]
        for(k in 1:length(area_types)){
          area_extract <- age_extract[area_cols[[k]]]
          for(h in 1:length(nationality_types)){
              row_out <- tibble(
              area_name = district_name,
              area_type = area_types[k],
              gender = gender_types[g],
              nationality_status = nationality_types[h],
              age_group = age_groups[a],
              count = area_extract[h]
              )
              
              total_out <- rbind(total_out, row_out)
          }
        }
      }
    table_10 <- rbind(table_10, total_out)  
    }

    # cleanup numerics

    table_10 <- cbind(table_10 %>% dplyr::select(1:5),
          table_10 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_10$count[is.na(table_10$count)] <- 0
    if(d == 65){
      table_10$count[table_10$area_type == "URBAN"] <- NA
      table_10$count[table_10$area_name %in% c("DASSU SUB DIVISION", "KANDIA SUB DIVISION", "PALAS SUB DIVISION", "PATAN SUB DIVISION") & table_10$area_type == "RURAL"] <- NA
    }
    
    table_10 <- table_10 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_10$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_10, paste0(dist_path, "table_10.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_10 <- rbind(natl_table_10, table_10)
    
  } # end else   

  # Process Table 11, Population by Mother Tongue ---
  if("11" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "11.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "11.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 26){
        pdf_text[8] <- "ALL SEXES      26,394         86        1        9    26,232       13       12       16            -       23     2"    
        pdf_text[9] <- "MALE           13,685         54        -        6    13,585        8        7       10            -       14     1"    
        pdf_text[10] <- "FEMALE         12,709         32        1        3    12,647        5        5        6            -        9     1"    
        pdf_text[11] <- "TRANSGENDER         -          -        -        -         -        -        -        -            -        -     -"    
        pdf_text[13] <- "ALL SEXES      26,394         86        1        9    26,232       13       12       16            -       23      2"    
        pdf_text[14] <- "MALE           13,685         54        -        6    13,585        8        7       10            -       14      1"    
        pdf_text[15] <- "FEMALE         12,709         32        1        3    12,647        5        5        6            -        9      1"    
        pdf_text[16] <- "TRANSGENDER         -          -        -        -         -        -        -        -            -        -      -"    
        pdf_text[18] <- "ALL SEXES             -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[19] <- "MALE                  -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[20] <- "FEMALE                -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[21] <- "TRANSGENDER           -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[24] <- "ALL SEXES      26,394         86        1        9    26,232       13       12       16            -       23     2"    
        pdf_text[25] <- "MALE           13,685         54        -        6    13,585        8        7       10            -       14     1"    
        pdf_text[26] <- "FEMALE         12,709         32        1        3    12,647        5        5        6            -        9     1"    
        pdf_text[27] <- "TRANSGENDER         -          -        -        -         -        -        -        -            -        -     -"    
        pdf_text[29] <- "ALL SEXES      26,394         86        1        9    26,232       13       12       16            -       23     2"    
        pdf_text[30] <- "MALE           13,685         54        -        6    13,585        8        7       10            -       14     1"    
        pdf_text[31] <- "FEMALE         12,709         32        1        3    12,647        5        5        6            -        9     1"    
        pdf_text[32] <- "TRANSGENDER         -          -        -        -         -        -        -        -            -        -     -"    
        pdf_text[34] <- "ALL SEXES             -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[35] <- "MALE                  -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[36] <- "FEMALE                -        -         -        -        -        -        -        -            -            -    -"
        pdf_text[37] <- "TRANSGENDER           -        -         -        -        -        -        -        -            -            -    -"
    }
    if(d == 65){
      pdf_text <- c(pdf_text[1:6], "OVERALL", pdf_text[7:10], 
                    "RURAL", pdf_text[12:16], 
                    "URBAN", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    pdf_text[17], "OVERALL", pdf_text[18:21],
                    "RURAL", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    "URBAN", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    pdf_text[22], "OVERALL", pdf_text[23:26],
                    "RURAL", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    "URBAN", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    pdf_text[27], "OVERALL", pdf_text[28:31],
                    "RURAL", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    "URBAN", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    pdf_text[32], "OVERALL", pdf_text[33:36],
                    "RURAL", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4),
                    "URBAN", rep("-    -   -   -   -   -   -   -    -   -   -   -", 4)
                    )
    }        
    if(d == 72){
      pdf_text[87] <- "OVERALL"
      pdf_text[88] <- "ALL SEXES        12,389           37          30          3,528            117             52        14               57             15       6,396          2,143"
      pdf_text[89] <- "MALE              6,871           32          25          1,919              113             39        7             40           10       3,396        1,290" 
      pdf_text[90] <- "FEMALE            5,518            5           5          1,609              4              13        7              17              5       3,000            853"
      pdf_text[91] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[96] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[103] <- "OVERALL"
      pdf_text[104] <- "ALL SEXES        35,495           98          77         15,842           125            6,395     221           104                96            623      11,914"
      pdf_text[105] <- "MALE             18,094           52          43          7,944        82            3,324     113            51                46            337       6,102"
      pdf_text[106] <- "FEMALE           17,400           46          34          7,898           43            3,071     107            53                50            286       5,812"
      pdf_text[107] <- "TRANSGENDER           1            -           -              -    -                -       1             -                 -              -           -"
      pdf_text[108] <- "RURAL"
      pdf_text[109] <- "ALL SEXES        35,495           98          77         15,842           125            6,395     221           104                96            623      11,914"
      pdf_text[110] <- "MALE             18,094           52          43          7,944        82            3,324     113            51                46            337       6,102"
      pdf_text[111] <- "FEMALE           17,400           46          34          7,898           43            3,071     107            53                50            286       5,812"
      pdf_text[112] <- "TRANSGENDER           1            -           -              -    -                -       1             -                 -              -           -"
      pdf_text[113] <- "URBAN"
      pdf_text[114] <- "ALL SEXES   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[115] <- "MALE   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[116] <- "FEMALE   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[117] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[119] <- "OVERALL"
      pdf_text[120] <- "ALL SEXES        11,730           45           9           883              86           9,696       1               1               3              8        998"
      pdf_text[121] <- "MALE              5,990           24           9           477          70           4,904       1               -               2              4        499"
      pdf_text[122] <- "FEMALE            5,740           21           -           406            16           4,792       -               1               1              4        499"
      pdf_text[123] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[124] <- "RURAL"
      pdf_text[125] <- "ALL SEXES        11,730           45           9           883              86           9,696       1               1               3              8        998"
      pdf_text[126] <- "MALE              5,990           24           9           477          70           4,904       1               -               2              4        499"
      pdf_text[127] <- "FEMALE            5,740           21           -           406            16           4,792       -               1               1              4        499"
      pdf_text[128] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[129] <- "URBAN"
      pdf_text[130] <- "ALL SEXES   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[131] <- "MALE   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[132] <- "FEMALE   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[133] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[135] <- "OVERALL"
      pdf_text[136] <- "ALL SEXES        64,019          268         728          4,066      3,142        54,363        16           265               218            581        372"
      pdf_text[137] <- "MALE             33,220          127         395          2,102       1,614        28,182         9           154               119            302        216"
      pdf_text[138] <- "FEMALE           30,797          141         333          1,964        1,528        26,179         7           111                99            279        156" 
      pdf_text[139] <- "TRANSGENDER           2            -           -              -      -             2         -             -                 -              -          -"
      pdf_text[140] <- "RURAL"
      pdf_text[141] <- "ALL SEXES        34,504          195         184          3,187           567          29,791      10               44             16            151        359"
      pdf_text[142] <- "MALE             17,867           91          98          1,649          292          15,405       5               30              9             78        210"
      pdf_text[143] <- "FEMALE           16,637          104          86          1,538            275          14,386       5               14              7             73        149"
      pdf_text[144] <- "TRANSGENDER   -   -   -   -   -   -   -    -   -   -   -"
      pdf_text[145] <- "URBAN"
      pdf_text[146] <- "ALL SEXES        29,515           73         544           879         2,575          24,572       6           221               202            430            13"
      pdf_text[147] <- "MALE             15,353           36         297           453         1,322          12,777       4           124               110            224             6"
      pdf_text[148] <- "FEMALE           14,160           37         247           426           1,253          11,793       2            97                92            206             7"
      pdf_text[149] <- "TRANSGENDER           2            -           -             -        -               2       -             -                 -              -             -"
    }
    if(d == 80){
      pdf_text[5] <- "MANSEHRA DISTRICT"
    }
    if(d == 88){
      pdf_text[6] <- "MUSAKHEL DISTRICT"
    }
        
    # OKARA DISTRICT DUPLICATES TABLE 10 HERE INSTEAD
    if(d == 97){
      table_11 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          area_name = NA, area_type = NA, gender = NA, mother_tongue = NA, count = NA
        )
      
      natl_table_11 <- rbind(natl_table_11, table_11)
    } else {
      
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    more_headers_starts <- grep("POPULATION BY MOTHER TONGUE", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("AREA/SEX", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}7\\s{6,}8", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*4\\s*5\\s*6\\s*7\\s*8", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    # find the breaks for each administrative area
    area_start <- as.integer(grep("TOTAL", pdf_trimmed) - 1)
    area_end <- as.integer(grep("URBAN", pdf_trimmed) + 4)
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    mother_tongues <- c("TOTAL", "URDU", "PUNJABI", "SINDHI", "PUSHTO", "BALOCHI", "KASHMIRI", "SARAIKI", "HINDKO", "BRAHVI", "OTHER")
    table_11 <- tibble()

    # break down each administrative area
    for(k in 1:length(area_start)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_extract <- area_split[area_start[k]:area_end[k]]
      total_out <- tibble()

      total_start <- grep("TOTAL", area_extract)
      rural_start <- grep("RURAL", area_extract)
      urban_start <- grep("URBAN", area_extract)
      area_starts <- c(total_start, rural_start, urban_start)
      
      for(a in 1:length(area_types)){
        area_sub <- area_extract[area_starts[a]:ifelse(a != 3, area_starts[a+1]-1, length(area_extract))]
        for(g in 1:length(gender_types)){
          for(m in 1:length(mother_tongues)){
                row_out <- tibble(
                area_name = area_extract[[1]][1],
                area_type = area_types[a],
                gender = gender_types[g],
                mother_tongue = mother_tongues[m],
                count = area_sub[[g+1]][m+1]
                )
                
                total_out <- rbind(total_out, row_out)
            
          }
        }
        
      }
    
      table_11 <- rbind(table_11, total_out)  
    }

    # cleanup numerics

    table_11 <- cbind(table_11 %>% dplyr::select(1:4),
          table_11 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_11$count[is.na(table_11$count)] <- 0

    table_11 <- table_11 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_11$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_11, paste0(dist_path, "table_11.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_11 <- rbind(natl_table_11, table_11)
    }
  } # end else   
    
  # Process Table 12, Population by Literacy and Age Group  ---
  if("12" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "12.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "12.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub(", ", "", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("---                      -                -                 -               -                -              -", 15),
                    "MALE", rep("---                      -                -                 -               -                -              -", 15),
                    "FEMALE", rep("---                      -                -                 -               -                -              -", 15),
                    "TRANSGENDER", rep("---                      -                -                 -               -                -              -", 15))
    }
    if(d == 88){
      pdf_text[8] <- "MUSAKHEL DISTRICT"
    }
    if(d == 103){
      pdf_text <- c(pdf_text[1:723], pdf_text[725:859])
    }
    if(d == 109){
      pdf_text[1104] <- "SARGODHA TEHSIL"
      pdf_text[1324] <- "SHAHPUR TEHSIL"
      pdf_text[1538] <- "SILLANWALI TEHSIL"
    }

    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    more_headers_starts <- grep("TABLE 12 - POPULATION", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("\\s{15,}LITERATE", pdf_trimmed)}
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("SEX/ AGE GROUP", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}6\\s{6,}7", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("1\\s*2\\s*3\\s*4\\s*5\\s*6\\s*7", pdf_trimmed)
      }
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s{25,}FORMAL", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    # find the breaks for each administrative area
    area_types <- c("OVERALL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    literacy_types <- c("ILLITERATE", "TOTAL LITERATE", "FORMAL LITERATE", "INFORMAL LITERATE")
    age_groups <- c("10 AND ABOVE", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")
    
    area_starts <- as.integer(grep("OVERALL", pdf_trimmed) - 1)
    
    table_12 <- tibble()
    for(k in 1:length(area_starts)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_end <- ifelse(k != length(area_starts), (as.integer(area_starts[k+1]) - 1), length(area_split))
      area_split <- area_split[area_starts[k]:area_end]
      for(r in 1:length(area_types)){
        area_sub_start <- grep(area_types[r], area_split)
        area_sub_end <- ifelse(r != 3, (as.integer(grep(area_types[r+1], area_split)-1)), length(area_split))
        area_extract <- area_split[area_sub_start:area_sub_end]
        total_out <- tibble()
        
        for(g in 1:length(gender_types)){
          gender_start <- grep(gender_types[g], area_extract)
          gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
          gender_extract <- area_extract[gender_start:gender_end]
          
          for(l in 1:length(literacy_types)){
            for(a in 1:length(age_groups)){
                  row_out <- tibble(
                  area_name = area_split[[1]][1],
                  area_type = area_types[r],
                  gender = gender_types[g],
                  age_group = age_groups[a],
                  total_population = gender_extract[[a+1]][2],
                  literacy_ratio = gender_extract[[a+1]][7],
                  literacy_status = literacy_types[l],
                  count = gender_extract[[a+1]][l+2]
                  )
                  
                  total_out <- rbind(total_out, row_out)
            }
        }
        }
        table_12 <- rbind(table_12, total_out)  
        }

    }
    
    # cleanup numerics
    table_12 <- cbind(
          table_12 %>% dplyr::select(1:4),
          table_12 %>% dplyr::select(total_population, literacy_ratio) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_12 %>% dplyr::select(literacy_status),
          table_12 %>% dplyr::select(c(count)) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_12$count[is.na(table_12$count)] <- 0
    table_12$total_population[is.na(table_12$total_population)] <- 0
    table_12$literacy_ratio[is.na(table_12$literacy_ratio)] <- 0
    table_12$area_type[table_12$area_type == "OVERALL"] <- "TOTAL"
    
    if(d == 65){
      table_12$count[table_12$area_type == "URBAN"] <- NA
      table_12$total_population[table_12$area_type == "URBAN"] <- NA
      table_12$literacy_ratio[table_12$area_type == "URBAN"] <- NA
    }

    table_12 <- table_12 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_12$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_12, paste0(dist_path, "table_12.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_12 <- rbind(natl_table_12, table_12)
    
  } # end else   
  
  # Process Table 13, Over-10 Aged Population by Literacy ---
  if("13" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "13.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "13.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    # CORRECT FOR PAROA TEHSIL IN DI KHAN WHICH OMITS THE FEMALE RURAL POP ROW; VALUES HERE DERIVED FROM DIFFERENCE OF TOTAL - M+T POP
    if(d == 21){
      pdf_text <- c(pdf_text[1:96],
                       "FEMALE          79377          69503          9874          9494          380          12.44",
                       pdf_text[97:102])
    }    
    if(d == 65){
      pdf_text <- c(pdf_text[1:8], "OVERALL", pdf_text[9:12], "RURAL", pdf_text[14:17], "URBAN", pdf_text[19:22])
    }
    if(d == 88){
      pdf_text[7] <- "MUSAKHEL DISTRICT"
    }

    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]

    # find the breaks for each administrative area
    area_start <- as.integer(grep("OVERALL", pdf_trimmed) - 1)
    area_end <- as.integer(grep("URBAN", pdf_trimmed) + 4)
    
    area_types <- c("OVERALL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    literacy_types <- c("ILLITERATE", "TOTAL LITERATE", "FORMAL LITERATE", "INFORMAL LITERATE")
    table_13 <- tibble()

    # break down each administrative area
    for(k in 1:length(area_start)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_extract <- area_split[area_start[k]:area_end[k]]
      total_out <- tibble()

      total_start <- grep("OVERALL", area_extract)
      rural_start <- grep("RURAL", area_extract)
      urban_start <- grep("URBAN", area_extract)
      area_starts <- c(total_start, rural_start, urban_start)
      
      for(a in 1:length(area_types)){
        area_sub <- area_extract[area_starts[a]:ifelse(a != 3, area_starts[a+1]-1, length(area_extract))]
        for(g in 1:length(gender_types)){
          for(l in 1:length(literacy_types)){
                row_out <- tibble(
                area_name = area_extract[[1]][1],
                area_type = area_types[a],
                gender = gender_types[g],
                total_population = area_sub[[g+1]][2],
                literacy_ratio = area_sub[[g+1]][7],
                literacy_status = literacy_types[l],
                count = area_sub[[g+1]][l+2]
                )
                
                total_out <- rbind(total_out, row_out)
            
          }
        }
        
      }
    
      table_13 <- rbind(table_13, total_out)  
    }

    # cleanup numerics

    table_13 <- cbind(
          table_13 %>% dplyr::select(1:3),
          table_13 %>% dplyr::select(total_population, literacy_ratio) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_13 %>% dplyr::select(literacy_status),
          table_13 %>% dplyr::select(c(count)) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_13$count[is.na(table_13$count)] <- 0
    table_13$total_population[is.na(table_13$total_population)] <- 0
    table_13$literacy_ratio[is.na(table_13$literacy_ratio)] <- 0
    table_13$area_type[table_13$area_type == "OVERALL"] <- "TOTAL"
    
    table_13 <- table_13 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_13$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_13, paste0(dist_path, "table_13.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_13 <- rbind(natl_table_13, table_13)
    
  } # end else   
    
  # Process Table 14, Over-10 Literate Population Educational Attainment ---
  if("14" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "14.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "14.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("ABOVE", "ABOVE  ", pdf_text)
    
    if(d == 2){
      pdf_text[432] <- "FATEH JANG TEHSIL"
      pdf_text[433] <- "OVERALL"
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("---            -          -       -         -            -            -         -         -          -        -", 15),
                    "MALE", rep("---            -          -       -         -            -            -         -         -          -        -", 15),
                    "FEMALE", rep("---            -          -       -         -            -            -         -         -          -        -", 15),
                    "TRANSGENDER", rep("---            -          -       -         -            -            -         -         -          -        -", 15))
    }
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    if(d == 133){
      pdf_text <- c(pdf_text[1:1038], "WASHUK SUB-TEHSIL", pdf_text[1039:1240])
    }
    pdf_text <- pdf_text[pdf_text != ""]
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    more_headers_starts <- grep("TABLE 14 - LITERATE POPULATION", pdf_trimmed)
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("\\s{15,}LITERATE POPULATION", pdf_trimmed)}
    if(length(more_headers_starts) == 0){
      more_headers_starts <- grep("SEX/ AGE GROUP", pdf_trimmed)}
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}10\\s{6,}11", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("1\\s*2\\s*3\\s*4\\s*5\\s*6\\s*7\\s*8\\s*9\\s*10\\s*11", pdf_trimmed)
      }

      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}   


    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    ed_types <- c("TOTAL", "BELOW PRIMARY", "PRIMARY", "MIDDLE", "MATRIC", "INTERMEDIATE",
                  "GRADUATE", "MASTERS AND ABOVE", "DIPLOMA/CERTIFICATE", "OTHER")
    age_groups <- c("10 AND ABOVE", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")
    
    area_starts <- as.integer(grep("TOTAL", pdf_trimmed) - 1)
    
    table_14 <- tibble()
    for(k in 1:length(area_starts)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_end <- ifelse(k != length(area_starts), (as.integer(area_starts[k+1]) - 1), length(area_split))
      area_split <- area_split[area_starts[k]:area_end]
      for(r in 1:length(area_types)){
        area_sub_start <- grep(area_types[r], area_split)
        area_sub_end <- ifelse(r != 3, (as.integer(grep(area_types[r+1], area_split)-1)), length(area_split))
        area_extract <- area_split[area_sub_start:area_sub_end]
        total_out <- tibble()
        
        for(g in 1:length(gender_types)){
          gender_start <- grep(gender_types[g], area_extract)
          gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
          gender_extract <- area_extract[gender_start:gender_end]
          
          for(l in 1:length(ed_types)){
            for(a in 1:length(age_groups)){
                  row_out <- tibble(
                  area_name = area_split[[1]][1],
                  area_type = area_types[r],
                  gender = gender_types[g],
                  ed_attainment = ed_types[l],
                  age_group = age_groups[a],
                  count = gender_extract[[a+1]][l+1],
                  )
                  
                  total_out <- rbind(total_out, row_out)
            }
        }
        }
        table_14 <- rbind(table_14, total_out)  
        }

    }
    
    # cleanup numerics

    table_14 <- cbind(table_14 %>% dplyr::select(1:5),
          table_14 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_14$count[is.na(table_14$count)] <- 0
    if(d == 65){
      table_14$count[table_14$area_type == "URBAN"] <- NA
    }
    
    table_14 <- table_14 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_14$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_14, paste0(dist_path, "table_14.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_14 <- rbind(natl_table_14, table_14)
    
  } # end else 
            
  # Process Table 15, District-Wide Over-5 Literate Population by Educational Attainment ---
  if("15" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "15.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "15.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("ABOVE", "ABOVE  ", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("---           -           -        -           -            -            -           -        -         -          -          -", 9),
                    "MALE", rep("---           -           -        -           -            -            -           -        -         -          -          -", 9),
                    "FEMALE", rep("---           -           -        -           -            -            -           -        -         -          -          -", 9),
                    "TRANSGENDER", rep("---           -           -        -           -            -            -           -        -         -          -          -", 9))
      pdf_text[54] <- "RURAL"
    }
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    if(d == 118){
      pdf_text[13] <- ""
      pdf_text[14] <- "5 AND ABOVE          554,377     362,389    71,685     44,866      34,995       23,031        8,325    4,395     2,736           216     1,739"
      pdf_text[22] <- "40 AND ABOVE          104,146      86,206     2,659      4,060       4,860        3,425        1,170      816       698            83       169"
      pdf_text[23] <- ""
      pdf_text[25] <- ""
      pdf_text[26] <- "5 AND ABOVE          293,518     150,710    46,116     32,931      29,275       20,213        7,067    3,640     2,378           187     1,001"
      pdf_text[36] <- ""
      pdf_text[37] <- "5 AND ABOVE          260,809     211,636    25,567     11,933       5,719        2,816        1,258        755        358        29           738"
      pdf_text[58] <- ""
      pdf_text[59] <- "5 AND ABOVE          554,377     362,389    71,685     44,866      34,995       23,031        8,325    4,395     2,736           216     1,739"
      pdf_text[67] <- "40 AND ABOVE          104,146      86,206     2,659      4,060       4,860        3,425        1,170      816       698            83       169"
      pdf_text[68] <- ""
      pdf_text[70] <- ""
      pdf_text[71] <- "5 AND ABOVE          293,518     150,710    46,116     32,931      29,275       20,213        7,067    3,640     2,378           187     1,001"
      pdf_text[81] <- ""
      pdf_text[82] <- "5 AND ABOVE          260,809     211,636    25,567     11,933       5,719        2,816        1,258        755        358        29           738"
    }
    
    pdf_text <- pdf_text[pdf_text != ""]
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    ed_types <- c("TOTAL", "NEVER ATTAINED", "BELOW PRIMARY", "PRIMARY", "MIDDLE", "MATRIC", "INTERMEDIATE",
                  "GRADUATE", "MASTERS AND ABOVE", "DIPLOMA/CERTIFICATE", "OTHER")
    age_groups <- c("5 AND ABOVE", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 AND ABOVE")

    table_15 <- tibble()
    for(k in 1:length(area_types)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(g in 1:length(gender_types)){
        gender_start <- grep(gender_types[g], area_extract)
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_start:gender_end]
        
        for(l in 1:length(ed_types)){
          for(a in 1:length(age_groups)){
                row_out <- tibble(
                area_name = district_name,
                area_type = area_types[k],
                gender = gender_types[g],
                ed_attainment = ed_types[l],
                age_group = age_groups[a],
                count = gender_extract[[a+1]][l+1],
                )
                
                total_out <- rbind(total_out, row_out)
          }
        }
      }

        table_15 <- rbind(table_15, total_out)  
    }
    
    # cleanup numerics

    table_15 <- cbind(table_15 %>% dplyr::select(1:5),
          table_15 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_15$count[is.na(table_15$count)] <- 0

    table_15 <- table_15 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_15$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_15, paste0(dist_path, "table_15.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_15 <- rbind(natl_table_15, table_15)
    
  } # end else 
    
  # Process Table 16, Over-10 by Work Status ---
  if("16" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "16.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "16.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 54){
      pdf_text <- c(pdf_text, "75 & ABOVE   -   -   -   -   -   -")
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("---                             -                       -                    -               -                   -              -", 15),
                    "MALE", rep("---                            -                       -                    -               -                   -              -", 15),
                    "FEMALE", rep("---                            -                       -                    -               -                   -              -", 15),
                    "TRANSGENDER", rep("---                            -                       -                    -               -                   -              -", 15))
      pdf_text[60] <- "TRANSGENDER"
      pdf_text[76] <- "RURAL"
      pdf_text[125] <- "TRANSGENDER"
    }
    if(d == 88){
      pdf_text[11] <- "MUSAKHEL DISTRICT"
    }    
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("OVER ALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- gsub("\\s{2}- (?=\\d)", "-  ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    work_types <- c("TOTAL POPULATION", "WORKING", "SEEKING WORK", "STUDENT", "HOUSEKEEPING", "OTHER")
    age_groups <- c("10 AND ABOVE", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")

    table_16 <- tibble()
    for(k in 1:length(area_types)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(g in 1:length(gender_types)){
        gender_start <- grep(gender_types[g], area_extract)
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_start:gender_end]
        
        for(w in 1:length(work_types)){
          for(a in 1:length(age_groups)){
                row_out <- tibble(
                area_name = district_name,
                area_type = area_types[k],
                gender = gender_types[g],
                work_activity = work_types[w],
                age_group = age_groups[a],
                count = gender_extract[[a+1]][w+1],
                )
                
                total_out <- rbind(total_out, row_out)
          }
        }
      }

        table_16 <- rbind(table_16, total_out)  
    }
    
    # cleanup numerics
    table_16 <- cbind(table_16 %>% dplyr::select(1:5),
          table_16 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_16$count[is.na(table_16$count)] <- 0
    if(d == 65){
      table_16$count[table_16$area_type == "URBAN"] <- NA
    }
    
    table_16 <- table_16 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_16$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_16, paste0(dist_path, "table_16.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_16 <- rbind(natl_table_16, table_16)
    
  } # end else 
      
  # Process Table 17, Disabled Population ---
  if("17" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "17.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "17.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("KILLA ABDULLAB DISTRICT", "KILLA ABDULLAH DISTRICT", pdf_text)
    if(d == 52){
      pdf_text[46] <- "75 & ABOVE       -     -    -     -"
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("---                             -                       -                    -               -", 9),
                    "MALE", rep("---                             -                       -                    -               -", 9),
                    "FEMALE", rep("---                             -                       -                    -               -", 9),
                    "TRANSGENDER", rep("---                             -                       -                    -               -", 9))
      pdf_text[26] <- "RURAL"
    }  
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)    
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    pdf_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
        
    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    
    
    # NEED TO CORRECT TO ACCOUNT FOR CASE IN BAJAUR WHERE THEY OMMITTED THE 'URBAN' SECTION HEADER
    if(d == 7){
      pdf_split <- c(pdf_split[1:37], "URBAN", pdf_split[38:54])
    }
    
    # check and confirm if all area types are present
    #missing_areas <- area_types[!(area_types %in% pdf_split)]
    
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    age_groups <- c("ALL AGES", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")

    table_17 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], pdf_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], pdf_split)-1)), length(pdf_split))
      area_extract <- pdf_split[area_start:area_end]
      
      total_out <- tibble()

       for(g in 1:length(gender_types)){
          for(a in 1:length(age_groups)){
                row_out <- tibble(
                area_name = district_name,
                area_type = area_types[k],
                gender = gender_types[g],
                age_group = age_groups[a],
                count = area_extract[[a+1]][g+1],
                )
                
                total_out <- rbind(total_out, row_out)
          }
       }

        table_17 <- rbind(table_17, total_out)  
    }
    
    # cleanup numerics

    table_17 <- cbind(table_17 %>% dplyr::select(1:4),
          table_17 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_17$count[is.na(table_17$count)] <- 0
    if(d == 65){
      table_17$count[table_17$area_type == "URBAN"] <- NA
    }    
    
    table_17 <- table_17 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_17$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_17, paste0(dist_path, "table_17.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_17 <- rbind(natl_table_17, table_17)
    
  } # end else 
    
  # Process Table 18, Disabled Population Educational Attainment ---
  if("18" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "18.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "18.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    
    if(d == 44){
      pdf_text <- c(pdf_text[1:9], "JHELUM DISTRICT", pdf_text[10:216])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL DISABLED", rep("               -              -                   -                   -           -         -            -              -", 16),
                    "MALE", rep("               -              -                   -                   -           -         -            -              -", 16),
                    "FEMALE", rep("               -              -                   -                   -           -         -            -              -", 16),
                    "TRANSGENDER", rep("               -              -                   -                   -           -         -            -              -", 16))
    } 
    if(d == 88){
      pdf_text[12] <- "MUSAKHEL DISTRICT"
    }
    if(d == 118){
      pdf_text[15] <- ""
      pdf_text[16] <- "05 YEARS AND ABOVE              1,283          255       1,028            240         185              42         10              3"
      pdf_text[33] <- ""
      pdf_text[34] <- "05 YEARS AND ABOVE                844          188           656          176         133              32          9              2"
      pdf_text[51] <- ""
      pdf_text[52] <- "05 YEARS AND ABOVE                431           67           364           64          52              10          1              1"
      pdf_text[87] <- ""
      pdf_text[88] <- "05 YEARS AND ABOVE              1,283          255       1,028            240         185              42         10              3"
      pdf_text[105] <- ""
      pdf_text[106] <- "05 YEARS AND ABOVE                844          188           656          176         133              32          9              2"
      pdf_text[123] <- ""
      pdf_text[124] <- "05 YEARS AND ABOVE                431           67           364           64          52              10          1              1"
    }
    if(d == 124){
      pdf_text[13] <- ""
      pdf_text[14] <- "05 YEARS AND ABOVE                 2,748                 531           2,217            516             405                88                19               4"
      pdf_text[32] <- ""
      pdf_text[33] <- "05 YEARS AND ABOVE                 1,780                 408           1,372            397             315                70                11               1"
      pdf_text[88] <- ""
      pdf_text[89] <- "05 YEARS AND ABOVE                 2,308                 396           1,912            384             312                61                    7            4"
      pdf_text[107] <- ""
      pdf_text[108] <- "05 YEARS AND ABOVE                 1,510                 321           1,189            313             255                53                    4            1"
    }
    
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("ABOVE", "ABOVE  ", pdf_text)
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL DISABLED", "MALE", "FEMALE", "TRANSGENDER")
    ed_types <- c("TOTAL DISABLED POPULATION", "TOTAL DISABLED LITERATE", "TOTAL DISABLED ILLITERATE", 
                  "TOTAL DISABLED FORMALLY LITERATE", "BELOW MATRIC", "MATRIC BELOW DEGREE", "DEGREE AND ABOVE", "OTHER")
    age_groups <- c("5 AND ABOVE", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")

    table_18 <- tibble()
    
    for(k in 1:length(area_types)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- as.integer(grep("ALL DISABLED", area_split)[k]) - 1 # in at least some cases, "OVERALL" header was ommitted
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(g in 1:length(gender_types)){
        gender_start <- grep(gender_types[g], area_extract)
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_start:gender_end]
        
        for(l in 1:length(ed_types)){
          for(a in 1:length(age_groups)){
                row_out <- tibble(
                area_name = district_name,
                area_type = area_types[k],
                gender = gender_types[g],
                ed_attainment = ed_types[l],
                age_group = age_groups[a],
                count = gender_extract[[a+1]][l+1],
                )
                
                total_out <- rbind(total_out, row_out)
          }
        }
      }

        table_18 <- rbind(table_18, total_out)  
    }
    
    # cleanup numerics

    table_18 <- cbind(table_18 %>% dplyr::select(1:5),
          table_18 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_18$count[is.na(table_18$count)] <- 0
    table_18$gender[table_18$gender == "ALL DISABLED"] <- "ALL SEXES"
    if(d == 65){
      table_18$count[table_18$area_type == "URBAN"] <- NA
    }   
    
    table_18 <- table_18 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_18$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_18, paste0(dist_path, "table_18.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_18 <- rbind(natl_table_18, table_18)
    
  } # end else 
  
  # Process Table 19, Disabled Population by Work Status ---
  if("19" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "19.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "19.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d == 58){
      pdf_text <- c(pdf_text[1:10], "TOTAL", pdf_text[11:204])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL DISABLED", rep("                      -               -                   -                -                   -                     -", 15),
                    "MALE", rep("                      -               -                   -                -                   -                     -", 15),
                    "FEMALE", rep("                      -               -                   -                -                   -                     -", 15),
                    "TRANSGENDER", rep("                      -               -                   -                -                   -                     -", 15))
      pdf_text[74] <- "RURAL"
    } 
    if(d == 116){
      pdf_text <- c(pdf_text[1:10], "TOTAL", pdf_text[11:206])
    }
    if(d == 133){
      pdf_text[9] <- "WASHUK DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    
    # in at least some cases, a blank row is throwing off the following
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
        
    if(d == 31){
      pdf_trimmed <- c(pdf_trimmed[1:130], "TRANSGENDER",
                       rep("AGE                -              -              -             -             -              -", 15),
                       pdf_trimmed[131:195])
    }
    
    # find the breaks for each administrative area
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL DISABLED", "MALE", "FEMALE", "TRANSGENDER")
    work_types <- c("TOTAL POPULATION", "WORKING", "SEEKING WORK", "STUDENT", "HOUSEKEEPING", "OTHER")
    age_groups <- c("10 AND ABOVE", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")

    table_19 <- tibble()
    for(k in 1:length(area_types)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()

      for(g in 1:length(gender_types)){
        gender_start <- grep(gender_types[g], area_extract)
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_start:gender_end]
        
        for(w in 1:length(work_types)){
          for(a in 1:length(age_groups)){
                row_out <- tibble(
                area_name = district_name,
                area_type = area_types[k],
                gender = gender_types[g],
                work_activity = work_types[w],
                age_group = age_groups[a],
                count = gender_extract[[a+1]][w+1],
                )
                
                total_out <- rbind(total_out, row_out)
          }
        }
      }

        table_19 <- rbind(table_19, total_out)  
    }
    
    # cleanup numerics
    table_19 <- cbind(table_19 %>% dplyr::select(1:5),
          table_19 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_19$count[is.na(table_19$count)] <- 0
    table_19$gender[table_19$gender == "ALL DISABLED"] <- "ALL SEXES"    
    if(d == 65){
      table_19$count[table_19$area_type == "URBAN"] <- NA
    }   
    
    
    table_19 <- table_19 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_19$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_19, paste0(dist_path, "table_19.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_19 <- rbind(natl_table_19, table_19)
    
  } # end else 
    
  # Process Table 20, Expatriate Population ---
    if("20" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "20.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "20.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 10){
      pdf_text[2] <- ""
    }
    if(d == 26){
      pdf_text[6] <- "FR LAKKI MARWAT"
    }
    if(d == 65){
      pdf_text[6] <- "KOHISTAN DISTRICT          9808      6466        3342     9808        6466        3342   -         -        -"
      pdf_text[7] <- "DASSU SUB-DIVISION            2130      1317         813     2130        1317         813   -         -        -" 
      pdf_text[8] <- "KANDIA SUB-DIVISION            3252      1979        1273     3252        1979        1273   -         -        -" 
      pdf_text[9] <- "PALAS SUB-DIVISION             1961      1164         797     1961        1164         797   -         -        -"
      pdf_text[10] <- "PATTAN SUB-DIVISION            2465      2006         459     2465        2006         459   -         -        -"
    }
    if(d == 88){
      pdf_text[7] <- "MUSAKHEL DISTRICT"
    }
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("BOTH SEXES", "MALE", "FEMALE") # note - this table omits transgender counts
    table_20 <- tibble()
    
    # break down each administrative area
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    area_cols <- list(c(2:4), c(5:7), c(8:10))
    
    for(a in 1:length(area_split)){
      total_out <- tibble()
      for(k in 1:length(area_types)){
        area_extract <- area_split[[a]][area_cols[[k]]]
        for(g in 1:length(gender_types)){
              row_out <- tibble(
              area_name = area_split[[a]][1],
              area_type = area_types[k],
              gender = gender_types[g],
              count = area_extract[g]
              )
              
              total_out <- rbind(total_out, row_out)
          }
      }

    table_20 <- rbind(table_20, total_out)  
    }

    # cleanup numerics
    table_20 <- cbind(table_20 %>% dplyr::select(1:3),
          table_20 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_20$count[is.na(table_20$count)] <- 0

    table_20 <- table_20 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_20$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_20, paste0(dist_path, "table_20.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_20 <- rbind(natl_table_20, table_20)
    
  } # end else 
        
  # Process Table 21, Population with CNIC ---
    if("21" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "21.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "21.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN",
                    "ALL SEXES", rep("-   -   -    -    -     -    -            -                -                   -                     -", 14)
      )
      pdf_text[25] <- "RURAL"
    } 
    if(d == 73){
      pdf_text[13] <- "LAYYAH DISTRICT"
    }
    if(d == 88){
      pdf_text[9] <- "MUSAKHEL DISTRICT"
    }
    pdf_text <- pdf_text[pdf_text != ""]
    pdf_text <- gsub("ABOVE", "ABOVE  ", pdf_text)
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # find the breaks for each administrative area
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    cnic_status <- c("TOTAL POPULATION", "CNIC OBTAINED", "CNIC NOT OBTAINED")
    age_groups <- c("18 AND ABOVE", "18 - 19", "20 - 24", "25 - 29",
                    "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 AND ABOVE")
    gender_cols <- list(c(2:4), c(5:7), c(8:10), c(11:13))
    table_21 <- tibble()
        
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()
      
      for(a in 1:length(age_groups)){
        for(g in 1:length(gender_types)){
          gender_extract <- area_extract[[a+1]][gender_cols[[g]]]
            for(c in 1:length(cnic_status)){
                  row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  gender = gender_types[g],
                  cnic_status = cnic_status[c],
                  age_group = age_groups[a],
                  count = gender_extract[c]
                  )
                  
              total_out <- rbind(total_out, row_out)
          }
        }
      }

    table_21 <- rbind(table_21, total_out)  
    }

    # cleanup numerics
    table_21 <- cbind(table_21 %>% dplyr::select(1:5),
          table_21 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_21$count[is.na(table_21$count)] <- 0
    if(d == 65){
      table_21$count[table_21$area_type == "URBAN"] <- NA
    }   
    
    table_21 <- table_21 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_21$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_21, paste0(dist_path, "table_21.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_21 <- rbind(natl_table_21, table_21)
    
  } # end else 
      
        
  # Process Table 22, Homeless Population Demographics --- # this is actually four tables squashed together - demographic data is unrelated to other columns
  if("22" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "22.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "22.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      
      pdf_text[15] <- "ALL AGES              15        2        10         3          0        15          0          15           13       2"
      pdf_text[26] <- "MALE"
      pdf_text[27] <- "ALL AGES                 15          2        10          3         0         15          0        15           13        2"
      pdf_text[38] <- "FEMALE"
      pdf_text[39] <- "ALL AGES               -          -          -         -          -          -         -          -           -        -"
      pdf_text[50] <- "TRANSGENDER"
      pdf_text[51] <- "ALL AGES               -          -          -         -          -          -         -          -           -        -"
      pdf_text[62] <- "RURAL"
      pdf_text[63] <- "ALL SEXES"
      pdf_text[64] <- "ALL AGES                   15          2        10          3         0         15          0        15           13        2"
      pdf_text[75] <- "MALE"
      pdf_text[76] <- "ALL AGES                  15          2        10          3         0         15          0        15           13        2"
      pdf_text[87] <- "FEMALE"
      pdf_text[88] <- "ALL AGES                -          -          -         -          -          -         -          -           -        -"
      pdf_text[99] <- "TRANSGENDER"
      pdf_text[100] <- "ALL AGES               -          -          -         -          -          -         -          -           -        -"
      pdf_text <- c(pdf_text,
                    "URBAN", "ALL SEXES", 
                    rep("-              -          -          -         -          -          -         -          -           -        -", 11),
                    "MALE", rep("-              -          -          -         -          -          -         -          -           -        -", 11),
                    "FEMALE", rep("-              -          -          -         -          -          -         -          -           -        -", 11),
                    "TRANSGENDER", rep("-              -          -          -         -          -          -         -          -           -        -", 11)
      )
      pdf_text <- c(pdf_text[1:14], "ALL SEXES", pdf_text[15:159])
      
    } 
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    age_types = c("ALL AGES", "BELOW 5 YEARS", "05-09", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 AND ABOVE")
    marital_types <- c("NEVER MARRIED", "MARRIED", "WIDOWED", "DIVORCED")
    religious_affiliations <- c("MUSLIM", "NON-MUSLIM")
    work_types <- c("WORKING", "OTHER")

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_22_marital <- tibble()
    table_22_religion <- tibble()
    table_22_literacy <- tibble()
    table_22_work <- tibble()
    
    # break down each administrative area
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
  
      all_start <- as.integer(grep("ALL SEXES", area_extract))
      male_start <- as.integer(grep("MALE", area_extract)[[1]][1])
      fem_start <- as.integer(grep("FEMALE", area_extract))
      trans_start <- as.integer(grep("TRANSGENDER", area_extract))
      gender_starts <- c(all_start, male_start, fem_start, trans_start)
      total_out <- tibble()
      for(g in 1:length(gender_starts)){
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_starts[g]:gender_end]
        for(m in 1:length(marital_types)){
          for(a in 1:length(age_types)){
                    row_out <- tibble(
                    area_name = district_name,
                    area_type = area_types[k],
                    gender = gender_types[g],
                    age_group = age_types[a],
                    total_population = gender_extract[[a+1]][2],
                    marital_status = marital_types[m],
                    count = gender_extract[[a+1]][m+2],
                    )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
    }
      
      table_22_marital <- rbind(table_22_marital, total_out)  
    }

    # break down each administrative area
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
  
      all_start <- as.integer(grep("ALL SEXES", area_extract))
      male_start <- as.integer(grep("MALE", area_extract)[[1]][1])
      fem_start <- as.integer(grep("FEMALE", area_extract))
      trans_start <- as.integer(grep("TRANSGENDER", area_extract))
      gender_starts <- c(all_start, male_start, fem_start, trans_start)
      total_out <- tibble()
      for(g in 1:length(gender_starts)){
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_starts[g]:gender_end]
        for(r in 1:length(religious_affiliations)){
          for(a in 1:length(age_types)){
                    row_out <- tibble(
                    area_name = district_name,
                    area_type = area_types[k],
                    gender = gender_types[g],
                    age_group = age_types[a],
                    total_population = gender_extract[[a+1]][2],
                    religious_affiliation = religious_affiliations[r],
                    count = gender_extract[[a+1]][r+6],
                    )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
    }
      
      table_22_religion <- rbind(table_22_religion, total_out)  
    }

    # break down each administrative area
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
  
      all_start <- as.integer(grep("ALL SEXES", area_extract))
      male_start <- as.integer(grep("MALE", area_extract)[[1]][1])
      fem_start <- as.integer(grep("FEMALE", area_extract))
      trans_start <- as.integer(grep("TRANSGENDER", area_extract))
      gender_starts <- c(all_start, male_start, fem_start, trans_start)
      total_out <- tibble()
      for(g in 1:length(gender_starts)){
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_starts[g]:gender_end]
          for(a in 1:length(age_types)){
                    row_out <- tibble(
                    area_name = district_name,
                    area_type = area_types[k],
                    gender = gender_types[g],
                    age_group = age_types[a],
                    total_population = gender_extract[[a+1]][2],
                    literate_over_ten_count = gender_extract[[a+1]][9],
                    )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
      
      table_22_literacy <- rbind(table_22_literacy, total_out)  
    }

    # break down each administrative area
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
  
      all_start <- as.integer(grep("ALL SEXES", area_extract))
      male_start <- as.integer(grep("MALE", area_extract)[[1]][1])
      fem_start <- as.integer(grep("FEMALE", area_extract))
      trans_start <- as.integer(grep("TRANSGENDER", area_extract))
      gender_starts <- c(all_start, male_start, fem_start, trans_start)
      total_out <- tibble()
      for(g in 1:length(gender_starts)){
        gender_end <- ifelse(g != 4, (as.integer(grep(gender_types[g+1], area_extract)-1)), length(area_extract))
        gender_extract <- area_extract[gender_starts[g]:gender_end]
        for(w in 1:length(work_types)){
          for(a in 1:length(age_types)){
                    row_out <- tibble(
                    area_name = district_name,
                    area_type = area_types[k],
                    gender = gender_types[g],
                    age_group = age_types[a],
                    total_population = gender_extract[[a+1]][2],
                    work_status = work_types[w],
                    count = gender_extract[[a+1]][w+9],
                    )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
    }
      
      table_22_work <- rbind(table_22_work, total_out)  
    }        

    # cleanup numerics
    table_22_marital <- cbind(
          table_22_marital %>% dplyr::select(1:4),
          table_22_marital %>% dplyr::select(5) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_22_marital %>% dplyr::select(6),
          table_22_marital %>% dplyr::select(7) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)  
    )
    
    table_22_marital$count[is.na(table_22_marital$count)] <- 0
    table_22_marital$total_population[is.na(table_22_marital$total_population)] <- 0

    table_22_work <- cbind(
          table_22_work %>% dplyr::select(1:4),
          table_22_work %>% dplyr::select(5) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_22_work %>% dplyr::select(6),
          table_22_work %>% dplyr::select(7) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)  
    )

    table_22_work$count[is.na(table_22_work$count)] <- 0
    table_22_work$total_population[is.na(table_22_work$total_population)] <- 0
    
    table_22_religion <- cbind(
          table_22_religion %>% dplyr::select(1:4),
          table_22_religion %>% dplyr::select(5) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_22_religion %>% dplyr::select(6),
          table_22_religion %>% dplyr::select(7) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)  
    )    

    table_22_religion$count[is.na(table_22_religion$count)] <- 0
    table_22_religion$total_population[is.na(table_22_religion$total_population)] <- 0    
    
    table_22_literacy<- cbind(
          table_22_literacy %>% dplyr::select(1:4),
          table_22_literacy %>% dplyr::select(5:6) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)  
    )            

    table_22_literacy$literate_over_ten_count[is.na(table_22_literacy$literate_over_ten_count)] <- 0
    table_22_literacy$total_population[is.na(table_22_literacy$total_population)] <- 0    
    
    table_22_literacy <- table_22_literacy %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())

    table_22_marital <- table_22_marital %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    table_22_religion <- table_22_religion %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())

    table_22_work <- table_22_work %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
            
    prov_path <- paste0("./processed_forms/", unique(table_22_literacy$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_22_literacy, paste0(dist_path, "table_22_literacy.csv", na = ""))
    write_csv(table_22_marital, paste0(dist_path, "table_22_marital.csv", na = ""))
    write_csv(table_22_religion, paste0(dist_path, "table_22_religion.csv", na = ""))
    write_csv(table_22_work, paste0(dist_path, "table_22_work.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_22_literacy <- rbind(natl_table_22_literacy, table_22_literacy)
    natl_table_22_marital <- rbind(natl_table_22_marital, table_22_marital)
    natl_table_22_religion <- rbind(natl_table_22_religion, table_22_religion)
    natl_table_22_work <- rbind(natl_table_22_work, table_22_work)
    
  } # end else 
  
  # Process Table 23, Detailed Population of Rural Localities ---
  if(("23" %in% missing_tables)|d == 65){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "23.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "23.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub(", ", "", pdf_text)
    if(d == 4){
      pdf_text[392] <- "ADDL. TANDO GHULAM ALI TC           7,789     4,013     3,775    1   32.85   46.61   18.53        -      469      180   -      364      53   -      64     5   -     6,383    1,406     5,011     3,672      358    1,840     2,515    11064"
    }
    if(d == 5){
      pdf_text[985] <- "CHORRI WALA TOBA          -------           170        92        78          -    44.80    61.54     26.67      0.00       11      10       -       16        1       -        1         -       -       170         -       125         93         8        47         73        0"
      pdf_text[1687] <- ""
    }
    if(d == 8){
      pdf_text[247] <- "SUR KAMAND KHANZAD MUGHAL KHEL        0000193    1,311        679       632        -    44.43    65.99     22.33         -       110       48        -       122       21        -       37        16          -     1,311         -       871         629       62       274        530       435"
      pdf_text[248] <- ""
    }
    if(d == 12){
      pdf_text[24] <- "NAWAKALAY (NAWAKALAY ASHIZAI)     0000046     3,134         1,525     1,609           -      42.68        68.63      19.15           -        430       121         -      114           13             -       22          2            -     3,134               -     2,111       1,364        156        465       1,117      1455"
      pdf_text[25] <- ""
      pdf_text[222] <- "ASHGHER TARINAN (ASGHAR TARIN)    0000119    1,912           955       957           -      45.36        63.61      26.66           -        235       118         -      135           16             -       18          2            -     1,912               -     1,314         873         97        357         756      1281"
      pdf_text[223] <- ""
    }
    if(d == 21){
      pdf_text[18] <- "DERA ISMAIL KHAN DISTRICT     1,264,870    649,892   614,919       59    36.43     50.73     21.46    13.56   101,734   41,251     4    61,531   18,421     2   16,201      5,912        -     1,264,507       363   838,619    600,184    63,841   234,423   506,137   1713321"
      pdf_text[271] <- "TAKWARA NALLAH YAQUB ZAI PC       57              31        26        -     8.33     14.29      0.00        -         2        -     -         1        -     -        -          -        -        57         -        36         23         3         -        18     11928"
    }
    if(d == 26){
      pdf_text[16] <- "TRIBAL AREA ADJ. LAKKI MARWAT DISTRICT   26,394    13,685   12,709          -   20.37   35.34    4.57            -   1,831      179                    -     696          58             -     129          16                 -   26,362          32      16,857      11,499     1,119         4,864            8,412           -"
    }
    if(d == 29){
      pdf_text[283] <- "BELO BOZDAR          -------           1,193       664       528     1    48.76     69.39     25.33          -       179      77        -       65         9      -       19         1        -     1,156        37       804        592        55        26       423         0"
      pdf_text[284] <- "PIPRI          -------           3,030     1,578     1,452     -    40.36     68.25     11.39          -       321      59        -      230        12      -       35         1        -     2,761       269     2,022      1,380       158       132     1,108         0"
    }
    if(d == 30){
      pdf_text[149] <- "BOTALA SARDAR JHANDA SINGH PC               7,659    3,766     3,893          -    73.22    77.34      69.30         -    1,226     1,032         -      597       632         -      83       146         -     7,437      222      5,791       4,338     569    1,622    3,490     2,139"
      pdf_text[150] <- ""
      pdf_text[346] <- "ALLAHABAD(NANGALDUNA SINGH)PC                       6,611     3,409     3,202          -    59.26    59.45      59.07         -       810       753         -      234       303         -      38        28         -     6,582       29      4,725       3,393      405      405     2,401     5,202"
      pdf_text[347] <- ""
      pdf_text[502] <- "HYDERABAD (THATTA GULAB SINGH)         0000527     1,680      846       834         -    71.30    74.76      67.97         -      292       231         -     103       106          -      15        34         -      1,617        63      1,251          896    132      416      712      1,837"
      pdf_text[503] <- ""
    }
    if(d == 33){
      pdf_text[18] <- "CHAK CHATHA QH           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[19] <- "BORIANWALA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[21] <- "CHAK CHATHA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[26] <- "GHANIAN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[29] <- "HARDO BHAUN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[31] <- "JURIAN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[34] <- "KASSOKE PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[36] <- "KOT HASSAN KHAN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[40] <- "KURIALA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[43] <- "LAWERE PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[47] <- "SIOKE PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[51] <- "WACHHOKI PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[54] <- "WINNI PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[57] <- "HAFIZABAD-I QH           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[58] <- "HAFIZABAD PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[61] <- "KALIANWALA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[64] <- "MADHARIAN WALA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[68] <- "MAMNA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[75] <- "QILA RAM KAUR PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[78] <- "HAFIZABAD-II QH           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[79] <- "GARHI GHOUS PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[82] <- "MANGAT NICHE PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[85] <- "MURADIAN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[90] <- "SAGAR KALAN PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[93] <- "SAGAR KHURD PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[97] <- "KALEKEY QH           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[98] <- "BANGAR PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[103] <- "BURJ DARA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[107] <- "GIGE PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[111] <- "JALALIANA PC           --   --   --   --   --   --   --    --   --   --    --   --    --    --    --    --   --   --   --   --   --   --   --   --   --"
      pdf_text[434] <- "BADAR ALI PC         15,982         8,225     7,756          1    48.83       60.48      36.64           -    2,150     1,334           -      790       303          -      140         87         -   15,964             18    11,562       8,743      1,183       3,883      6,511     10465"
    }
    if(d == 35){
      pdf_text[132] <- "DHERI LABAN BANDI(CHANG BANDI)     0000191      8,009     4,233     3,776        -    75.76    83.68     66.78        -     1,319     974       -     1,023       612      -      190       157         -   8,009         -     6,072      4,817       575     1,610     4,341     2509"
      pdf_text[133:134] <- ""
    }
    if(d == 37){
      pdf_text[30] <- "MIANO          -------           9,633     5,031     4,601           1    38.78       47.95      28.66        0.00       825      496       -     359             109             -      96          25            -     8,604       1,029      6,777       5,022        465      1,669       3,534       7,962"

    }
    if(d == 38){
      pdf_text[15] <- "ISLAMABAD DISTRICT"
    }
    if(d == 42){
      pdf_text[76] <- "(KALAN SHADI HAR)S.BUTTA KALAN    0000097     966      517       449       -    17.54     26.10      7.05          -       41      11       -        26        3       -       1        -        -       966         -      536        412        41        17       240     2195"
      pdf_text[77:78] <- ""
    }
    if(d == 46){
      pdf_text[222] <- "KAREZ QAMBARANI(KHASOON DOON)       0000009        378       187       191         -    51.69    62.39     41.18          -       26      20       -       29       23       -       12         6       -     378         -      236        171        20         1       152      1128"
      pdf_text[223:224] <- ""
      pdf_text[464] <- "CHOOTANIK MUHAMMAD SHAHISHARQI       0000041          1,274      784       490         -    54.09    69.02     27.63          -       43      20       -      264       10       -      15         6       -   1,274        -      843        485        39        37       339      2587"
      pdf_text[465:467] <- ""
      pdf_text[634] <- "REHMAT BOOZ KORACHO JIO       0000150       1,318      668       650         -    65.61    70.07     60.67          -      158      87       -       36        8       -        9         4       -   1,318         -      820        526        44        80       344       975"
      pdf_text[635:636] <- ""
    }
    if(d == 51){
      pdf_text[24] <- "HUB          -------           2,306      1,291     1,015           -      40.97        48.07      31.58           -        269       127           -        97        44           -        6          1            -     2,299               7     1,545       1,133         78         672        763         -"
      pdf_text[26] <- "MAIGARHI          -------           5,134      2,688     2,446           -      33.70        37.47      29.35           -        398       280           -       119        77           -       15          5            -     5,096              38     3,516       2,600        194       1,244      1,785         -"
    }
    if(d == 54){
      pdf_text[18] <- "KASUR DISTRICT    2564101   1328622   1235368   111   57.41   65.07   49.12   27.93   710403   241380   14   149397   86928   8   91893   1846710   1360869   150743   544081   1097495   981573   150743   544081   1097495   981573"
      pdf_text[281] <- "KASUR TEHSIL     845004   438789    406176   39   56.96    64.33   48.94   17.95   296101   78686   2   51,224   29,306   3  9,823   6,871  -   805,684   39320   611880   452109   50865   176197   359524   355755"
      pdf_text[367] <-  "BURJ RAJOKE                        0000298          8         8          -            -   100.00     -    -               -       1        -              -        6      -                -       1        -            -         8            -          8            8            -             8            8           67"
      pdf_text[731] <- "PATTOKI TEHSIL   753853   391493   362322   38   57.6   65.06   49.47   44.74   284963   72031   8   43838   26639   3   8262   6993   0   729181   24672   544982   403191   42268   163983   330335   223511"
    }
    if(d == 56){
      pdf_text[249] <- "GANNO KALHORO          -------           6,050     3,044     3,006        -    35.76    49.93     21.22         -       522     315       -       336        53      -      119        16         -     6,050         -     4,164      2,971       271     1,476     2,312     2009"
      pdf_text[250] <- "GARHI          -------           3,853     1,958     1,895        -    60.01    71.62     47.91         -       347     318       -       439       212      -      162        69         -     3,648       205     2,783      2,035       197       959     1,747     1570"
      pdf_text[251] <- "MORI          -------           6,490     3,366     3,124        -    59.82    69.85     49.03         -       612     476       -       558       382      -      361       139         -     6,238       252     4,711      3,521       311     1,729     2,970     1213"
    }
    if(d == 57){
      pdf_text[208] <- "CHAH SULTAN WALA          -------           41       21       20           -    13.33       23.53          -           -        2        -            -         1         -             -       -           -            -        41              -        30          22          3         14          15         0"
    }
    if(d == 63){
      pdf_text[182] <- "BANDATGOWAL, ISMAIL ZAI    0000072       7,735      4,135      3,600           -         9.27      14.16       3.37               -        154         14           -        74                5            -       19              1            -      7,731                  4     4,381       3,085        390       1,474      2,115           3028"
    }
    if(d == 64){
      pdf_text[31] <- "JANGLE MIR ASGHAR MELA PC         4,767       2,136     2,630          1    61.20     85.36     43.76          -      532      578     -       628      245     -       81        31        -     4,767         -     3,549       2,698      419       451     2,348       8764"
      pdf_text[83] <- "MIAN KHEL MIR AHMED KHEL PC       8,713     4,509     4,203          1    67.15     81.64     51.67     100.00    1,022      680     1     1,089      550     -      436       218        -     8,709         4     6,329       4,788      494     1,387     4,031       1139"
      pdf_text[85] <- "MIAN KHEL MIR AHMED KHEL       0000043       6,966     3,667     3,299          -    66.86     81.44     50.77          -      763      508     -       906      441     -      407       183        -     6,962         4     5,061       3,816      358     1,186     3,252        720"
      pdf_text[86:87] <- ""
      pdf_text[91] <- "BANDA MIRZA HASSAN KHEL    0000019      2,392     1,159     1,233          -    53.61     78.91     30.89          -      407      183     -       116        5     -        5         -        -     2,392         -     1,634       1,059      115       135     1,034        268"
      pdf_text[92:93] <- ""
    }
    if(d == 74){
      pdf_text[55] <- "CHAK SARDAR KHAN (SARWAR) KHAN     0000008      1,076           521       555        -     42.80       47.85      37.74           -        111         95            -       58        32              -        7          4            -     1,076           -       743          542         44        265         442       454"
      pdf_text[56] <- ""
      pdf_text[57] <- ""
    }
    if(d == 76){
      pdf_text[948] <- "SHAGOKASS          -------           1,891      937       954         -    64.37    82.76     47.28         -      228     150       -      150       59       -       58        16       -   1,891         -    1,277        882        91       332       674           -"
    }
    if(d == 81){
      pdf_text[64] <- "KAMAL ZAI (HOTI) JANUBI QH     366,256    185,304   180,952         -    53.47     69.57     37.22          -    41,161    22,925   -    27,774   10,733      -    9,120     4,601        - 366,048         208 257,492       185,638    20,123    78,717   155,078      58893"
      pdf_text[71] <- "CHAK SHAHBAZ GARHI PC      34,525          17,408    17,117         -    47.61     64.99     30.18          -     4,003     2,069   -     2,287      694      -      488       170        -    34,525         -    23,683      16,584     1,657     6,144    13,688       3447"
      pdf_text[112] <- "KAMAL ZAI (MARDAN) SHUMALI QH      482,370       242,704   239,664         2    51.90     68.18     35.83          -    54,049    31,836   -    37,854   13,844      -   10,017     3,703        - 482,185         185 338,820       241,785    26,810    92,936   202,469      67832"
      pdf_text[113] <- ""
    }
    if(d == 86){
      pdf_text[37] <- "SHAH BAIG (KAMAL HALIM ZAI)          -------           4,087          2,109                1,978              -           35.58          58.15             13.19                 -             555              25                 -              140            241                 -             22                 -                 -         4,087               -                 2,673             1,921            190                783             1,308                   -"
    }
    if(d == 88){
      pdf_text[16] <- "MUSAKHEL DISTRICT"
    }
    if(d == 93){
      pdf_text[83] <- "ADREES SATTA KAMAL BAT          -------           588       314        274            -     52.00         62.26      40.43           -         71         72          -        47         2        -        10           -          -         588          -         400         282         29          85         169        -"
      pdf_text[89] <- "KHER MUHAMMAD DASTI DABHRO          -------           1,078       506        572            -     89.58         92.63      87.03           -        109        258          -       165        96        -        47           6          -       1,078          -         777         555         48          60         471        -"
      pdf_text[91] <- "MATO SAYYAL CHATO BATI          -------           873       453        420            -     60.49         73.27      46.80           -        119         96          -        78        25        -        21           7          -         873          -         615         445         45          27         393        -"
      pdf_text[92] <- "YAQOOB SAYYAL          -------           92        50         42            -     47.27         67.86      25.93           -          7          4          -         7         -        -         -           -          -          92          -          55          38          3          16          31        -"
      pdf_text[162] <- "LANGHAR G          -------           3,306     1,703      1,603            -     49.27         59.05      39.04           -        334        263          -       175        79        -        44           9          -       2,973        333       2,184       1,533        142         833       1,211     2,740"
      pdf_text[163] <- "MEHRABPUR          -------           1,686       870        816            -     58.25         67.81      48.09           -        205        191          -       177        70        -        40          19          -       1,642         44       1,243         939        107         448         771     2,431"
    }
    if(d == 95){
      pdf_text[53] <- "JABBI PC            6,960          3,523     3,437          -    51.73     68.79     35.03       0.00      905      493        -      504      181       -         57       20      -     6,960         -     4,858     3,629     487      1,485     3,010      3843"
      pdf_text[54] <- ""
    }
    if(d == 101){
      pdf_text[16] <- "PESHAWAR DISTRICT                2,297,375    1,172,995     1,124,167           213    45.58   61.94   28.72         26.76   233,139   107,003       20    144,109    47,232       18   40,163      12,302      4    2,293,546      3,829   1,563,760    #######     99,231   396,858    856,950   270551"
      pdf_text[17] <- "PESHAWAR TEHSIL                2,297,375    1,172,995     1,124,167           213    45.58   61.94   28.72         26.76   233,139   107,003       20    144,109    47,232       18   40,163      12,302      4    2,293,546      3,829   1,563,760    #######     99,231   396,858    856,950   270551"
      pdf_text[18] <- ""
      pdf_text[19] <- ""
    }
    if(d == 103){
      pdf_text[38] <- "KHUSHABA KAREZ SARIAB          -------           5,947     3,190      2,756       1     37.88       53.24     20.14    100.00        515       139         -      290        103       1       20           1       -        5,947           -      3,775       2,492      208      1,165        975         0"
    }
    if(d == 106){
      pdf_text[31] <- "NAGIAL UMER KHAN          -------           1,875       918       957        -      77.26       84.29      70.74           -       322     257        -     206            188           -       41         60        -     1,864           11     1,482     1,207           191        547     1,086          855"
      pdf_text[100] <- "RATALA PC    4945   2358   2587    0    71.92   86.81   58.58  0  941  684  0   494   340   0  52   78   0   4945  0   3893   3054   479   701   2681   3094"
    }
    if(d == 114){
      pdf_text[228] <- "BELO BAGERJI          -------           1,420       754       665           1    20.71       36.72       2.35      100.00         80         6       -       69              4          -       8           -         -     1,215         205     1,091         674         79         22         566         0"
    }
    if(d == 118){
      pdf_text[371] <- "SHAMAN KHEL/CHAR KHEL SECTION    6,347            3,179    3,168           -      43.22        67.74      19.16           -      778       205           -       310         59            -       64         15            -     6,347              -     3,938       2,801        346         455      2,529              -"
      pdf_text[425] <- "NEKZAN/URMAR/KIKARAI KHEL SEC.    14,327          7,600    6,727           -      23.13        37.49       6.77           -    1,194       162           -       485         42            -       85         17            -    14,327              -     9,504       6,738        729       2,287      5,834              -"
      pdf_text[450] <- "DOUTHAN TOGI KHEL WAZIR TRIBE      83,775          44,320   39,446           9      36.54        53.57      16.89       11.11    7,487     2,207           -     4,973        549            1    1,019         96            -    83,641            134    53,350      36,742      3,519      10,090     21,851              -"
      pdf_text[478] <- "MUGHAL KHEL/HETHI KHEL SECTION     2,737           1,391    1,346           -      16.26        31.56       1.27           -      105         3           -        96          1            -       33          2            -     2,737              -     1,556       1,136        103       1,146        494              -"
      pdf_text[534] <- "KAKA KHEL/UTHMAN KHEL SECTION      1,317              684      632          1      32.42        42.58      20.73      100.00       85        47           1        51        12             -       9              1          -    1,317                  -       876         610         75         269        378              -"
      pdf_text[543] <- "UTHMAN KHEL/DESSI KHEL SECTION     1,646             895      751          -      30.54        42.66      14.08           -      128        26           -        84        14             -       7              2          -    1,646                  -     1,123         728         72         374        406              -"
      pdf_text[556] <- "SHAKH BAZID/GHANI KHEL SECTION     1,447             742      705          -      41.69        58.15      24.36           -      158        75           -        86        24             -      12              -          -    1,447                  -       885         604         49         150        501              -"
    }
    if(d == 119){
      pdf_text <- pdf_text[1:463]
    }

    if(d %in% c(27, 61)){
      pdf_text[2] <- ""
    }
    if(d %in% c(48, 49, 50, 67)){
      pdf_text <- gsub(district_name, "", pdf_text)
    }

    if(!(any(grepl(district_name, pdf_text)))){
      
      table_23 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          sublvl_01 = NA, sublvl_02 = NA, sublvl_03 = NA, summary_row = NA, area_name = NA, hadbast_deh_number = NA, pop_all = NA, pop_male = NA, pop_fem = NA, pop_trans = NA,
          lit_all = NA, lit_male = NA, lit_fem = NA, lit_trans = NA, primary_male = NA, primary_fem = NA, primary_trans = NA, matric_male = NA, matric_fem = NA, matric_trans = NA,
          degree_male = NA, degree_fem = NA, degree_trans = NA, muslim = NA, non_muslim = NA, age_10_up = NA, age_18_up = NA, age_60_up = NA, working_10_up = NA, cnic_18_up = NA,
          area_acre = NA
        )
      
      natl_table_23 <- rbind(natl_table_23, table_23)
    } else {
    
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    
    pdf_trimmed[grep("SELECTED POPULATION STATISTICS", pdf_trimmed)] <- ""
    more_headers_starts <- grep("POPULATION CHARACTERISTICS", pdf_trimmed)
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}26\\s{6,}27", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*25\\s*26\\s*27", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}


    if(d == 21){
      pdf_trimmed[252] <- "TAKWARA NALLAH HUSSAIN ZAI                   0000002   4,434      2,283     2,150        1    26.04     41.14     10.04   100.00       312       39     1       156       10     -       15          2        -     4,434         -     2,869      2,082       259       230     1,667     19164"
      pdf_trimmed[253] <- ""
      pdf_trimmed[255] <- "TAKWARA NALLAH YAQUB ZAI          0000003     57        31        26        -     8.33     14.29         -        -         2        -     -         1        -     -        -          -        -        57         -        36         23         3         -        18     11928"
      pdf_trimmed[256:257] <- ""
    }
    
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # on first pass, in some cases cells are not fully splitting, so need to clean up first
    pdf_correct <- gsub("N0 ", "NO ", pdf_trimmed)
    pdf_correct <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_correct, perl = T)
    #pdf_correct <- gsub("(?<=[[:alpha:]])\\s(?=\\d)", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("\\)(?=\\d)", ")   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=\\s)-(?=\\s)", "  -  ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=(NO.(\\s)\\d)(?=\\d{7}))", "    ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?=\\d{7})", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=\\d{7})", "   ", pdf_correct, perl = T)
    #pdf_correct <- gsub("CHAK NO   ", "CHAK NO ", pdf_correct, perl = T)
    
    if(d == 31){
      pdf_correct[410] <- "KEERAN-WALA KHAS       286/289    5,073    2,444      2,629             -   68.18   73.76   63.21                          -        750      635               -       282      388              -       53      68               -      5,073                 -          3,774         2,846         425         688        2,350      2142"
    }

    pdf_split <- strsplit(trimws(pdf_correct), "\\s{2,}")
    
    # create a dataframe, identify area aggregate rows by missing hadbast / deh number field 
    # (note that Sindh does not use these and many other areas also have null codes; code sequences are not consistent across forms)
    
    total_out <- tibble()
    line_break <- F
    partial_break <- F    
    for(x in 1:length(pdf_split)){
      
      row_extract <- pdf_split[[x]]
      if(length(row_extract) > 1){
        if(length(row_extract) > 2){
        
          if(line_break == T){
            area_correct <- pdf_split[[x-1]]
            row_extract <- c(area_correct, row_extract)
            line_break <- F
          } else {
            if(partial_break == T){
              area_correct <- paste0(pdf_split[[x-1]][1], " ", row_extract[[1]][1])
              correct_deh <- pdf_split[[x-1]][2]
              row_extract <- c(area_correct, correct_deh, row_extract[2:length(row_extract)])
              partial_break <- F
            }
          }
    
      row_out <- tibble(
        area_name = row_extract[1],
        summary_row = ifelse(area_name %in% administrative_areas, ifelse(length(row_extract) < 27, "YES", "NO"), "NO"),
        hadbast_deh_number = ifelse(summary_row == "NO", row_extract[2], NA),
        pop_all = ifelse(summary_row == "NO", 
                 row_extract[3],
                 row_extract[2]),
        pop_male = ifelse(summary_row == "NO", 
                 row_extract[4],
                 row_extract[3]),
        pop_fem = ifelse(summary_row == "NO", 
                 row_extract[5],
                 row_extract[4]),
        pop_trans = ifelse(summary_row == "NO",
                 row_extract[6],
                 row_extract[5]),
        lit_all = ifelse(summary_row == "NO", 
                 row_extract[7],
                 row_extract[6]),
        lit_male = ifelse(summary_row == "NO", 
                 row_extract[8],
                 row_extract[7]),
        lit_fem = ifelse(summary_row == "NO", 
                 row_extract[9],
                 row_extract[8]),
        lit_trans = ifelse(summary_row == "NO", 
                 row_extract[10],
                 row_extract[9]),
        primary_male = ifelse(summary_row == "NO", 
                 row_extract[11],
                 row_extract[10]),
        primary_fem = ifelse(summary_row == "NO", 
                 row_extract[12],
                 row_extract[11]),
        primary_trans = ifelse(summary_row == "NO", 
                 row_extract[13],
                 row_extract[12]),
        matric_male = ifelse(summary_row == "NO", 
                 row_extract[14],
                 row_extract[13]),
        matric_fem = ifelse(summary_row == "NO", 
                 row_extract[15],
                 row_extract[14]),
        matric_trans = ifelse(summary_row == "NO", 
                 row_extract[16],
                 row_extract[15]),
        degree_male = ifelse(summary_row == "NO", 
                 row_extract[17],
                 row_extract[16]),
        degree_fem = ifelse(summary_row == "NO", 
                 row_extract[18],
                 row_extract[17]),
        degree_trans = ifelse(summary_row == "NO", 
                 row_extract[19],
                 row_extract[18]),
        muslim = ifelse(summary_row == "NO", 
                 row_extract[20],
                 row_extract[19]),
        non_muslim = ifelse(summary_row == "NO", 
                 row_extract[21],
                 row_extract[20]),
        age_10_up = ifelse(summary_row == "NO", 
                 row_extract[22],
                 row_extract[21]),
        age_18_up = ifelse(summary_row == "NO", 
                 row_extract[23],
                 row_extract[22]),
        age_60_up = ifelse(summary_row == "NO", 
                 row_extract[24],
                 row_extract[23]),
        working_10_up = ifelse(summary_row == "NO", 
                 row_extract[25],
                 row_extract[24]),
        cnic_18_up = ifelse(summary_row == "NO", 
                 row_extract[26],
                 row_extract[25]),
        area_acre = ifelse(summary_row == "NO", 
                 row_extract[27],
                 row_extract[26])
      )
      
      total_out <- rbind(total_out, row_out)
        } else {partial_break <- T}
          } else {line_break <- T}      
    }
    
    #figure out subdivision parent-child relationships by relative positions
    subdivisions <- total_out %>% mutate(id = row_number()) %>%
      filter(summary_row == "YES")
    #
    subdivisions <- subdivisions %>%
        mutate(child_03 = ifelse(id - (lead(id, n = 1)-1) != 0, "child", "parent"))
    subdivisions$child_03[is.na(subdivisions$child_03)] <- "child"
    #
    temp <- subdivisions %>%
      filter(child_03 == "parent") %>%
      mutate(child_02 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
    temp$child_02[is.na(temp$child_02)] <- "child"
    subdivisions <- left_join(subdivisions, temp)
    #
    temp <- subdivisions %>%
      filter(child_03 == "parent" & child_02 == "parent") %>%
      mutate(child_01 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
    temp$child_01[is.na(temp$child_01)] <- "child"
    subdivisions <- left_join(subdivisions, temp)
    #
    total_out$child_04 <- ifelse(total_out$summary_row == "YES", "parent", "child")
    #
    table_23 <- left_join(total_out, subdivisions)
    #
    table_23$child_01 <- ifelse(table_23$child_01 == "parent", table_23$area_name, NA)
    table_23$child_02 <- ifelse(table_23$child_02 == "parent" & is.na(table_23$child_01), table_23$area_name, NA)
    table_23$child_03 <- ifelse(table_23$child_03 == "parent" & is.na(table_23$child_01) & is.na(table_23$child_02), table_23$area_name, NA)
    table_23$child_04 <- ifelse(table_23$child_04 == "parent" & is.na(table_23$child_01) & is.na(table_23$child_02) & is.na(table_23$child_03), table_23$area_name, NA)
    #
    # fill forward missing names
    table_23 <- table_23 %>% fill(c(child_01, child_02, child_03, child_04)) %>%
      rename(
        district_name = child_01,
        sublvl_01 = child_02,
        sublvl_02 = child_03,
        sublvl_03 = child_04
      )
    
    table_23 <- table_23 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    sublvl_01, sublvl_02, sublvl_03, summary_row, everything()) %>%
      dplyr::select(-id)

    # correct over-fills
    table_23$sublvl_02[table_23$sublvl_01 == table_23$area_name] <- NA
    table_23$sublvl_03[table_23$sublvl_01 == table_23$area_name] <- NA
    table_23$sublvl_03[table_23$sublvl_02 == table_23$area_name] <- NA

    if(d == 32){
      table_23$sublvl_03[table_23$area_name == "SUNTSER" & table_23$summary_row == "NO"] <- "SUNTSER UC"
    }        
    if(d == 58){
      table_23$sublvl_03[table_23$area_name == "TOHMULK" & table_23$summary_row == "NO"] <- "TOHMULK UC"
    }    
    if(d == 100){
      table_23$sublvl_03[table_23$area_name == "GOWARGO" & table_23$summary_row == "NO"] <- "DASHT E SHAHBAZ UC"
    }
    if(d == 133){
      table_23$sublvl_03[table_23$area_name == "BESIMA" & table_23$summary_row == "NO"] <- "BESIMA UC"
      table_23$sublvl_03[table_23$area_name == "NAG" & table_23$summary_row == "NO"] <- "NAG UC"      
    }
    
    # cleanup numerics
    table_23 <- cbind(
          table_23 %>% dplyr::select(1:9),
          table_23 %>% dplyr::select(10:34) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_23[is.na(table_23)] <- 0
    table_23$hadbast_deh_number[table_23$summary_row == "YES"] <- NA
    table_23$sublvl_01[table_23$sublvl_01 == 0] <- NA
    table_23$sublvl_02[table_23$sublvl_02 == 0] <- NA
    table_23$sublvl_03[table_23$sublvl_03 == 0] <- NA
    
    prov_path <- paste0("./processed_forms/", unique(table_23$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_23, paste0(dist_path, "table_23.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_23 <- rbind(natl_table_23, table_23)
    
    }
    } # end else 
  
  # Process Table 24, Housing Status of Rural Localities ---
    if("24" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "24.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "24.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)  
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d %in% c(48, 49, 50, 67)){
      pdf_text <- gsub(district_name, "", pdf_text)
    }
    if(d == 21){
      pdf_text[12] <- "DERA ISMAIL KHAN DISTRICT"
    }
    if(d == 38){
      pdf_text[10] <- "ISLAMABAD DISTRICT"
    }
    if(d == 51){
      pdf_text[13] <- "KARACHI WEST DISTRICT"
      pdf_text[22] <- "HUB          -------           325         303          7         15         314          296           1        317      315       310           6.95"
      pdf_text[24] <- "MAIGARHI          -------           893         728         38        127         302          558          36        823      845       718           5.70"
    }
    if(d == 56){
      pdf_text[10] <- "KHAIRPUR DISTRICT"
    }
    if(d == 86){
      pdf_text[34] <- "SHAH BAIG (KAMAL HALIM ZAI)          -------           414            12               41            361             289              392           3             411              388              67            9.67"
    }
    if(d == 93){
      pdf_text[81] <- "ADREES SATTA KAMAL BAT)          -------           93           49         12        32         91          93          1           93             91            92           6.19"
      pdf_text[87] <- "KHER MUHAMMAD DASTI DABHRO)          -------           193          159         13        21        124         193          -          101            110           120           5.59"
      pdf_text[89] <- "MATO SAYYAL CHATO BATI)          -------           135           94         18        23        129         133          -           91            110           128           6.47"
      pdf_text[90] <- "YAQOOB SAYYAL)          -------           19            5          4        10         19          15          -           16             17            15           4.84"
    }
    if(d == 119){
      pdf_text[12] <- "SUJAWAL DISTRICT"
    }

    if(!(any(grepl(district_name, pdf_text)))){
      
      table_24 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          sublvl_01 = NA, sublvl_02 = NA, sublvl_03 = NA, summary_row = NA, area_name = NA, hadbast_deh_number = NA, 
          total_housing = NA, pacca_housing = NA, semi_pacca_housing = NA, kacha_housing = NA,
          potable_water = NA, electricity = NA, gas = NA, kitchen = NA, bath_room = NA, latrine = NA, avg_hh_size = NA
        )
      
      natl_table_24 <- rbind(natl_table_24, table_24)
    } else {
              
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    
    pdf_trimmed[grep("SELECTED HOUSING STATISTICS", pdf_trimmed)] <- ""
    more_headers_starts <- grep("HOUSING CHARACTERISTICS", pdf_trimmed)
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}12\\s{6,}13", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*11\\s*12\\s*13", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # on first pass, in some cases cells are not fully splitting, so need to clean up first
    pdf_correct <- gsub("N0 ", "NO ", pdf_trimmed)
    pdf_correct <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?=\\d{7})", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=\\s)-(?=\\s)", "  -  ", pdf_correct, perl = T)
    #pdf_correct <- gsub("\\)(?=\\d)", ")   ", pdf_correct, perl = T)
    #pdf_correct <- gsub("CHAK NO   ", "CHAK NO ", pdf_correct, perl = T)
    
    if(d == 4){
      pdf_correct <- gsub("MARI WASSAYO 1 &    1   1", "MARI WASSAYO 1 & 11", pdf_correct)
    }
    if(d == 5){
      pdf_correct[324] <- "CHISHTIAN TEHSIL                                     87,191   58,867   16,792      11,532       73,237           79,418   2,019       33,561      67,348      61,310           6.15"
      pdf_correct[611] <- "FORT ABBAS TEHSIL                                     56,877   37,195       9,657   10,025       43,543        48,901   506   22,442    49,158      44,791           6.32"
      pdf_correct[660] <- "CHAK NO 245-A/H.L          -               0         -               -           -                     -                   -         -             -                   -           -             0.00"
      pdf_correct[894] <- "CHORRI WALA TOBA           -             8       2             -        6                 -                   -            -            7           7       1        12.75"
      pdf_correct[897] <- "GHULAM WALA TOBA           -             0         -               -          -                   -                   -            -              -               -           -           0.00"
      pdf_correct[902] <- "SOLI SARANA                -             0         -               -          -                   -                   -            -              -               -           -           0.00"
      pdf_correct[977] <- "HAROONABAD TEHSIL                                     61,656   35,738   16,803      9,115       46,693           58,031     670     25,568      55,549      52,165           6.15"
      pdf_correct[1219] <- "MINCHINABAD TEHSIL                                71,607   42,691   16,289    12,627       59,344           61,478     806     24,114      53,712      42,123           6.29"
    }
    if(d == 6){
      pdf_correct[1197] <- "KHARBA TOBA              -                    -             -                -          -                     -                -            -             -                 -           -                 -"
    }
    if(d == 12){
      pdf_correct[205] <- "ASHGHER TARINAN (ASGHAR TARIN)           0000119      257         57        114       86      169            138               -         100      100        73       7.44"
      pdf_correct[206] <- ""
      pdf_correct[207] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]
    }
    if(d == 21){
      pdf_correct[249] <- "TAKWARA NALLAH HUSSAIN ZAI       0000002     346           12           35        299          66          338             7         321        307         330       12.11"
      pdf_correct[250] <- ""
      pdf_correct[251] <- ""
      pdf_correct[253] <- "TAKWARA NALLAH YAQUB ZAI    0000003        4              -                -            4             -              4               -             4          4           4       14.25" 
      pdf_correct[254] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]
    }
    if(d == 29){
      pdf_correct[266] <- "BELO BOZDAR        -                       224        103          4        117        214            169              -           76        71        73           5.11"
      pdf_correct[267] <- "PIPRI              -                       590        174         26        390        586            530            6        181       192       185           5.13"
      
    }
    if(d == 31){
      pdf_correct[409] <- "KEERAN-WALA KHAS                     286/289       769         328       414           27       557        751        510        644      661       684       6.55"
    }
    if(d == 35){
      pdf_correct[116] <- "DHERI LABAN BANDI(CHANG BANDI)        0000191        1313         1,171               53            89         1,159           1,284               -          1,175         1,282        1,285            5.87"
      pdf_correct[117] <- ""
      pdf_correct[118] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]
    }
    if(d == 42){
      pdf_correct[59] <- "(KALAN SHADI HAR)S.BUTTA KALAN	             0000097        166             1              13         152          51       161             -            24              19             47            5.82"
      pdf_correct[60] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]
    }
    if(d == 46){
      pdf_correct[188] <- "KAREZ QAMBARANI(KHASOON DOON)     0000009        67              -                -              67             21            34             -            61        65           28            5.64"
      pdf_correct[189] <- ""
      pdf_correct[190] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]      
    }
    if(d == 56){
      pdf_correct[233] <- "GANNO KALHORO          -                  939        522        119       298        922         864       346        833       893           926           6.44"
      pdf_correct[234] <- "GARHI              -                      724        379        204       141        720         607       528        680       711           717           5.32"
      pdf_correct[235] <- "MORI               -                     1027        694        212       121      1,004       1,017       676        992       906         1,021           6.28"
    }
    if(d == 57){
      pdf_correct[177] <- "CHAH SULTAN WALA    -------                   6          1              -             5           6                  -               -            2          -             -             6.83"
    }
    if(d == 74){
      pdf_correct[39] <- "CHAK SARDAR KHAN (SARWAR) KHAN        0000008      144      78            33     33            121          131       -        47            100      82             7.47"
      pdf_correct[40] <- ""
      pdf_correct[41] <- ""
      pdf_correct <- pdf_correct[pdf_correct != ""]      
    }
    if(d == 76){
      pdf_correct[857] <- "SHAGOKASS           -                   177        46        24         107     147     177             -         157      169       172    10.29"
    }
    if(d == 85){
      pdf_correct[32] <- "ABADGAR TC                               543           153        1,001      1,202     1,022             10      924          927       1,062            -              6.22"
    }
    if(d == 87){
      pdf_correct[505] <- "QADIR PURRAWAN SHARQI           0000204      1577        796          265     516        1,491        1,539      124      399        1,013    1,047        6.08"
    }
    if(d == 103){
      pdf_correct[20] <- "KHUSHABA KAREZ SARIAB       -           482        236       166       80       444       473         459        455       474        476           11.91"
    }
    if(d == 106){
      pdf_correct[16] <- "NAGIAL UMER KHAN             -                        324        294         12        18         269        322         245        299       307       309           5.79"
    }
    if(d == 114){
      pdf_correct[212] <- "BELO BAGERJI               -                  268        175         12         81         255           27           5          5        13         5        5.30"
    }

    pdf_split <- strsplit(trimws(pdf_correct), "\\s{2,}")
    
    # create a dataframe, identify area aggregate rows by missing hadbast / deh number field 
    # (note that Sindh does not use these and many other areas also have null codes; code sequences are not consistent across forms)
    
    total_out <- tibble()
    line_break <- F
    partial_break <- F
    for(x in 1:length(pdf_split)){
      
      row_extract <- pdf_split[[x]]
      if(length(row_extract) > 1){
        if(length(row_extract) > 2){
        
          if(line_break == T){
            area_correct <- pdf_split[[x-1]]
            row_extract <- c(area_correct, row_extract)
            line_break <- F
          } else {
            if(partial_break == T){
              area_correct <- paste0(pdf_split[[x-1]][1], " ", row_extract[[1]][1])
              correct_deh <- pdf_split[[x-1]][2]
              row_extract <- c(area_correct, correct_deh, row_extract[2:length(row_extract)])
              partial_break <- F
            }
          }
    
          row_out <- tibble(
            area_name = row_extract[1],
            summary_row = ifelse(area_name %in% administrative_areas, ifelse(length(row_extract) < 13, "YES", "NO"), "NO"),
            hadbast_deh_number = ifelse(summary_row == "NO", row_extract[2], NA),
            total_housing =  ifelse(summary_row == "NO", 
                     row_extract[3],
                     row_extract[2]),
            pacca_housing = ifelse(summary_row == "NO", 
                     row_extract[4],
                     row_extract[3]),
            semi_pacca_housing = ifelse(summary_row == "NO", 
                     row_extract[5],
                     row_extract[4]),
            kacha_housing = ifelse(summary_row == "NO", 
                     row_extract[6],
                     row_extract[5]),
            potable_water = ifelse(summary_row == "NO", 
                     row_extract[7],
                     row_extract[6]),
            electricity = ifelse(summary_row == "NO",
                     row_extract[8],
                     row_extract[7]),
            gas = ifelse(summary_row == "NO",
                     row_extract[9],
                     row_extract[8]),
            kitchen = ifelse(summary_row == "NO",
                     row_extract[10],
                     row_extract[9]),
            bath_room = ifelse(summary_row == "NO",
                     row_extract[11],
                     row_extract[10]),
            latrine = ifelse(summary_row == "NO",
                     row_extract[12],
                     row_extract[11]),
            avg_hh_size = ifelse(summary_row == "NO",
                     row_extract[13],
                     row_extract[12])
          )
          
          total_out <- rbind(total_out, row_out)
            } else {partial_break <- T}
          } else {line_break <- T}
    }
    
    #figure out subdivision parent-child relationships by relative positions
    subdivisions <- total_out %>% mutate(id = row_number()) %>%
      filter(summary_row == "YES")
    #
    subdivisions <- subdivisions %>%
        mutate(child_03 = ifelse(id - (lead(id, n = 1)-1) != 0, "child", "parent"))
    subdivisions$child_03[is.na(subdivisions$child_03)] <- "child"
    #
    temp <- subdivisions %>%
      filter(child_03 == "parent") %>%
      mutate(child_02 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
    temp$child_02[is.na(temp$child_02)] <- "child"
    subdivisions <- left_join(subdivisions, temp)
    #
    temp <- subdivisions %>%
      filter(child_03 == "parent" & child_02 == "parent") %>%
      mutate(child_01 = ifelse(id - (lead(id, n = 1)-1) == 0, "parent", "child"))
    temp$child_01[is.na(temp$child_01)] <- "child"
    subdivisions <- left_join(subdivisions, temp)
    #
    total_out$child_04 <- ifelse(total_out$summary_row == "YES", "parent", "child")
    #
    table_24 <- left_join(total_out, subdivisions)
    #
    table_24$child_01 <- ifelse(table_24$child_01 == "parent", table_24$area_name, NA)
    table_24$child_02 <- ifelse(table_24$child_02 == "parent" & is.na(table_24$child_01), table_24$area_name, NA)
    table_24$child_03 <- ifelse(table_24$child_03 == "parent" & is.na(table_24$child_01) & is.na(table_24$child_02), table_24$area_name, NA)
    table_24$child_04 <- ifelse(table_24$child_04 == "parent" & is.na(table_24$child_01) & is.na(table_24$child_02) & is.na(table_24$child_03), table_24$area_name, NA)
    #
    # fill forward missing names
    table_24 <- table_24 %>% fill(c(child_01, child_02, child_03, child_04)) %>%
      rename(
        district_name = child_01,
        sublvl_01 = child_02,
        sublvl_02 = child_03,
        sublvl_03 = child_04
      )
    
    table_24 <- table_24 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    sublvl_01, sublvl_02, sublvl_03, summary_row, everything()) %>%
      dplyr::select(-id)

    # correct over-fills
    table_24$sublvl_02[table_24$sublvl_01 == table_24$area_name] <- NA
    table_24$sublvl_03[table_24$sublvl_01 == table_24$area_name] <- NA
    table_24$sublvl_03[table_24$sublvl_02 == table_24$area_name] <- NA

    if(d == 32){
      table_24$sublvl_03[table_24$area_name == "SUNTSER" & table_24$summary_row == "NO"] <- "SUNTSER UC"
    }        
    if(d == 58){
      table_24$sublvl_03[table_24$area_name == "TOHMULK" & table_24$summary_row == "NO"] <- "TOHMULK UC"
    }    
    if(d == 100){
      table_24$sublvl_03[table_24$area_name == "GOWARGO" & table_24$summary_row == "NO"] <- "DASHT E SHAHBAZ UC"
    }    
    if(d == 133){
      table_24$sublvl_03[table_24$area_name == "BESIMA" & table_24$summary_row == "NO"] <- "BESIMA UC"
      table_24$sublvl_03[table_24$area_name == "NAG" & table_24$summary_row == "NO"] <- "NAG UC"      
    }

    # cleanup numerics
    table_24 <- cbind(
          table_24 %>% dplyr::select(1:9),
          table_24 %>% dplyr::select(10:20) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_24[is.na(table_24)] <- 0
    table_24$hadbast_deh_number[table_24$summary_row == "YES"] <- NA
    table_24$sublvl_01[table_24$sublvl_01 == 0] <- NA
    table_24$sublvl_02[table_24$sublvl_02 == 0] <- NA
    table_24$sublvl_03[table_24$sublvl_03 == 0] <- NA
    
    prov_path <- paste0("./processed_forms/", unique(table_24$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_24, paste0(dist_path, "table_24.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_24 <- rbind(natl_table_24, table_24)
    
    }
    } # end else 
  
  # Process Table 25, Detailed Population of Urban Localities ---
  if("25" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "25.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "25.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)  
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d %in% c(28, 86)){
      pdf_text <- gsub(district_name, "", pdf_text)
    }
    if(d == 16){
      pdf_text[60] <- ""
    }
    if(d == 19){
      pdf_text[16] <- "DERA BUGTI DISTRICT      100,365   52,024    48,341        -       37.59       57.69      16.01            -    8,126     2,556        -    5,852          916             -    1,489        165            -   99,104            1,261    57,162      40,635      2,809       9,323     31,787"
    }
    if(d == 21){
      pdf_text[17] <- "DERA ISMAIL KHAN DISTRICT     360,218   187,793   172,382          43     68.74      77.96     58.72      46.51    39,995   28,175       5    38,676   24,167         8    18,665     13,843        4    357,434       2,784    264,624   200,427   21,234   79,894   179,050"
      pdf_text[18] <- "DERA ISMAIL KHAN TEHSIL       218,018   114,116   103,865          37     79.51      85.14     73.31      48.65    25,199   20,496       5    27,625   19,829         8    15,365     12,284        4    215,348       2,670    165,925   128,992   13,145   52,320   116,987"
      pdf_text[19] <- "DERA ISMAIL KHAN CANTONMENT     5,694     3,519     2,175           -     89.11      94.29     79.98       0.00       553      435       -     1,435      411         -       601        370        -      5,231         463      4,499     3,658      228    2,302     3,547"
      pdf_text[20] <- ""
    }      
    if(d == 30){
      pdf_text[53] <- "GUJRANWALA MUNICIPAL CORPORATION       2,028,421      1,028,354   999,797         270    78.73   79.65   77.80    52.96   308,307 259,373        66 183,488 202,715       38   46,651   64,047         17   1,959,781 68,640 1,528,756 1,165,668 110,669   487,464    936,604"
      pdf_text[54] <- ""
      pdf_text[55] <- "CHARGE NO 06                 27,403    15,193    12,200          10    82.78   82.66   82.96    60.00     5,058   3,211         3   3,067   2,927        -      810    1,122          -      27,143     260     21,867   17,671   1,488     6,937    14,177"
    }
    if(d == 37){
      pdf_text[16] <- "HYDERABAD CANTONMENT (Part of Hyderabad City Taluka)       52,062          28,613             23,443              6           82.87          85.70             79.32             16.67           5,653           4,848               -            8,214           5,389              -           4,503                2,942              -            48,747             3,315             41,177             33,065               3,058                14,330                27,591"
      pdf_text[17] <- ""
      pdf_text[30] <- "CIRCLE NO 01                                   7,705               4,090              3,615              -           73.92          74.19             73.61              0.00             890             723               -              822             839                 -           -                     187"
      pdf_text[32] <- "HYDERABAD MUNICIPAL CORPORATION (Part of Hyderabad City Taluka)     673,091      348,749            324,258             84           70.53          72.05             68.89             39.29          76,985          68,486              11           67,391          65,900             12             29,106            18,656                 3       632,782             40,309              522,068            410,925            37,811               172,788               307,639"
      pdf_text[33] <- ""
      pdf_text[35] <- "CIRCLE NO 01                                   7,131               3,806              3,324              1           25.94          30.32             20.73            100.00             423             258               1              201              81                 -             -                    20"
      pdf_text[38] <- "CIRCLE NO 01                                  13,170               6,981              6,188              1           48.09          55.23             40.00            100.00             816             566               -              804             465                 -             -                   107"
      pdf_text[43] <- "CIRCLE NO 01                                   7,391               3,827              3,564              -           64.87          64.83             64.90              0.00             920             849               -              587             595                 -             -                    60"
      pdf_text[49] <- "CIRCLE NO 07                                  21,116              10,964             10,149              3           62.52          64.03             60.86             33.33           2,325           2,002               -            2,001           1,934                 -             -                  313                 1        20,717                399               15,932             12,560             1,009                 5,539                 8,807"
      pdf_text[51] <- "CIRCLE NO 01                                   7,000               3,620              3,379              1           79.22          81.01             77.28            100.00             946             828               -              814             817                 -             -                   171"
      pdf_text[56] <- "CIRCLE NO 01                                   5,772               2,903              2,868              1           84.86          85.21             84.49            100.00             850             759               -              632             675                 -             -                   105"
      pdf_text[62] <- "CIRCLE NO 01                                   3,438               1,759              1,679              -           49.32          51.61             46.93              0.00             380             337               -              197             186                 -             -                    22"
      pdf_text[67] <- "CIRCLE NO 01                                   3,160               1,631              1,529              -           61.54          62.84             60.15              0.00             391             399               -              285             232                 -             -                    47"
      pdf_text[73] <- "CIRCLE NO 01                                   5,396               2,743              2,653              -           77.02          79.61             74.30              0.00             636             534               -              457             440                 -             -                    97"
      pdf_text[79] <- "CIRCLE NO 01                                   4,700               2,437              2,262              1           58.18          59.62             56.65              0.00             539             467               -              300             281                 -             -                    59"
      pdf_text[84] <- "CIRCLE NO 01                                   5,416               2,756              2,658              2           75.95          80.73             70.98             50.00             672             659               1              592             476                 -             -                   233"
      pdf_text[86] <- "CIRCLE NO 03                                   3,934               2,000              1,934              -           81.71          84.18             79.21              0.00             458             432               -              498             523                 -             -                  173                 -         3,895                39                 3,046              2,407                213                  731                 1,887"
      pdf_text[90] <- "CIRCLE NO 01                                   3,973               2,057              1,916              -           73.49          72.10             74.98              0.00             509             455               -              445             462                 -             -                    82"
      pdf_text[95] <- "CIRCLE NO 01                                   4,251               2,226              2,025              -           68.38          70.02             66.58              0.00             572             433               -              354             350                  -            -                    59"
      pdf_text[99] <- "CIRCLE NO 01                                   5,521               2,862              2,657              2           74.77          73.50             76.12             50.00             679             605               1              563             705                  -            -                   122"
      pdf_text[104] <- "CIRCLE NO 01                                   4,012               2,079              1,932              1           74.85          79.86             69.25            100.00             500             436               -              504             406                 -            -                     87"
      pdf_text[110] <- "CIRCLE NO 01                                   4,334               2,175              2,159              -           74.50          78.29             70.66              0.00             636             551               -              448             444                 -             -                    69"
      pdf_text[116] <- "CIRCLE NO 01                                   4,001               2,083              1,918              -           83.92          84.42             83.38              0.00             359             351               -              531             510                 -             -                   285"
      pdf_text[123] <- "CHARGE NO 17                                  21,144              10,819             10,318              7           61.47          65.63             57.06             42.86           2,135           1,770               -            1,395           1,101                 -            -                   411                 -        17,483              3,661               15,204             11,323                907                4,520                 8,614"
      pdf_text[124] <- "CIRCLE NO 01                                   4,736               2,449              2,287              -           81.10          84.43             77.46              0.00             620             534               -              472             430                 -            -                    257"
      pdf_text[130] <- "CIRCLE NO 01                                   2,707               1,381              1,326              -           86.63          89.17             83.98              0.00             335             294               -              350             339                 -             -                   134"
      pdf_text[136] <- "CIRCLE NO 01                                   4,766               2,568              2,198              -           90.06          92.86             86.76              0.00             571             572               -              660             522                 -             -                   279"
      pdf_text[142] <- "CIRCLE NO 01                                   3,075               1,521              1,553              1           77.62          79.97             75.27            100.00             357             309               -              333             387                 -            -                    122"
      pdf_text[149] <- "CIRCLE NO 01                                   2,520               1,325              1,194              1           84.98          86.44             83.37            100.00             303             264               -              379             343                 -            -                    119"
      pdf_text[154] <- "CIRCLE NO 01                                   3,264               1,696              1,568              -           82.95          85.25             80.44              0.00             340             299               -              475             462                -              -                   209"
      pdf_text[160] <- "CIRCLE NO 07                                   7,833               3,997              3,836              -           68.89          69.50             68.24              0.00             871             743               -              808             968                 -            -                   174                 -         6,326              1,507                6,110              4,892                460                2,242                 3,506"
      pdf_text[162] <- "CIRCLE NO 01                                   4,120               2,043              2,075              2           64.10          73.48             55.42              0.00             474             473               -              375             247                  -             -                  101"
      pdf_text[166] <- "CIRCLE NO 01                                   2,997               1,576              1,421              -           78.83          80.57             76.94              0.00             335             284               -              430             467                 -             -                   101"
      pdf_text[171] <- "CIRCLE NO 01                                   2,028               1,018              1,010              -           89.33          91.92             86.69              0.00             264             268               -              354             306                 -             -                    75"
      pdf_text[178] <- "CIRCLE NO 01                                   5,897               3,028              2,869              -           77.71          80.68             74.53              0.00             646             653               -              691             644                 -              -                  210"
      pdf_text[185] <- "CIRCLE NO 01                                   1,569                 793                776              -           73.59          70.35             76.86              0.00             214             174               -              155             240                 -             -                    37"
      pdf_text[192] <- "CIRCLE NO 01                                   3,263               1,666              1,597              -           95.29          95.60             94.96              0.00             433             372               -              505             552                 -               -                 182"
      pdf_text[197] <- "CIRCLE NO 06                                   3,626               1,816              1,810              -           86.38          86.59             86.17              0.00             351             326               -              549             592                 -              -                 253                 -         3,619                    7              2,915              2,377               289                   974                 2,041"
      pdf_text[199] <- "CIRCLE NO 01                                   3,652               1,876              1,776              -           87.35          90.09             84.44              0.00             536             464               -              517             542                -              -                   167"
      pdf_text[206] <- "CIRCLE NO 01                                   5,508               2,799              2,708              1           92.54          94.44             90.54            100.00             572             567               -              922             911               -              -                    461"
      pdf_text[213] <- "CHARGE NO 05                                  21,136              10,843             10,289              4           45.57          51.71             39.14             25.00           1,791           1,449               1            1,350             830              -               -                    108"
      pdf_text[214] <- "CIRCLE NO 01                                  10,218               5,243              4,975              -           53.38          57.60             48.86              0.00             995             888               -              879             635               -              -                     90"
      pdf_text[217] <- "CHARGE NO 04                                  71,760              37,760             33,994              6           69.57          78.14             59.98             50.00           7,413           6,126               2            8,050           5,287                -             -                  2,107"
      pdf_text[218] <- "CIRCLE NO 01                                  11,422               5,867              5,555              -           84.52          91.21             77.49              0.00           1,204           1,187               -            1,601           1,403                -              -                   519"
      pdf_text[227] <- "HYDERABAD CANTONMENT (Part of Latifabad Taluka)              42,386              23,435             18,937             14           69.70          72.62             65.89             21.43           5,010           4,224               1            6,070           3,492              -              1,245               577                 -        41,995                391               32,851             26,219             2,018                11,360                20,167"
      pdf_text[228] <- ""
      pdf_text[233] <- "CIRCLE NO 03                                   4,704               2,401              2,303              -           72.32          71.58             73.09              0.00             642             551               -              475             562             -                 -                    95"
      pdf_text[234] <- "CIRCLE NO 04                                   5,861               2,997              2,864              -           60.43          58.00             63.00              0.00             711             710               -              455             529              -               -                    65                 -         5,827                 34                4,493              3,533               322                 1,328                 2,559"
      pdf_text[237] <- "HYDERABAD MUNICIPAL CORPORATION (Part of Latifabad Taluka)          661,304     342,098             319,074            132           72.09          74.67             69.31             37.12          69,431          58,919              17           67,630          64,519             21             39,913            28,412                 4       615,053             46,251              504,582            395,199            36,603               162,195               318,452"
      pdf_text[238] <- ""
      pdf_text[240] <- "CIRCLE NO 01                                   6,939               3,585              3,353              1           65.03          70.57             59.11            100.00             775             655               -              657             544                 -              -                  227"
      pdf_text[247] <- "CIRCLE NO 01                                   2,790               1,453              1,337              -           82.14          82.49             81.76              0.00             412             330               -              384             424                 -             -                   109"
      pdf_text[253] <- "CIRCLE NO 01                                   3,402               1,749              1,653              -           67.92          69.28             66.41              0.00             416             358               -              281             253                 -             -                    41"
      pdf_text[260] <- "CIRCLE NO 01                                   2,287               1,168              1,119              -           76.99          76.07             77.95              0.00             266             243               -              263             298                 -             -                   74"
      pdf_text[264] <- "CIRCLE NO 01                                   4,361               2,241              2,120              -           81.32          84.10             78.43              0.00             576             509               -              553             519                 -             -                   269"
      pdf_text[269] <- "CIRCLE NO 01                                   4,022               2,040              1,982              -           84.79          85.96             83.58              0.00             448             405               -              567             582                 -             -                   300"
      pdf_text[270] <- "CIRCLE NO 02                                   5,950               3,074              2,876              -           84.05          84.32             83.76              0.00             720             605               -              722             759                 -             -                  324                 -         5,936                 14                4,697              3,734               364                   958                 3,092"
      pdf_text[276] <- "CIRCLE NO 01                                   4,960               2,426              2,534              -           96.55          97.51             95.66              0.00             333             406               -              671             778                 -             -                   715"
      pdf_text[282] <- "CIRCLE NO 01                                   6,098               3,107              2,990              1           89.32          90.36             88.24            100.00             629             593               1              825             916                 -             -                   422"
      pdf_text[287] <- "CIRCLE NO 01                                   6,211               3,194              3,017              -           84.47          85.87             82.99              0.00             716             643               -              911             975                 -             -                   263"
      pdf_text[294] <- "CIRCLE NO 01                                   3,616               1,869              1,746              1           94.90          95.43             94.39              0.00             299             277               -              527             546                -              -                   460"
      pdf_text[301] <- "CIRCLE NO 01                                   5,036               2,521              2,514              1           80.86          84.96             76.80              0.00             593             536               -              590             567                -              -                   400"
      pdf_text[305] <- "CIRCLE NO 01                                   6,419               3,238              3,178              3           88.56          89.95             87.18             66.67             732             675               -              960           1,052                -             -                    487"
      pdf_text[307] <- "CIRCLE NO 03                                   4,820               2,516              2,304              -           81.28          82.26             80.22              0.00             685             574               -              609             661                 -             -                  126                 -         4,808                   12              3,792              3,035               278                   959                 2,337"
      pdf_text[312] <- "CIRCLE NO 01                                   7,103               3,534              3,567              2           88.84          90.84             86.89             50.00             710             703               -              946           1,023                 -              -                  530"
      pdf_text[317] <- "CIRCLE NO 01                                   4,979               2,585              2,393              1           88.61          88.95             88.31              0.00             549             515               -              644             730                 -              -                  376"
      pdf_text[322] <- "CIRCLE NO 01                                   4,963               2,514              2,448              1           94.16          95.54             92.76            100.00             397             396               -              610             710                 -              -                  656"
      pdf_text[327] <- "CIRCLE NO 01                                   5,037               2,541              2,495              1           83.33          86.55             80.00            100.00             573             530               1              661             661                 -              -                  205"
      pdf_text[332] <- "CIRCLE NO 01                                   5,451               2,772              2,678              1           80.82          81.58             80.08              0.00             589             551               -              760             772                 -              -                  273"
      pdf_text[338] <- "CIRCLE NO 01                                   2,634               1,365              1,269              -           74.74          75.29             74.18              0.00             324             311               -              284             295                 -              -                   54"
      pdf_text[344] <- "CIRCLE NO 01                                   7,268               3,775              3,491              2           68.00          70.36             65.47             50.00             920             725               -              650             658                 1              -                 121                 -         7,225                 43                5,428              4,211               323                 2,961                 3,169"
      pdf_text[349] <- "CIRCLE NO 01                                   5,636               2,930              2,706              -           75.72          83.74             66.94              0.00             717             551               -              786             482                 -              -                  139"
      pdf_text[355] <- "CIRCLE NO 01                                   6,019               3,255              2,761              3           68.07          71.49             63.96             66.67             709             464               1              404             265                 -              -                  129"
      pdf_text[363] <- "CIRCLE NO 01                                   5,213               2,730              2,466             17           68.28          70.48             66.32             17.65             517             506               1              540             460                -               -                   89"
      pdf_text[370] <- "CIRCLE NO 01                                   5,865               3,003              2,862              -           48.77          53.15             44.16              0.00             539             455               -              413             300                 -              -                   60"
      pdf_text[376] <- "CIRCLE NO 01                                   5,470               2,802              2,666              2           72.54          76.05             68.90              0.00             686             557               -              599             582                 -              -                  101"
      pdf_text[381] <- "CIRCLE NO 01                                   3,671               1,915              1,755              1           65.99          67.40             64.43            100.00             472             409               -              358             339                 1              -                  59                 -         3,670                  1                2,805              2,212               181                   800                 1,667"
      pdf_text[386] <- "CIRCLE NO 01                                   8,026               4,141              3,885              -           52.86          59.55             45.55              0.00             899             661               -              531             341                 -             -                    80"
      pdf_text[393] <- "CIRCLE NO 01                                   3,220               1,649              1,571              -           24.87          30.49             19.10              0.00             179             108               -               56              25                  -             -                    5"
      pdf_text[398] <- "HYDERABAD CANTONMENT (Part of Qasimabad Taluka)       5,319               3,092              2,226              1           71.30          80.06             58.29            100.00             431             356               -            1,178             325                 1             341               232                 -         3,993              1,326                4,255              3,476               198                 2,004                 3,159"
      pdf_text[400] <- "CIRCLE NO 01                                   1,205                 606                599              -           68.34          72.97             63.64              0.00              92              72               -               82              93                -                  -               112"
      pdf_text[403] <- "CHARGE NO 01                                  60,732              31,864             28,867              1           52.87          60.34             44.60            100.00           4,927           3,732               -            4,047           2,469                 -                  -            1,504"
      pdf_text[404] <- "CIRCLE NO 01                                   5,280               2,709              2,571              -           42.29          48.05             36.08              0.00             296             241               -              287             184                 -                  -               84"
      pdf_text[411] <- "CIRCLE NO 01                                  11,162               5,877              5,284              1           87.73          93.49             81.30            100.00           1,086           1,300               -            1,406             960                 -                 -               717"
      pdf_text[417] <- "CIRCLE NO 01                                   8,666               4,466              4,200              -           89.61          93.96             85.01              0.00             706             783               -              892             828                 -                  -             905                 -         6,894              1,772                6,794              5,382               587                 1,736                 5,174"
      pdf_text[424] <- "CIRCLE NO 01                                   9,619               5,199              4,417              3           93.60          96.39             90.29            100.00             698             876               -            1,197           1,067                 -                  -            1,183"
      pdf_text[430] <- "CIRCLE NO 01                                  29,058              15,382             13,670              6           65.55          72.32             57.92             66.67           2,488           2,050               1            2,512           1,644                 -                 -               929"
      pdf_text[434] <- "CIRCLE NO 01                                  13,577               7,065              6,511              1           66.44          74.99             57.22            100.00           1,279             958               -              991             691                 -                  -              458"
      pdf_text[437] <- "CIRCLE NO 01                                  10,810               5,618              5,192              -           34.44          45.20             22.82              0.00             869             471               -              413             132                 -                  -               37"
    }
    if(d == 38){
      pdf_text[15] <- "ISLAMABAD DISTRICT"
      pdf_text[18] <- "ISLAMABAD METROPOLITAN CORPORATION    1,009,003   535,605   473,242           156    81.14      85.95     75.63            58.33   114,414    85,314            37   115,536    84,228            26   97,786     79,735            12   953,208    55,795   782,274    617,084    61,260   254,698   571,330"
      pdf_text[19] <- ""
      pdf_text[20] <- ""
    }
    if(d == 48){
      pdf_text[12] <- "KARACHI CENTRAL DISTRICT"
      pdf_text[15] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Gulberg Sub-Division)            496,094          257,941            238,090              63             87.52          87.75             87.27             68.25          49,317          43,011                  12        73,922           72,059                 18        50,665            43,739                  9         485,813           10,281              401,313          326,406            41,308              145,412              293,283"
      pdf_text[16] <- ""
      pdf_text[142] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Liaquatabad Sub-Division)      449,098   233,469   215,513   116   83.04   82.73   83.40    39.66   63,149   54,065   21   61,286   62,604   19   13,832   13,344   3   443,361   5,737   356,126   285,151   25,523   129,039   223,591" 
      pdf_text[143] <- ""
      pdf_text[254] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Nazimabad Sub-Division)     446,347   233,024   213,259   64   81.08   81.79   80.31    56.25   54,095   44,566   10   61,465   57,551   15   24,311   22,972   2   428,736   17,611   354,354   284,196   30,017   115,977   240,386"  
      pdf_text[255] <- ""
      pdf_text[357] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of New Karachi Sub-Division)      871,245   452,843   418,282   120   77.03   76.91   77.17    44.17   111,627   96,383   23   97,700   100,175   15   32,015   28,444   5   867,798   3,447   672,083   532,204   48,644   237,128   409,682"
      pdf_text[358] <- ""
      pdf_text[558] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of North Nazimabad Sub-Division)          708,598   364,751   343,716   131   81.94   83.25   80.58    45.04   65,134   56,388   17   90,186   87,538   13   66,499   59,380   8   695,458   13,140   559,579   447,113   54,295   187,601   386,133"
      pdf_text[559] <- ""
    }
    if(d == 49){
      pdf_text[18] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Ferozabad Sub-Division)      761,631     401,237    360,249          145   81.31        83.02      79.40       57.24    90,948     75,364       20   102,776    90,898            37    48,229     38,614           10     692,733         68,898     599,572    479,409               50,699    194,981     408,788"
      pdf_text[19:20] <- ""
      pdf_text[205] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Gulshan-e-Iqbal Sub-Division)      647,144      333,725       313,290     129      80.26   82.05   78.35    59.69   52,500   46,354   15   70,912   69,162   38    79,084   65,261   17   633,373   13,771   508,984   409,122   52,526   168,866   359,670"
      pdf_text[206:210] <- ""
      pdf_text[338] <- "FAISAL CANTONMENT (Part of Gulshan-e-Iqbal Sub-Division)       194,656      99,622    95,008   26   94.68   95.38   93.95    73.08   12,968   13,477   5   25,941   26,909   7   36,716   30,162   5   192,505   2,151   159,425   133,058   18,276   48,415   126,078"
      pdf_text[339:341] <- ""
      pdf_text[395] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Gulzar-e-Hijri Sub-Division)      730,689       388,431       342,108       150       54.98   59.88   49.28    42.00   60,085   40,766   18   50,844   38,170   19   30,467   20,861   6   714,629   16,060   520,650   389,584   33,305   152,393   281,695"
      pdf_text[396:400] <- ""
      pdf_text[475] <- "MALIR CANTONMENT (Part of Gulzar-e-Hijri Sub-Division)         61,762     33,113   28,648    1   75.98   81.12   69.91        -   4,502   3,594   -    7,652   5,148   -    6,843   5,139   -   60,089   1,673   45,748   36,421   3,628   17,137   31,512"
      pdf_text[476] <- ""
      pdf_text[486] <- "JAMSHED QUARTERS SUB-DIVISION    479,433   250,660    228,699               74     82.79     84.02   81.44    58.11   52,353   46,646   13   68,134   63,605   18   36,649   27,268   5   447,053   32,380   383,834   309,039   37,921   124,761   271,187"
      pdf_text[487] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Jamshed Quarters Sub-Division)     467,312      244,397      222,842     73        82.67     83.88   81.34    57.53   50,932   45,412   13   66,334   61,924   17   35,628   26,512   5   437,237   30,075   374,068   301,183   36,913   121,976   264,329"
      pdf_text[488:493] <- ""
      pdf_text[596] <- "KARACHI CANTONMENT (Part of Jamshed Quarters Sub-Division)        12,121         6,263         5,857    1   87.37   89.32   85.23   100.00   1,421   1,234   -   1,800   1,681   1   1,021     756   -    9,816    2,305    9,766    7,856   1,008    2,785    6,858"
      pdf_text[597:598] <- ""
    }
    if(d == 50){
      pdf_text[14] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Aram Bagh Sub-Division)    128,313    69,314        58,990                 9           85.62           86.07     85.07             66.67      17,600       14,434              3         21,620     18,364             1       5,980      4,185                 -     123,816         4,497        104,195            84,175            10,174              36,349              71,691"
      pdf_text[15:17] <- ""
      pdf_text[18] <- "CHARGE NO 01                      24,795    12,988        11,805                 2           87.16           88.15     86.04            100.00       3,693        3,312               2         3,964      3,492               -       771        527                 -     24,380            415         20,088            16,245                2,113            6,239              13,716"
      pdf_text[55] <- "CLIFTON CANTONMENT (Part of Civil Lines Sub-Division)             246,460   131,686       114,763                11           89.88           89.90     89.84             81.82      24,429       19,494               1        31,869     31,191               6    38,431     31,050                 2    231,898         14,562        204,942           170,820               25,530           69,930             158,991"
      pdf_text[56] <- ""
      pdf_text[100] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Civil Lines Sub-Division)        223,950   122,427       101,493                30           75.03           78.41     70.84             20.00      25,996       18,327               5        25,781     17,889               1    17,866     13,716                 -    204,686         19,264        176,624           140,646               14,708              66,924          122,171"
      pdf_text[101:103] <- ""
      pdf_text[152] <- "KARACHI CANTONMENT (Part of Civil Lines Sub-Division)              2,709     1,527            1,182              -           90.54           85.64     96.84                 -         207          170               -           344         331              -       486        434                 -      1,683          1,026             2,388             2,096               292               1,177               2,040"
      pdf_text[153] <- ""
      pdf_text[158] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Garden Sub-Division)                 363,967   188,988       174,945                34           82.27           83.09     81.38             23.53      52,571       46,072               2        51,230     48,700               3    12,153     10,550                 -    339,415         24,552        293,277           237,368               26,733          100,290             194,051"
      pdf_text[159:161] <- ""
      pdf_text[267] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Lyari Sub-Division)                  661,926   341,179       320,658                89           67.49           71.18     63.58             34.83      89,537       79,168             18         65,412     51,156               8    10,477      7,609                 1    639,253         22,673        514,425           406,596               42,216          159,948             323,730"
      pdf_text[268:270] <- ""
      pdf_text[383] <- "CLIFTON CANTONMENT (Part of Saddar Sub-Division)             52,038    27,903           24,134              1           90.60           91.41     89.63            100.00       5,292        4,280               -         6,747      5,941               -     7,724      6,182                 -     49,075          2,963         42,564               35,005             5,053              15,549              32,304"
      pdf_text[384] <- ""
      pdf_text[395] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Saddar Sub-Division)                   36,275    19,252           17,022              1           90.04           90.46     89.57            100.00       4,508        3,654                 -       6,332       5,806              1     2,558      2,340                 -     31,901          4,374            30,018            24,588             3,401              11,607              21,859"
      pdf_text[396:398] <- ""
      pdf_text[409] <- "KARACHI CANTONMENT (Part of Sadar Sub-Division)             53,592    28,415           25,162             15           83.21           85.83     80.24             40.00       6,293        5,304               2         7,604       5,991              3     4,276      3,386                 1     43,564         10,028            42,125            33,838             3,145              16,668              29,872"
      pdf_text[410] <- ""
    }
    if(d == 51){
      pdf_text[19] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Baldia Sub-Division)      832,583          434,973     397,480          130      64.58        70.14      58.45       23.85    104,109     79,654     13    74,569       54,188             9    10,349       7,413           -        816,297        16,286        611,553       460,758            39,606    204,585 341,161"
      pdf_text[20:24] <- ""
      pdf_text[70] <- "CIRCLE NO 02    6,286    3,163    3,123   -   74.65   80.34   68.92        -     751     699   -     829     640   -   194   162   -    5,514     772    4,883    3,801     388    1,697    3,095"
      pdf_text[128] <- "CHARGE NO 15                   77,977  40,914   37,013         50     63.39   67.80   58.60     6.00    7,719    6,497   1    7,511    5,210   2   2,126   1,447   -    76,154   1,823    56,825    42,282    3,391    17,501    32,564"
      pdf_text[159] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Harbour Sub-Division)     396,842     211,885     184,909         48     56.80   63.58   48.79    43.75   46,527   32,575   7   32,195   16,690   9   5,739   3,314   -   392,836   4,006   291,781   220,620   17,535   102,619   161,944"
      pdf_text[160:164] <- ""
      pdf_text[191] <- "CIRCLE NO 04                   12,138     6,388     5,750         -    48.64     58.87   37.23        -    1,396      843   -      669      220   -       76       48   -    12,129        9     8,537     6,064      414     3,372     4,488"  
      pdf_text[210] <- "MANORA CANTONMENT (Part of Harbour Sub-Division)     4,273          2,319     1,954         -    82.18     86.03   77.53        -      660      498   -      665      472   -      125       96   -     3,543      730     3,221     2,539      174       656     2,278"  
      pdf_text[211:213] <- ""
      pdf_text[217] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Manghopir Sub-Division)       550,332       287,384     262,911     37          66.74     68.60   64.70    37.84   64,496   51,430   7   48,970   46,757   4   12,017   10,519   -   525,880   24,452   406,772   312,102   27,421   115,902   230,526"
      pdf_text[218:223] <- ""
      pdf_text[258] <- "CHARGE NO 06                  135,145  70,789     64,344         12     56.16     59.13   52.87     8.33    15,486   12,180    -    7,219    6,349   -      674      640   -   121,874   13,271    98,144    72,929    5,972    28,688    48,130"
      pdf_text[282] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Mauripur Sub-Division)     70,611     38,030     32,578      3     69.96     76.09   62.59        -     8,575    6,519    -    8,743    4,957   -    2,144    1,493   -    62,657    7,954    53,242    41,614    3,453    17,223    34,679"
      pdf_text[283:286] <- ""
      pdf_text[297] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Mominabad Sub-Division)          845,797      438,296     407,422      79      75.79     77.62   73.83    30.38   115,158   95,614   13   95,187   91,084   4   14,185   12,770   -   839,949    5,848   643,369   498,414   41,729   208,842   365,771"
      pdf_text[298:301] <- ""
      pdf_text[324] <- "CIRCLE NO 01    2,927    1,490    1,437   -   71.40   78.10   64.12        -     400     302   -     281     222   -    47    40   -    2,926     1    2,185    1,665     138      374    1,349"   
      pdf_text[382] <- "CIRCLE NO 05    4,565    2,339    2,226   -   72.06   72.67   71.42        -     487     446   -     504     477   -    76    86   -    4,564       1    3,672    2,954     282   1,376    2,162" 
      pdf_text[440] <- "CIRCLE NO 01                    5,589   2,852     2,735         2      70.03   70.00   70.08    50.00      734      677   1      541      521   -      42      58   -     5,578      11     4,191     3,214      292       198     2,277"
      pdf_text[492] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Orangi Sub-Division)       520,195    271,646    248,524       25      63.00   67.90   57.58    44.00   60,555   42,993   6   40,245   31,028   3   5,397   3,908   -   512,116   8,079   378,578   282,374   23,860   117,657   203,032"     
      pdf_text[493:497] <- ""
      pdf_text[503] <- "CIRCLE NO 05    6,004    3,214    2,790   -   68.77   78.16   57.69        -     843     375   -     363     132   -    49    20   -    6,004       -    4,063    2,931     291      220    2,173" 
      pdf_text[561] <- "CIRCLE NO 02                    2,635   1,376     1,259           -      83.45   84.75   82.03        -      417      346   -      361      348   -      53      38   -     2,635       -     2,054     1,578      154       687     1,239"  
      pdf_text[582] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of S.I.T.E Sub-Division)          403,574      222,883         180,663      28       66.97   70.77   62.07    46.43   55,313   37,219   4   43,840   29,351   4   7,681   5,469   -   398,274   5,300   308,554   240,903   19,584   101,975   191,132"
      pdf_text[583:587] <- ""
      pdf_text[624] <- "CHARGE NO 06   30,032   15,672   14,356   4   89.50   89.68   89.31    75.00   4,577   3,799   -   4,439   4,292   -   900     923   -   29,994      38   23,507   19,036   1,827    7,023   14,992"  
    }
    if(d == 53){
      pdf_text[14] <- "KASHMOR DISTRICT"
      pdf_text[15] <- "KASHMOR DISTRICT      253,659      131,231    122,389         39     50.33    63.48     36.34     30.77     21,754   14,374        3   19,112     8,656        4     8,568      2,212          2    226,724    26,935 168,358 120,898     10,478    45,532      96,295"
      pdf_text[16] <- "KANDHKOT TALUKA       136,169      70,458     65,695         16     50.89    64.62     36.38     18.75     12,316    8,248        -   10,553     4,779        1     4,446        945          -    119,754    16,415   90,290    64,605    5,788   23,745      50,788"
    }
    if(d == 61){
      pdf_text[16] <- "KHYBER AGENCY             97,457       50,899   46,543          15    52.90    73.04    30.98      20.00   12,105    5,336         1      8,062     1,517           1    1,979            282          -    96,688           769     64,378     44,491      3,071     15,115      35,609"
      pdf_text[17] <- "JAMRUD TEHSIL             63,728       33,133   30,582          13    51.35    69.07    32.45      15.38    7,464    4,012         1      5,212     1,263           1    1,261            222          -    63,543           185     42,511     29,296      2,109      8,806      22,703"
      pdf_text[23] <- "LANDI KOTAL TEHSIL        33,729     17,766   15,961           2    55.91    80.58    28.03      50.00    4,641    1,324         -      2,850       254           -      718             60          -    33,145           584     21,867     15,195        962      6,309      12,906"
      pdf_text[24] <- ""
      pdf_text[25] <- "LANDI KOTAL TC            33,729      17,766   15,961           2    55.91    80.58    28.03      50.00    4,641    1,324         -      2,850       254           -      718             60          -    33,145           584     21,867     15,195        962      6,309      12,906"
      
    }
    if(d == 64){
      pdf_text[15] <- "KOHAT DISTRICT"
      pdf_text[18] <- "KOHAT CANTONMENT       36,719    22,886     13,831           2     87.25        94.04     74.57       0.00     3,710     2,895          -    11,143     2,833          -    2,599      1,425         -     34,952       1,767     29,195     23,450      1,325      8,507     22,230"
    }
    if(d == 67){
      pdf_text[15] <- "CLIFTON CANTONMENT (Part of Korangi Sub-Division)           6,496          3,402          3,094                -           93.96         92.14         95.98                 -       451            347                  -           656          895                 -     1,458        1,199             -       6,436              60           5,548     4,724          782       1,841          4,651"
      pdf_text[16] <- ""
      pdf_text[19] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Korangi Sub-Division)                1,071,514       566,288           505,047            179           71.68         73.36         69.78             44.69   143,209        115,801                 26       111,904      100,109                27    18,781       15,701             5   1,004,047          67,467         812,229     632,352     48,661     290,609        474,587"
      pdf_text[20:21] <- ""
      pdf_text[166] <- "KORANGI CREEK CANTONMENT (Part of Korangi Sub-Division)                         48,131        25,019            23,112              -           77.70         81.87         73.15                 -     5,564          4,781                  -         6,918        5,518                 -     2,311        1,710             -     43,020            5,111          36,884      29,405      2,388      12,008         25,046" 
      pdf_text[167] <- ""
      pdf_text[168] <- "CHARGE NO 01        48,131        25,019            23,112              -           77.70         81.87         73.15                 -     5,564          4,781                  -         6,918        5,518                 -     2,311        1,710             -     43,020            5,111          36,884      29,405      2,388      12,008         25,046"
      pdf_text[169] <- ""
      pdf_text[170] <- "CIRCLE NO 01                          4,925         2,533             2,392              -           86.88         90.01         83.55                 -       679            555                  -           754          728                 -       191          166             -      4,718              207           3,863       3,081        278       1,149          2,513"
      pdf_text[177] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Landhi Sub-Division)                  546,196       283,476           262,645             75           83.52         84.24         82.74             45.33    75,194         63,846                 17        79,281       76,414                 9    15,712       14,515             1    538,269            7,927         429,101     342,811     31,022     146,707        278,548"
      pdf_text[178:179] <- ""
      pdf_text[282] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Model Colony Sub-Division)                  384,389       197,401           186,906             82           87.09         87.96         86.16             81.71    44,870         40,474                     9     59,372       56,031                25    24,039       22,349            24    375,239               9,150      305,726     247,666     25,159       99,594       217,173"
      pdf_text[283:284] <- ""
      pdf_text[363] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Shah Faisal Sub-Division)                   447,724       231,368           216,209            147           87.92         89.70         86.03             66.67    50,812         46,043                 29        69,886       61,160                46    31,250       27,291            13    439,849            7,875         350,752     281,967     28,316     104,275        250,761"
      pdf_text[364:365] <- ""
      pdf_text[441] <- "FAISAL CANTONMENT (Part of Shah Faisal Sub-Division)              73,106     40,207            32,886             13           91.78         94.88         87.89             61.54         7,594          8,192                 2     16,251           9,100              4     6,328        4,569             1     65,009               8,097       58,352      43,858       2,749      18,897        41,036"
      pdf_text[442] <- ""
    }
    if(d == 69){
      pdf_text[67] <- "LAHORE METROPOLITAN CORPORATION (Part of Lahore Cantonment Tehsil)     551,544    289,998    261,495                51    74.41   77.95     70.44          58.82    75,503    58,329       13    53,275    41,922            11 22,107 20,491                3        513,085       38,459 413,494 317,381              30,553 137,126 260,438" 
      pdf_text[68] <- ""
      pdf_text[237] <- "LAHORE METROPOLITAN CORPORATION (Part of Lahore City Tehsil)     3,653,616        1,905,921        1,746,900            795    78.03   79.22    76.74           44.65   475,385   377,213      142      398,861   379,350        119 168,960 168,640                 31   3,544,216      109,400    2,811,044      2,209,147         214,589    952,304     1,844,513"
      pdf_text[238] <- ""
      pdf_text[623] <- "LAHORE METROPOLITAN CORPORATION (Part of Model Town Tehsil)     2,703,569 1,421,291 1,281,762            516    77.68   80.43    74.59           52.13   337,137   263,234          83   290,286   246,818          93 160,823 143,544                38   2,486,269      217,300 2,081,711 1,644,103         161,442    722,921 1,409,859"
      pdf_text[624] <- ""
      pdf_text[862] <- "LAHORE METROPOLITAN CORPORATION (Part of Raiwind Tehsil)      848,541    448,403    400,031           107    66.62 71.26      61.31           34.58   109,262    79,964          16    68,917    50,485          10    28,785    24,567             4     795,897           52,644    631,608    483,851     44,088    219,108     395,569"
      pdf_text[863] <- ""
      pdf_text[918] <- "LAHORE METROPOLITAN CORPORATION (Part of Shalimar Tehsil)     2,281,557 1,181,239 1,099,956            362    77.69   78.99    76.30           51.66   320,337   261,720          69   241,791   231,209          66    70,447    79,697             17   2,209,790          71,767 1,729,300 1,351,286      125,596    588,471 1,114,135"
      pdf_text[919] <- ""
    }
    if(d == 71){
      pdf_text[38] <- "LARKANA MUNICIPAL CORPORATION              488,006        249,599           238,230            177           66.27           75.43         56.72             35.59        49,341         43,237           24          44,689          28,775           21          28,637        12,750           11       473,847          14,159              350,604           258,036          21,727               81,086               209,444"
      pdf_text[39] <- ""
    }
    if(d == 77){
      pdf_text[17] <- "SWAT RANI ZAI SUB-DIVISION         67,686   34,885   32,799         2   62.08     72.69    50.80     50.00    8,156    6,048     -  5,596  3,153                -     1,940      705             -   67,590          96    47,267      34,156       3,313     12,812     30,136" 
    }
    if(d == 78){
      pdf_text[16] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Airport Sub-Division)        100,842 52,760 48,071              11     71.40 77.18     65.03            54.55 11,224 9,069                  3 10,927 8,003                  1 5,359 3,542                    -    88,149           12,693                  74,877       57,982       4,726          25,724             47,636"
      pdf_text[17] <- ""
      pdf_text[38] <- "FAISAL CANTONMENT (Part of Airport Sub-Division)      28,772 15,547 13,222               3     88.59   91.82   84.71         66.67       2,963    2,494             1    4,908    3,476             1   3,409        2,500           -    27,855              917                  23,365       18,782       1,548            8,553               17,478"
      pdf_text[39] <- ""
      pdf_text[46] <- "MALIR CANTONMENT (Part of Airport Sub-Division)       65,529 36,554 28,971               4     83.26   86.76   78.64         25.00       5,853    4,851             1   10,999    5,837             -   7,011        5,505           -    59,778            5,751                  51,051       41,384       3,774           22,702               38,398"
      pdf_text[47] <- ""
      pdf_text[59] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Ibrahim Hydri Sub-Division)        660,394 356,646 303,689            59     62.52   69.30   54.24            40.68   83,962 58,179               7   68,474 37,251               4   9,100        5,122           2   651,779            8,615                 484,619      369,792      27,357         174,916            302,869"
      pdf_text[60:61] <- ""
      pdf_text[147] <- "KORANGI CREEK CANTONMENT (Part of Ibrahim Hydri Sub-Division)       9,610   6,678   2,931             1     91.27   95.27   80.07        100.00         897      677             -    3,894      608             1     582          274           -     8,685              925                   7,837        6,553         164            5,183                6,300"
      pdf_text[148] <- ""
      pdf_text[152] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Murad Memon Sub-Division)        194,750 101,053 93,683             14     76.04   79.14   72.67         35.71      24,782 21,125               1   24,214 20,203               -   6,513        5,038           -   189,787            4,963                 150,379      119,314      10,635           44,118            99,576"
      pdf_text[153:154] <- ""
      pdf_text[193] <- "MALIR CANTONMENT (Part of Murad Memon Sub-Division)         6,815   4,020   2,795             -     53.90   64.51   36.68             -         694      378             -    1,080      207             -      84           34           -     6,451              364                   4,813        3,831         184            2,134                3,277"
      pdf_text[194] <- ""
    }
    if(d == 79){
      pdf_text[17] <- "MANDI BAHAUDDIN DISTRICT   326,358   159,784   166,484           90     79.10   82.86    75.60            33.33   50,530   44,617            14   30,252   30,879            8   7,521    11,880            3   322,736     3,622   247,969    190,781    21,780    75,653   168,567"
      pdf_text[27] <- "MANDI BAHAUDDIN TEHSIL     217,232   106,393   110,779           60     79.19   82.39    76.24            28.33   33,870   30,074             7   19,166   20,575            4   4,923     7,573            2   214,722     2,510   164,739    126,755    14,112    50,965   111,718"
    }
    if(d == 81){
      pdf_text[16] <- "MARDAN DISTRICT     440,222    225,724   214,444         54    64.84     76.77     52.28       59.26   52,882   37,706    10    43,446   23,008      6   16,246      8,783        2   437,740     2,482     318,822   235,392   22,686   95,978   196,279"
      pdf_text[17] <- "MARDAN TEHSIL       359,024     184,208   174,762         54    64.96     76.84     52.44       59.26   42,786   30,431    10    36,596   19,601      6   13,522      7,608        2   356,580     2,444     260,828   193,598   18,656   82,024   160,764"
      pdf_text[18] <- "MARDAN CANTONMENT   6,824     4,871     1,953          -    88.89     94.84     71.35        0.00      534      410     -     3,036      342      -      404        243        -     6,450       374       5,789     4,977      201    4,239     4,828"
      pdf_text[50] <- "TAKHT BHAI TEHSIL   81,198     41,516    39,682          -    64.28     76.46     51.58        0.00   10,096    7,275     -     6,850    3,407      -    2,724      1,175        -    81,160        38      57,994    41,794    4,030   13,954    35,515"
      pdf_text[51] <- ""
      pdf_text[52] <- "TAKHT BHAI MC     81,198       41,516    39,682          -    64.28     76.46     51.58        0.00   10,096    7,275     -     6,850    3,407      -    2,724      1,175        -    81,160        38      57,994    41,794    4,030   13,954    35,515" 
    }
    if(d == 88){
      pdf_text[16] <- "MUSAKHEL DISTRICT"
    }
    if(d == 95){
      pdf_text[42] <- "NOWSHERA CANTONMENT      36,669     23,338    13,304         27    85.91     92.06     73.75      51.85    4,523    2,715       4  10,432    2,532        7    2,346     1,456        1    35,024     1,645    29,586    24,429    1,295   12,671    22,939"
    }
    if(d == 101){
      pdf_text[18] <- "PESHAWAR DISTRICT       1,969,823 1,025,010   944,401    412   65.40   75.64 54.23 36.89    210,860 146,683      57 188,354 112,457      62 109,071 60,303      13   1,942,636 27,187 1,427,254 1,055,143 92,742 409,834    833,468"
      pdf_text[19] <- ""
      pdf_text[36] <- ""
      pdf_text[37] <- "PESHAWAR MUNICIPAL CORPORATION     1,893,344   980,155   912,789    400   64.40   74.82 53.20 36.75    203,124 140,482      54 172,784 105,498      61 99,811 54,952       13   1,871,793 21,551 1,365,711 1,005,226 88,898 388,333    786,379"
    }
    if(d == 106){
      pdf_text <- gsub("RAWALPINIDI", "RAWALPINDI", pdf_text)
    }
    if(d == 109){
      pdf_text[89] <- "SARGODHA MUNICIPAL CORPORATION        506,095 257,348 248,621            126    80.77   83.27    78.22           40.48 73,344 62,530              33 55,081 51,153                8   19,035    21,822          1 485,645          20,450         388,977      302,833       31,577        120,918      273,899"
      pdf_text[90] <- ""
    }
    if(d == 120){
      pdf_text[15] <- "NEW SUKKUR TALUKA        276,571      142,961     133,571              39       55.81   64.71   46.22             38.46 28,356      20,868                 1 18,119       9,837               5 8,331       3,014                 2 267,088          9,483         190,039        138,630       10,923           34,519        114,663"
      pdf_text[19] <- "SUKKUR MUNICIPAL CORPORATION (Part of New Sukkur Taluka)    268,942   139,070 129,838              34       55.16   64.00   45.63             41.18 27,322      20,006                 1 17,205       9,228               5 7,994       2,929                 2 260,080          8,862         184,377        134,287       10,476           32,413        110,997"
      pdf_text[20:21] <- ""
      pdf_text[49] <- "PANO AQIL CANTONMENT       24,094     17,885   6,209               -       90.14   96.01   67.03                 -    2,310     1,179                 -   11,195       810               -    958        395                 - 23,208             886          19,349         17,183          213           14,261         16,778"
      pdf_text[85] <- "SUKKUR CITY TALUKA         231,459     118,916     112,511              32       80.18   84.92   75.19             40.63   27,609    25,155                 2   27,115    24,456               6 17,408      9,857                 3 219,622         11,837         177,666        138,078       12,185           40,249        121,081"
      pdf_text[86] <- "SUKKUR MUNICIPAL CORPORATION (Part of Sukkur City Taluka)    231,459   118,916 112,511              32       80.18   84.92   75.19             40.63   27,609    25,155                 2   27,115    24,456               6 17,408      9,857                 3 219,622         11,837         177,666        138,078       12,185           40,249        121,081"
      pdf_text[87:88] <- ""
    }
    if(d == 121){
      pdf_text[25] <- "COL. SHER KILLI/NAWAN KILLI TC    26,214    13,125    13,089          -    67.58    81.93     53.54         -    3,426    2,498       -    2,598    1,145       -      824       373         -  26,213         1    19,216    14,263    1,829     5,460    12,487"
      pdf_text[26:27] <- ""
    }
    if(d == 122){
      pdf_text[72] <- "KHAWAZAKHELA TEHSIL      48,016    25,294     22,722           -     52.98      66.91     37.40       0.00     5,116     3,046         -      3,918    1,148       -      1,107       396        -     47,938          78     33,431    23,958    2,184      9,176     22,055"
      pdf_text[73] <- "KHAWAZA KHELA MC      48,016      25,294     22,722           -     52.98      66.91     37.40       0.00     5,116     3,046         -      3,918    1,148       -      1,107       396        -     47,938          78     33,431    23,958    2,184      9,176     22,055"
    }
    
    if(!(any(grepl(district_name, pdf_text)))){
      
      table_25 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          sublvl_01 = NA, sublvl_02 = NA, sublvl_03 = NA, summary_row = NA, area_name = NA, pop_all = NA, pop_male = NA, pop_fem = NA, pop_trans = NA,
          lit_all = NA, lit_male = NA, lit_fem = NA, lit_trans = NA, primary_male = NA, primary_fem = NA, primary_trans = NA, matric_male = NA, matric_fem = NA, matric_trans = NA,
          degree_male = NA, degree_fem = NA, degree_trans = NA, muslim = NA, non_muslim = NA, age_10_up = NA, age_18_up = NA, age_60_up = NA, working_10_up = NA, cnic_18_up = NA
        )
      
      natl_table_25 <- rbind(natl_table_25, table_25)
    } else {
      
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    
    pdf_trimmed[grep("SELECTED POPULATION STATISTICS", pdf_trimmed)] <- ""
    more_headers_starts <- grep("POPULATION CHARACTERISTICS", pdf_trimmed)
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}24\\s{6,}25", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*23\\s*24\\s*25", pdf_trimmed)
        if(length(more_headers_ends) ==0){
          more_headers_ends <- grep("\\s{25,}25", pdf_trimmed)
        }
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # on first pass, in some cases cells are not fully splitting, so need to clean up first
    pdf_correct <- gsub(", C", "C", pdf_trimmed)
    pdf_correct <- gsub("N0 ", "NO ", pdf_correct)
    pdf_correct <- gsub("######", " ------ ", pdf_correct)
    pdf_correct <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_correct, perl = T)
    pdf_correct <- gsub("(?<=\\s)-(?=\\s)", "  -  ", pdf_correct, perl = T)
    pdf_split <- strsplit(trimws(pdf_correct), "\\s{2,}")
    
    total_out <- tibble()
    for(x in 1:length(pdf_split)){
      
      row_extract <- pdf_split[[x]]

      row_out <- tibble(
        area_name = row_extract[1],
        summary_row = ifelse(area_name %in% administrative_areas, "YES", "NO"),
        pop_all = row_extract[2],
        pop_male = row_extract[3],
        pop_fem = row_extract[4],
        pop_trans = row_extract[5],
        lit_all = row_extract[6],
        lit_male = row_extract[7],
        lit_fem = row_extract[8],
        lit_trans = row_extract[9],
        primary_male = row_extract[10],
        primary_fem = row_extract[11],
        primary_trans = row_extract[12],
        matric_male = row_extract[13],
        matric_fem = row_extract[14],
        matric_trans = row_extract[15],
        degree_male = row_extract[16],
        degree_fem = row_extract[17],
        degree_trans = row_extract[18],
        muslim = row_extract[19],
        non_muslim = row_extract[20],
        age_10_up = row_extract[21],
        age_18_up = row_extract[22],
        age_60_up = row_extract[23],
        working_10_up = row_extract[24],
        cnic_18_up = row_extract[25]
      )
      
      total_out <- rbind(total_out, row_out)
    }    

    total_out <- total_out %>% mutate(
      id = row_number())
  
    #figure out subdivision parent-child relationships by relative positions
    subdivisions <- filter(total_out, summary_row == "YES")
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
    #
    total_out$child_05 <- ifelse(total_out$summary_row == "NO", "child", "parent")
    #
    table_25 <- left_join(total_out, subdivisions)
    #
    table_25$child_01 <- ifelse(!is.na(table_25$child_01), table_25$area_name, NA)
    table_25$child_02 <- ifelse(table_25$child_02 == "child" & is.na(table_25$child_01), table_25$area_name, NA)
    table_25$child_03 <- ifelse(table_25$child_03 == "child" & is.na(table_25$child_01) & is.na(table_25$child_02), table_25$area_name, NA)
    table_25$child_04 <- ifelse(table_25$child_04 == "child" & is.na(table_25$child_01) & is.na(table_25$child_02) & is.na(table_25$child_03), table_25$area_name, NA)
    table_25$child_05 <- ifelse(table_25$child_05 == "child" & is.na(table_25$child_01) & is.na(table_25$child_02) & is.na(table_25$child_03) & is.na(table_25$child_04), table_25$area_name, NA)
    #
    
    table_25 <- table_25 %>% fill(c(child_01, child_02, child_03, child_04)) %>%
      rename(
        district_name = child_01,
        sublvl_01 = child_02,
        sublvl_02 = child_03,
        sublvl_03 = child_04,
        sublvl_04 = child_05
      )
    
    table_25 <- table_25 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    sublvl_01, sublvl_02, sublvl_03, summary_row, everything()) %>%
      dplyr::select(-c(id, sublvl_04))
    
    # correct over-fills
    
    table_25$sublvl_02[table_25$sublvl_01 == table_25$area_name] <- NA
    table_25$sublvl_03[table_25$sublvl_01 == table_25$area_name] <- NA
    table_25$sublvl_03[table_25$sublvl_02 == table_25$area_name] <- NA

    # cleanup numerics
    table_25 <- cbind(
          table_25 %>% dplyr::select(1:8),
          table_25 %>% dplyr::select(9:32) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_25[is.na(table_25)] <- 0
    table_25$sublvl_01[table_25$sublvl_01 == 0] <- NA
    table_25$sublvl_02[table_25$sublvl_02 == 0] <- NA
    table_25$sublvl_03[table_25$sublvl_03 == 0] <- NA
    
    prov_path <- paste0("./processed_forms/", unique(table_25$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_25, paste0(dist_path, "table_25.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_25 <- rbind(natl_table_25, table_25)
    }   
  } # end else 
  
  # Process Table 26, Housing Status of Urban Localities ---

  if("26" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "26.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "26.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)  
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d %in% c(28, 86)){
      pdf_text <- gsub(district_name, "", pdf_text)
    }
    if(d == 18){
      pdf_text[54] <- "KHAIRPUR NATHAN SHAH TALUKA                         13,382      9,934             1,918           1,530       10,752         12,898     9,127          11,657       12,101      13,129           5.36"
      pdf_text[55] <- ""
    }
    if(d == 20){
      pdf_text[9] <- "DERA GHAZI KHAN DISTRICT"
      pdf_text[12] <-  "DERA GHAZI KHAN MUNICIPAL CORPORATION        52872          46,848            3,743         2,281          36,319            51,860        33,254         39,341        50,101       51,604            7.29"
      pdf_text[13] <- ""
    }
    if(d == 22){
      pdf_text[23] <- "FAISALABAD MUNICIPAL CORPORATION       499575     468626            25739         5210      289364        495489     428529      375744     492939      495815           6.30"
      pdf_text[24] <- ""
    }
    if(d == 30){
      pdf_text[8] <- "GUJRANWALA DISTRICT"
      pdf_text[10] <- "GUJRANWALA CITY TEHSIL           27354      20,306       6,537            511      25,821    27,016      21,532      19,122     26,614     27,103           6.74"
      pdf_text[11] <- ""
      pdf_text[31] <- "GUJRANWALA SADDAR TEHSIL       326362     296,296     23,554           6,512     297,975   323,478     283,855     263,640    320,528    322,975           6.52"
      pdf_text[32] <- ""
      pdf_text[49] <- "GUJRANWALA MUNICIPAL CORPORATION      305705     277,086     22,530           6,089     279,221   302,972     264,814     244,126    300,179    302,480           6.57"
      pdf_text[50] <- ""
    }
    if(d == 37){
      pdf_text[13] <- "HYDERABAD CANTONMENT (Part of Hyderabad City Taluka)     8344    7,948      251             145     8,089       8,261        7,744       8,155      8,228      8,314           5.67"
      pdf_text[14:16] <- ""
      pdf_text[31] <- "HYDERABAD MUNICIPAL CORPORATION (Part of Hyderabad City Taluka)     132,580       125,759    4,026   2,795  128,463      130,625      120,124     125,877    130,384    130,719           4.99"
      pdf_text[32:35] <- ""
      pdf_text[229] <- "HYDERABAD CANTONMENT (Part of Latifabad Taluka)                     5772                       5,553          132              87     5,349       5,679        5,389       5,524      5,690      5,748           6.69"
      pdf_text[230:231] <- ""
      pdf_text[240] <- "HYDERABAD MUNICIPAL CORPORATION (Part of Latifabad Taluka)                    135014        123,471         4,871           6,672  121,406      129,170      114,250     128,577    129,378    129,913           4.84"
      pdf_text[241:244] <- ""
      pdf_text[404] <- "HYDERABAD CANTONMENT (Part of Qasimabad Taluka)           739     673           53              13       732         729          721         731        735        736           5.91"
      pdf_text[405:406] <- ""
    }
    if(d == 38){
      pdf_text[9] <- "ISLAMABAD DISTRICT"
      pdf_text[12] <- "ISLAMABAD METROPOLITAN CORPORATION          167695      157,677            4,433           5,585      120,459        161,344     120,572      160,086       162,448      162,375           5.78"
      pdf_text[13:14] <- ""
    }
    if(d == 48){
      pdf_text[12] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Gulberg Sub-Division)    95,040     93,241        820        979      82,934    94,605      89,622     93,608    94,380     94,344           5.12"
      pdf_text[13] <- ""
      pdf_text[147] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Liaquatabad Sub-Division)       84,440    82,431      1,355          654      81,904    84,002      80,701     83,569    84,032     84,162           5.28"
      pdf_text[148] <- ""
      pdf_text[267] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of Nazimabad Sub-Division)    80,952    77,910      2,116          926      77,236    80,387      76,248     79,676    80,281     80,150           5.43"
      pdf_text[268] <- ""
      pdf_text[378] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of New Karachi Sub-Division)        148,193    143,743      2,600       1,850     138,156   147,380     140,548    145,276   146,872    147,652           5.81"  
      pdf_text[379] <- ""
      pdf_text[603] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (Part of North Nazimabad Sub-Division)           125,567    122,368      1,881      1,318     112,363   124,881     120,268    123,613   124,768    124,776           5.53"
      pdf_text[604] <- ""
    }
    if(d == 49){
      pdf_text[14] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Ferozabad Sub-Division)   140,111    136,223   2,080   1,808         124,109              139,226     133,382    137,815   138,772   139,342       5.33"
      pdf_text[15:19] <- ""
      pdf_text[223] <- "GULSHAN-E-IQBAL SUB-DIVISION     154,136      145,984    2,367      5,785    138,726     152,213      142,151    150,110   150,406   151,878       5.33"   
      pdf_text[224] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Gulshan-e-Iqbal Sub-Division)    115,317   108,305    2,098    4,914   105,095     113,660               104,920    111,630   112,382   113,653       5.48"
      pdf_text[225:230] <- ""
      pdf_text[378] <- "FAISAL CANTONMENT (Part of Gulshan-e-Iqbal Sub-Division)    38,819    37,679    269           871     33,631     38,553     37,231     38,480   38,024    38,225       4.89"
      pdf_text[379:380] <- ""
      pdf_text[443] <- "GULZAR-E-HIJRI SUB-DIVISION    117,373 105,153    6,368       5,852 102,279 110,949                   100,247    111,427   112,761   114,207       6.56"   
      pdf_text[444] <- ""
      pdf_text[445] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Gulzar-e-Hijri Sub-Division)      107,839    96,462    60,37    5,340    94,302 101,979                  92,669    102,484   103,725   104,826       6.59"   
      pdf_text[446:449] <- ""
      pdf_text[534] <- "MALIR CANTONMENT (Part of Gulzar-e-Hijri Sub-Division)           9,534                        8,691       331       512     7,977     8,970               7,578      8,943    9,036     9,381       6.13"    
      pdf_text[535:536] <- ""
      pdf_text[547] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (Part of Jamshed Quarters Sub-Division)           84,299            82,126      1,206         967       81,028        83,696              80,338     82,661   83,514    83,262       5.42"   
      pdf_text[548:553] <- ""
      pdf_text[666] <- "KARACHI CANTONMENT (Part of Jamshed Quarters Sub-Division)            2,264           2,196              55                  13      2,056      2,241      2,187      2,249    2,252     2,246       5.09"  
      pdf_text[667:669] <- ""
    }
    if(d == 50){
      pdf_text[15] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Aram Bagh Sub-Division)              25,068        24,337                 476            255      22,599          24,901        23,320             24,008         24,645           24,605                 4.99"
      pdf_text[16:18] <- ""
      pdf_text[56] <- "CLIFTON CANTONMENT (Part of Civil Lines Sub-Division)       46,394        43,918               1,137          1,339      26,977          46,091        43,797               45,971           45,538          45,795              5.21"
      pdf_text[57:58] <- ""
      pdf_text[101] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Civil Lines Sub-Division)            41,436        39,803                971            662       31,177          41,179        39,050               39,224           40,826          40,877              5.29"
      pdf_text[102:104] <- ""
      pdf_text[153] <- "KARACHI CANTONMENT (Part of Civil Lines Sub-Division)          516           512                  2              2          403             514           495                  496              505             507              4.69"
      pdf_text[154] <- ""
      pdf_text[159] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Garden Sub-Division)             67,198        65,225               1,256           717       62,472          66,745        64,252               65,807           66,627          66,637              5.32"
      pdf_text[160:162] <- ""
      pdf_text[268] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Lyari Sub-Division)           111,620       107,768               2,636          1,216     105,923         110,736       108,193            106,015        110,550          111,332                 5.81"
      pdf_text[269:271] <- ""
      pdf_text[384] <- "CLIFTON CANTONMENT (Part of Saddar Sub-Division)        9,866         9,469                233            164        6,128           9,818         9,213                9,663            9,690           9,820              5.19"
      pdf_text[385] <- ""
      pdf_text[396] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (Part of Saddar Sub-Division)                  7,049         6,850            136             63           6,752            7,003        6,687             6,877            6,833           6,954              5.04"
      pdf_text[397:399] <- ""
      pdf_text[410] <- "KARACHI CANTONMENT (Part of Sadar Sub-Division)            9,785         9,510            153            122           8,528            9,742        9,338             9,618            9,717           9,761              5.34"
      pdf_text[411] <- ""
    }
    if(d == 51){
      pdf_text[14] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Baldia Sub-Division)        128,305           122,536           3,713               2,056       73,050      124,606    120,124       126,503     126,821    126,528       6.40"
      pdf_text[15:19] <- ""
      pdf_text[164] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Harbour Sub-Division)      64,824        61,327          2,604                  893       33,022        64,101    61,600        61,899      63,744     63,575       5.95" 
      pdf_text[165:168] <- ""
      pdf_text[224] <- "MANORA CANTONMENT (Part of Harbour Sub-Division)                                         728          718           10            -             316          721        670           706         712        713       5.79"  
      pdf_text[225] <- ""
      pdf_text[229] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Manghopir Sub-Division)        89,249        80,446        4,923       3,880          68,103       83,319     68,549        87,472      86,945     85,833       6.10"      
      pdf_text[230:232] <- ""
      pdf_text[291] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Mauripur Sub-Division)   10,468      9,689       608          171           6,057       10,206      9,507        10,217      10,366     10,172       6.39"
      pdf_text[292:294] <- ""
      pdf_text[305] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Mominabad Sub-Division)      144,191     138,117     4,978       1,096          94,840      142,249    136,797       143,256     143,435    143,421       5.78" 
      pdf_text[306:311] <- ""
      pdf_text[532] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (Part of Orangi Sub-Division)     80,968        77,141        2,748                  1,079       73,809        79,724    77,194        79,429      80,068     80,692       6.29"
      pdf_text[533:536] <- ""
      pdf_text[621] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF S.I.T.E SUB-DIVISION)         61,796          59,228        1,742                    826       48,482        61,424    58,561        59,475      60,642     60,338       6.23"
      pdf_text[622:625] <- ""
    }
    if(d == 67){
      pdf_text[14] <- "CLIFTON CANTONMENT (Part of Korangi Sub-Division)             1,149           1,036                   12               101                 251              1,114          1,122                    1,146             1,110            1,143              5.60"
      pdf_text[15] <- ""
      pdf_text[18] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Korangi Sub-Division)                174,706         167,823                4,277             2,606             136,392            170,761        163,445                  172,148           172,706          171,818              6.05"
      pdf_text[19] <- ""
      pdf_text[165] <- "KORANGI CREEK CANTONMENT (Part of Korangi Sub-Division)               7,720           7,523                  154               43                7,662              7,672          7,550                    7,701             7,685            7,704              6.11"
      pdf_text[166] <- ""
      pdf_text[175] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Landhi Sub-Division)                 91,935          89,916                1,128              891               85,388             91,327         87,674                   91,478            91,334           91,757              5.86"
      pdf_text[176] <- ""
      pdf_text[279] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Model Colony Sub-Division)                 70,158          67,555                1,689              914               59,207             69,663         65,583                   69,719            69,600           69,772              5.40"
      pdf_text[280:281] <- ""
      pdf_text[360] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (Part of Shah Faisal Sub-Division)                 80,182          77,739                1,047             1,396              73,832             79,518         75,038                   79,481            79,490           79,452              5.51"
      pdf_text[361:362] <- ""
      pdf_text[438] <- "FAISAL CANTONMENT (Part of Shah Faisal Sub-Division)          12,082          11,738                  167              177               11,269             12,007         11,746                   12,041            11,985           12,051              5.56"
      pdf_text[439] <- ""
    }
    if(d == 69){
      pdf_text[74] <- "LAHORE METROPOLITAN CORPORATION (Part of Lahore Cantonment Tehsil)           82073    75,677         4,660     1,736           75,170     81,018     58,040     66,434   79,408    80,617           6.58" 
      pdf_text[75] <- ""
      pdf_text[251] <- "LAHORE METROPOLITAN CORPORATION (Part of Lahore City Tehsil)         573,650        537,508        21,423    14,719      524,113    567,474    511,524    506,835   562,345   567,852           6.25"
      pdf_text[252:254] <- ""
      pdf_text[663] <-  "LAHORE METROPOLITAN CORPORATION (Part of Model Town Tehsil)          427,790         406,280        11,674     9,836      380,030    423,025    354,053    369,059   418,545   422,514           6.11"
      pdf_text[664:665] <- ""
      pdf_text[907] <- "LAHORE METROPOLITAN CORPORATION (Part of Raiwind Tehsil)                                                    130446   111,493       15,320         3,633    119,766     128,121     70,591     93,778   123,836   125,953           6.34"
      pdf_text[908] <- ""
      pdf_text[963] <- "LAHORE METROPOLITAN CORPORATION (Part of Shalimar Tehsil)     354717     337,471           10,926     6,320        308,542    351,990    310,218    306,655    347,010   351,255           6.35"
      pdf_text[964:965] <- ""
    }
    if(d == 71){
      pdf_text[35] <- "LARKANA MUNICIPAL CORPORATION                            84,552     62,169        13,116        9,267       82,248         80,530     68,179       78,803      82,428        80,933           5.69"
      pdf_text[36] <- ""
    }
    if(d == 76){
      pdf_text[11] <- "LOWER DIR DISTRICT         4,538     3,396         481         661     3,377     4,482       2,016      4,285     4,430      4,310     8.53"
    }
    if(d == 78){
      pdf_text[14] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Airport Sub-Division)             16,828 16,172            411      245             15,409         16,585 15,779 16,532               16,705 16,783                5.85"
      pdf_text[15:16] <- ""
      pdf_text[37] <- "FAISAL CANTONMENT (Part of Airport Sub-Division)     4,625    4,339           195      91                3,779        4,589      4,283     4,558         4,558      4,611            5.68"
      pdf_text[38] <- ""
      pdf_text[45] <- "MALIR CANTONMENT (Part of Airport Sub-Division)      10,130    9,558           160     412                9,054        9,889      9,052     9,461         9,835      9,880            5.89"
      pdf_text[46] <- ""
      pdf_text[46] <- ""
      pdf_text[58] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Ibrahim Hydri Sub-Division)            99,546   92,846        4,646    2,054               94,838       98,238     94,277   97,598         97,743     98,541            6.41"
      pdf_text[59:60] <- ""
      pdf_text[146] <- "KORANGI CREEK CANTONMENT (Part of Ibrahim Hydri Sub-Division)        1,133    1,113            7       13                1,113        1,132      1,099    1,123          1,125      1,112            5.24"
      pdf_text[147:148] <- ""
      pdf_text[152] <- "DISTRICT MUNICIPAL CORPORATION MALIR (Part of Murad Memon Sub-Division)            37,102   33,833        1,958    1,311               32,803       36,556     33,852   36,709         36,685     36,871            5.18"
      pdf_text[153:154] <- ""
      pdf_text[193] <- "MALIR CANTONMENT (Part of Murad Memon Sub-Division)        949      909            33       7                  876          454        115       931           934        946            6.14"
      pdf_text[194] <- ""
    }
    if(d == 104){
      pdf_text[11] <- "RAHIM YAR KHAN DISTRICT"
      pdf_text[12] <- "RAHIM YAR KHAN DISTRICT     163506    151,020         7,632   4,854        129,233      159,901   113,745   120,230        156,853   158,151             6.21"
    }
    if(d == 106){
      pdf_text <- gsub("RAWALPINIDI", "RAWALPINDI", pdf_text)
    }
    if(d == 115){
      pdf_text[99] <- "SIALKOT MUNICIPAL CORPORATION                       91710  86,411         3,214         2,085     84,488       91,058      82,384     83,965      90,142    90,038           6.35"
      pdf_text[100] <- ""
    }
    if(d == 120){
      pdf_text[17] <- "SUKKUR MUNICIPAL CORPORATION (Part of New Sukkur Taluka)           45,351     29,941        6,632          8,778      42,241        41,022       25,966      36,256      40,710 40,866             5.80"
      pdf_text[18:19] <- ""
      pdf_text[84] <- "SUKKUR MUNICIPAL CORPORATION (Part of Sukkur City Taluka)           41,436     38,614         1,850           972       39,351       40,509       37,875      39,632      41,060   41,085           5.46"
      pdf_text[85:86] <- ""
    }


    if(!(any(grepl(district_name, pdf_text)))){
      
      table_26 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          sublvl_01 = NA, sublvl_02 = NA, sublvl_03 = NA, summary_row = NA, area_name = NA, 
          total_housing = NA, pacca_housing = NA, semi_pacca_housing = NA, kacha_housing = NA, 
          potable_water = NA, electricity = NA, gas = NA, kitchen = NA, bath_room = NA, latrine = NA, avg_hh_size = NA
          
        )
      
      natl_table_26 <- rbind(natl_table_26, table_26)
    } else {
      
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    
    pdf_trimmed[grep("SELECTED HOUSING STATISTICS", pdf_trimmed)] <- ""
    more_headers_starts <- grep("HOUSING CHARACTERISTICS", pdf_trimmed)
    if(length(more_headers_starts) > 0){
      more_headers_ends <- grep("\\s{25,}11\\s{6,}12", pdf_trimmed)
      if(length(more_headers_ends) == 0){
        more_headers_ends <- grep("\\s*10\\s*11\\s*12", pdf_trimmed)
      }
      more_headers <- list()
      for(j in 1:length(more_headers_starts)){
        headers_out <- more_headers_starts[j]:more_headers_ends[j]
        more_headers <- c(more_headers, headers_out)
      }
      more_headers <- unlist(more_headers)
      
    pdf_trimmed <- pdf_trimmed[- more_headers]
    } else {}
    
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    # on first pass, in some cases cells are not fully splitting, so need to clean up first
    pdf_correct <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_correct <- gsub("(?<=\\s)-(?=\\s)", "  -  ", pdf_correct, perl = T)
    pdf_split <- strsplit(trimws(pdf_correct), "\\s{2,}")
    
    total_out <- tibble()
    for(x in 1:length(pdf_split)){
      
      row_extract <- pdf_split[[x]]

      row_out <- tibble(
        area_name = row_extract[1],
        summary_row = ifelse(area_name %in% administrative_areas, "YES", "NO"),        
        total_housing = row_extract[2],
        pacca_housing = row_extract[3],
        semi_pacca_housing = row_extract[4],
        kacha_housing = row_extract[5],
        potable_water = row_extract[6],
        electricity = row_extract[7],
        gas = row_extract[8],
        kitchen = row_extract[9],
        bath_room = row_extract[10],
        latrine = row_extract[11],
        avg_hh_size = row_extract[12]
      )
      
      total_out <- rbind(total_out, row_out)
    }    

    total_out <- total_out %>% mutate(
      id = row_number())
    
    # note that in cases where PBS incorrectly formatted a summary row, the bold text search isn't capturing rows
    # so need to double-check this further against a master list of administrative areas
  
    #figure out subdivision parent-child relationships by relative positions
    subdivisions <- filter(total_out, summary_row == "YES")
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
    #
    total_out$child_05 <- ifelse(total_out$summary_row == "NO", "child", "parent")
    #
    table_26 <- left_join(total_out, subdivisions)
    #
    table_26$child_01 <- ifelse(!is.na(table_26$child_01), table_26$area_name, NA)
    table_26$child_02 <- ifelse(table_26$child_02 == "child" & is.na(table_26$child_01), table_26$area_name, NA)
    table_26$child_03 <- ifelse(table_26$child_03 == "child" & is.na(table_26$child_01) & is.na(table_26$child_02), table_26$area_name, NA)
    table_26$child_04 <- ifelse(table_26$child_04 == "child" & is.na(table_26$child_01) & is.na(table_26$child_02) & is.na(table_26$child_03), table_26$area_name, NA)
    table_26$child_05 <- ifelse(table_26$child_05 == "child" & is.na(table_26$child_01) & is.na(table_26$child_02) & is.na(table_26$child_03) & is.na(table_26$child_04), table_26$area_name, NA)
    #
    
    table_26 <- table_26 %>% fill(c(child_01, child_02, child_03, child_04)) %>%
      rename(
        district_name = child_01,
        sublvl_01 = child_02,
        sublvl_02 = child_03,
        sublvl_03 = child_04,
        sublvl_04 = child_05
      )
    
    table_26 <- table_26 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    sublvl_01, sublvl_02, sublvl_03, summary_row, everything()) %>%
      dplyr::select(-c(id, sublvl_04))
    
    # correct over-fills
    
    table_26$sublvl_02[table_26$sublvl_01 == table_26$area_name] <- NA
    table_26$sublvl_03[table_26$sublvl_01 == table_26$area_name] <- NA
    table_26$sublvl_03[table_26$sublvl_02 == table_26$area_name] <- NA

    # cleanup numerics
    table_26 <- cbind(
          table_26 %>% dplyr::select(1:8),
          table_26 %>% dplyr::select(9:19) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_26[is.na(table_26)] <- 0
    table_26$sublvl_01[table_26$sublvl_01 == 0] <- NA
    table_26$sublvl_02[table_26$sublvl_02 == 0] <- NA
    table_26$sublvl_03[table_26$sublvl_03 == 0] <- NA
    
    prov_path <- paste0("./processed_forms/", unique(table_26$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_26, paste0(dist_path, "table_26.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_26 <- rbind(natl_table_26, table_26)
    }
  } # end else 
    
    
  # Process Table 27, Types of Housing ---
  if("27" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "27.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "27.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)  
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 88){
      pdf_text[5] <- "MUSAKHEL DISTRICT"
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]

    area_types <- c("OVERALL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    housing_types <- c("REGULAR", "INSTITUTIONAL", "HOMELESS", "TOTAL")

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    area_starts <- as.integer(grep("OVERALL", area_split) - 1)
    table_27 <- tibble()

    for(k in 1:length(area_starts)){
      area_end <- ifelse(k != length(area_starts), (as.integer(area_starts[k+1]) - 1), length(area_split))
      area_extract <- area_split[area_starts[k]:area_end]
      # check and confirm if all area types are present
      missing_areas <- area_types[!(area_types %in% area_extract)]
      total_out <- tibble()
      
      for(a in 1:length(area_types)){
        sub_starts <- as.integer(grep("TOTAL", area_extract)) - 4
        if("RURAL" %in% missing_areas){
          sub_starts <- c(sub_starts[1], NA, sub_starts[2]) # if rural is missing, it throws off the following
        }
        for(g in 1:length(gender_types)){
          for(h in 1:length(housing_types)){
            
          # check to make sure rows for all areas are present
          
          
          row_out <- tibble(
                  area_name = area_extract[[1]][1],
                  area_type = area_types[a],
                  gender_type = gender_types[g],
                  total_by_gender = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[sub_starts[a]+4]][g+1]),
                  percent_by_gender = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[sub_starts[a]+5]][g+1]),
                  housing_type = housing_types[h],
                  total_by_housing_type = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[sub_starts[a]+h]][6]),
                  percent_by_housing_type = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[sub_starts[a]+h]][7]),
                  count = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[sub_starts[a]+h]][g+1])
                  )
        
          total_out <- rbind(total_out, row_out)
          }
        }
      }

            
      table_27 <- rbind(table_27, total_out)        
    }

      # cleanup numerics

    table_27 <- cbind(
          table_27 %>% dplyr::select(1:3),
          table_27 %>% dplyr::select(total_by_gender, percent_by_gender) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_27 %>% dplyr::select(6),
          table_27 %>% dplyr::select(total_by_housing_type, percent_by_housing_type, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_27$count[is.na(table_27$count) & !(table_27$area_type %in% missing_areas)] <- 0
    table_27$total_by_gender[is.na(table_27$total_by_gender) & !(table_27$area_type %in% missing_areas)] <- 0
    table_27$percent_by_gender[is.na(table_27$percent_by_gender) & !(table_27$area_type %in% missing_areas)] <- 0
    table_27$area_type[table_27$area_type == "OVERALL"] <- "TOTAL"

    table_27 <- table_27 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_27$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_27, paste0(dist_path, "table_27.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_27 <- rbind(natl_table_27, table_27)
    
  } # end else 
      
  # Process Table 28, Household Size ---
  if("28" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "28.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "28.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text[9] <- "KOHISTAN DISTRICT"
    }
    if(d == 130){
      pdf_text[16] <- "UMER KOT DISTRICT"
    }
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    area_starts <- as.integer(grep("TOTAL", area_split) - 1)
    area_types <- c("TOTAL", "RURAL", "URBAN")
    household_sizes <- c("TOTAL", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10 AND UP")
    table_28 <- tibble()
    
    for(k in 1:length(area_starts)){
      area_start <- area_starts[k]
      area_end <- ifelse(k != length(area_starts), (as.integer(area_starts[k+1]) - 1), length(area_split))
      area_extract <- area_split[area_start:area_end]
      missing_areas <- area_types[!(area_types %in% unlist(area_extract))]
      total_out <- tibble()
      for(a in 1:3){
        for(h in 1:length(household_sizes)){
                  row_out <- tibble(
                  area_name = area_extract[[1]][1],
                  area_type = area_types[a],
                  total_population = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[grep(area_types[a], area_extract)]][13]),
                  household_size = ifelse(area_types[a] %in% missing_areas, NA, household_sizes[h]),
                  count = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[grep(area_types[a], area_extract)]][h+1])
                  )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
      
      table_28 <- rbind(table_28, total_out)  
    }

    # cleanup numerics

    table_28 <- cbind(
          table_28 %>% dplyr::select(1:2),
          table_28 %>% dplyr::select(total_population) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_28 %>% dplyr::select(household_size),
          table_28 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_28$count[is.na(table_28$count) & !(table_28$area_type %in% missing_areas)] <- 0
    table_28$total_population[is.na(table_28$total_population) & !(table_28$area_type %in% missing_areas)] <- 0

    table_28 <- table_28 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_28$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_28, paste0(dist_path, "table_28.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_28 <- rbind(natl_table_28, table_28)
    
  } # end else 
    
  # Process Table 29, Housing Unit Size ---
    if("29" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "29.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "29.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    if(d == 27){
      pdf_text <- c(pdf_text[1:8], "FR PESHAWAR", pdf_text[9:50])
    }
    if(d == 30){
      pdf_text <- c(pdf_text[1:7], "GUJRANWALA DISTRICT", pdf_text[8:49])
    }
    if(d == 34){
      pdf_text <- c(pdf_text[1:8], "HANGU DISTRICT", pdf_text[9:50])
    }
    if(d == 41){
      pdf_text[38] <- ""
    }
    if(d == 65){
      pdf_text[9] <- "KOHISTAN DISTRICT"
      pdf_text[20] <- ""
      pdf_text[21] <- "10 PERSONS AND ABOVE                 3,562      9,884      6,175     3,998       1,243       612     378       170           625    26,647   26.80       2.9"
      pdf_text[24] <- ""
      pdf_text[25] <- "PERSONS PER HOUSING UNIT          6.3         7.4        7.9       8.3         8.4       8.7       8.1       9.0       7.8"
      pdf_text[26] <- ""
      pdf_text[37] <- ""
      pdf_text[38] <- "10 PERSONS AND ABOVE                  3562       9884       6175      3998         1243      612      378       170          625    26,647    26.80       2.9"
      pdf_text[41] <- ""
      pdf_text[42] <- "PERSONS PER HOUSING UNIT                     6.3         7.4        7.9       8.3         8.4       8.7       8.1       9.0       7.8"
      pdf_text[43] <- ""
      pdf_text[pdf_text != ""]
      pdf_text <- c(pdf_text,
                    "URBAN", 
                    rep("-    -    -    -    -    -    -    -    -    -    -    -    -", 13)
      )
    }
    if(d == 73){
      pdf_text[8] <- "LAYYAH DISTRICT"
    }
    if(d == 81){
      pdf_text[18] <- ""
      pdf_text[19] <- "10 PERSONS AND ABOVE                  2,374      11,400      17,525      16,968        9,515      5,837      2308       1439         1583      68,949      22.38         3.92"
      pdf_text[22] <- "PERSONS PER HOUSING UNIT    5.18      6.59        7.93        9.38        10.81      12.00      12.68      13.16        14.22       7.57"
      pdf_text[23] <- ""
      pdf_text[34] <- ""
      pdf_text[35] <- "10 PERSONS AND ABOVE                  1,975       9,449      14,706      14,105        7,715      4,701      1829       1143         1236      56,859      22.79         3.89"
      pdf_text[38] <- "PERSONS PER HOUSING UNIT    5.20     6.63        8.01        9.48        10.99      12.25      13.15      13.47        14.72       7.62"
      pdf_text[39] <- ""
      pdf_text[50] <- "10 PERSONS AND ABOVE    399       1,951       2,819       2,863        1,800      1,136        479       296           347     12,090      20.64         4.04"
      pdf_text[53] <- "PERSONS PER HOUSING UNIT   5.06    6.38        7.56        8.94        10.17      11.15      11.23      12.11        12.71       7.36"
      pdf_text[54] <- ""
    }
    if(d == 100){
      pdf_text[8] <- "PANJGUR DISTRICT"
    }
    if(d == 118){
      pdf_text[7] <- "SOUTH WAZIRISTAN AGENCY"
    }
    if(d == 133){
      pdf_text[9] <- "WASHUK DISTRICT"
    }
    
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    household_sizes <- c("1 PERSON", "2 PERSONS", "3 PERSONS", "4 PERSONS", "5 PERSONS", "6 PERSONS", "7 PERSONS", "8 PERSONS", "9 PERSONS", "10 PERSONS AND ABOVE")
    number_of_rooms <- c(1:8, "9 AND MORE")
    pdf_pad <- gsub(" P", "P", pdf_trimmed)
    pdf_pad <- gsub(" A", "A", pdf_pad)
    pdf_pad <- gsub(" H", "H", pdf_pad)
    pdf_pad <- gsub(" U", "U", pdf_pad)
    pdf_pad <- gsub(" ", "  ", pdf_pad) # had to pad this to ensure all columns properly split
    area_split <- strsplit(trimws(pdf_pad), "\\s{2,}")
    
    # NEED TO CORRECT TO ACCOUNT FOR CASE IN CHINIOT WHERE THEY OMMITTED THE 'URBAN' SECTION HEADER AND MESSED UP ROWS
    if(d == 16){
      area_split <- c(area_split[1:30], "URBAN", area_split[31:44])
      area_split <- area_split[-c(30,45)]
    }
    #missing_areas <- area_types[!(area_types %in% unlist(area_split))]
    table_29 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()
      for(n in 1:length(number_of_rooms)){
        for(c in 1:length(household_sizes)){
                  row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  household_sizes = household_sizes[c],
                  number_of_rooms = number_of_rooms[n],
                  total_housing_units_by_hh_size = area_extract[[c+1]][11],
                  percent_of_total_units_by_hh_size = area_extract[[c+1]][12],
                  rooms_per_housing_unit = area_extract[[c+1]][13],
                  total_housing_units_by_room_size = area_extract[[12]][n+1],
                  percent_of_total_units_by_room_size = area_extract[[13]][n+1],
                  persons_per_housing_unit = area_extract[[14]][n+1],
                  count = area_extract[[c+1]][n+1]
                  )

        total_out <- rbind(total_out, row_out)
              
            }
          }

      table_29 <- rbind(table_29, total_out)  
    }        

    # cleanup numerics
    table_29 <- cbind(
          table_29 %>% dplyr::select(1:4),
          table_29 %>% dplyr::select(5:11) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_29$count[is.na(table_29$count)] <- 0
    table_29$total_housing_units_by_hh_size[is.na(table_29$total_housing_units_by_hh_size)] <- 0
    table_29$percent_of_total_units_by_hh_size[is.na(table_29$percent_of_total_units_by_hh_size)] <- 0
    table_29$rooms_per_housing_unit[is.na(table_29$rooms_per_housing_unit)] <- 0
    table_29$total_housing_units_by_room_size[is.na(table_29$total_housing_units_by_room_size)] <- 0
    table_29$percent_of_total_units_by_room_size[is.na(table_29$percent_of_total_units_by_room_size)] <- 0
    table_29$persons_per_housing_unit[is.na(table_29$persons_per_housing_unit)] <- 0
    if(d == 65){
      table_29$count[table_29$area_type == "URBAN"] <- NA
      table_29$total_housing_units_by_hh_size[table_29$area_type == "URBAN"] <- NA
      table_29$percent_of_total_units_by_hh_size[table_29$area_type == "URBAN"] <- NA
      table_29$rooms_per_housing_unit[table_29$area_type == "URBAN"] <- NA
      table_29$total_housing_units_by_room_size[table_29$area_type == "URBAN"] <- NA
      table_29$percent_of_total_units_by_room_size[table_29$area_type == "URBAN"] <- NA
      table_29$persons_per_housing_unit[table_29$area_type == "URBAN"] <- NA
    }   
    
    
    table_29 <- table_29 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_29$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_29, paste0(dist_path, "table_29.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_29 <- rbind(natl_table_29, table_29)
    
  } # end else 

  # Process Table 30, Housing Unit Size by Ownership Status ---
    if("30" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "30.pdf"))
    } else {

    # load the pdf file
    # Nowshera district file was for Maneshra district, no Nowshera data
    if(d == 95){
      table_30 <- tibble(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name) %>%
        mutate(
          area_name = NA, are_type = NA, ownership_status = NA, number_of_rooms = NA, total_housing_units_by_ownership = NA, 
          percent_of_total_units_by_ownership = NA, total_housing_units_by_size = NA, percent_of_total_units_by_size = NA, count = NA
        )
    } else {
      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "30.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text[6] <- "KOHISTAN DISTRICT"
      pdf_text <- c(pdf_text,
                    "URBAN",
                    rep("-   -    -    -    -    -    -    -    -    -    -   -", 5)
      )
    }
    if(d == 104){
      pdf_text <- c(pdf_text[1:6], "OVERALL", pdf_text[7:11], "RURAL", pdf_text[12:16], "URBAN", pdf_text[17:21])
    }
    
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    ownership_types <- c("OWNED", "RENTED", "RENT FREE")
    number_of_rooms <- c(1:9)
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_30 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()
      for(n in 1:length(number_of_rooms)){
        for(o in 1:length(ownership_types)){
                  row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[o],
                  number_of_rooms = number_of_rooms[n],
                  total_housing_units_by_ownership = area_extract[[o+1]][11],
                  percent_of_total_units_by_ownership = area_extract[[o+1]][12],
                  total_housing_units_by_size = area_extract[[5]][n+1],
                  percent_of_total_units_by_size = area_extract[[6]][n+1],
                  count = area_extract[[o+1]][n+1]
                  )

        total_out <- rbind(total_out, row_out)
              
            }
          }

      table_30 <- rbind(table_30, total_out)  
    }        

    # cleanup numerics
    table_30 <- cbind(
          table_30 %>% dplyr::select(1:4),
          table_30 %>% dplyr::select(5:9) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_30$count[is.na(table_30$count)] <- 0
    table_30$total_housing_units_by_ownership[is.na(table_30$total_housing_units_by_ownership)] <- 0
    table_30$percent_of_total_units_by_ownership[is.na(table_30$percent_of_total_units_by_ownership)] <- 0
    table_30$total_housing_units_by_size[is.na(table_30$total_housing_units_by_size)] <- 0
    table_30$percent_of_total_units_by_size[is.na(table_30$percent_of_total_units_by_size)] <- 0

    if(d == 65){
    table_30$count[table_30$area_type == "URBAN"] <- NA
    table_30$total_housing_units_by_ownership[table_30$area_type == "URBAN"] <- NA
    table_30$percent_of_total_units_by_ownership[table_30$area_type == "URBAN"] <- NA
    table_30$total_housing_units_by_size[table_30$area_type == "URBAN"] <- NA
    table_30$percent_of_total_units_by_size[table_30$area_type == "URBAN"] <- NA
    }
    table_30 <- table_30 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_30$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_30, paste0(dist_path, "table_30.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_30 <- rbind(natl_table_30, table_30)
    }
  } # end else 
  
  # Process Table 31, Housing Ownership Count by Gender ---
    
  if("31" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "31.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "31.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 4){
      pdf_text[9] <- "GOLARCHI (S.F.RAHU) TALUKA            56,190    54,475    1,715        -    47,276    45,740    1,536        -    8,914     8,735          179         -"
      pdf_text[10] <- ""
    }
    if(d == 24){
      pdf_text[8] <- "TRIBAL AREA ADJ. DERA ISMAIL KHAN DISTRICT                    6,536      6,327       209        -     6,536  6,327       209         -         -          -            -        -"
      pdf_text[9] <- ""
    }
    if(d == 26){
      pdf_text[5] <- "FR LAKKI MARWAT"
    }
    if(d == 38){
      pdf_text[4] <- ""
    }
    if(d == 52){
      pdf_text[8] <- "BANDA DAUD SHAH TEHSIL             15,774    13,967      1,807        -     15,774    13,967    1,807        -          -         -               -            -"
      pdf_text[9] <- ""
      pdf_text[10] <- "KARAK TEHSIL                      29,515    26,422      3,093        -     25,091    22,351    2,740        -      4,424     4,071             353            -"
    }
    if(d == 65){
      pdf_text[6] <- ""
      pdf_text[8] <- "KOHISTAN DISTRICT     95,610   93,489     2,121           -      95,610   93,489     2,121         -         -        -        -        -"
    }
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    if(length(header_end) > 1){
       pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end[[2]])])
    } else {
      pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    }
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    gender_types <- c("ALL SEXES", "MALE", "FEMALE", "TRANSGENDER")
    table_31 <- tibble()
    
    # break down each administrative area
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    area_cols <- list(c(2:5), c(6:9), c(10:13))
    
    for(a in 1:length(area_split)){
      total_out <- tibble()
      for(k in 1:length(area_types)){
        area_extract <- area_split[[a]][area_cols[[k]]]
        for(g in 1:length(gender_types)){
              row_out <- tibble(
              area_name = area_split[[a]][1],
              area_type = area_types[k],
              gender = gender_types[g],
              count = area_extract[g]
              )
              
              total_out <- rbind(total_out, row_out)
          }
      }

    table_31 <- rbind(table_31, total_out)  
    }

    # cleanup numerics
    table_31 <- cbind(table_31 %>% dplyr::select(1:3),
          table_31 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_31$count[is.na(table_31$count)] <- 0
    if(d == 65){
      table_31$count[table_31$area_type == "URBAN"] <- NA
    }
    
    table_31 <- table_31 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_31$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_31, paste0(dist_path, "table_31.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_31 <- rbind(natl_table_31, table_31)
    
  } # end else 
      
  # Process Table 32, Housing Unit Size by Construction Duration ---
    if("32" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "32.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "32.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text[7] <- "KOHISTAN DISTRICT"
      pdf_text <- c(pdf_text,
                    "URBAN",
                    rep("-    -   -   -    -    -    -    -    -    -    -   -", 7))
    }
    if(d == 81){
      pdf_text[10] <- "UNDER CONSTRUCTION     653          1,014        802        442         175          82      50         32         31      3,281        1.32"
      pdf_text[18] <- "UNDER CONSTRUCTION     544            819        636        367         137          68      37         24         25      2,657        1.28"
    }
    
    # remove headers and footers from document; these are not consistent across district
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    # in at least some cases, a blank row is throwing off the following
    pdf_trimmed <- pdf_trimmed[lengths(pdf_trimmed) > 0L]
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("TOTAL", "RURAL", "URBAN")
    construction_periods <- c("UNDER CONSTRUCTION", "LESS THAN 5 YEARS", "5-10 YEARS", "11-50 YEARS", "OVER 50 YEARS")
    number_of_rooms <- c(1:8, "9 AND MORE")
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_32 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      total_out <- tibble()
      for(n in 1:length(number_of_rooms)){
        for(c in 1:length(construction_periods)){
                  row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_duration = construction_periods[c],
                  number_of_rooms = number_of_rooms[n],
                  total_housing_units_by_duration = area_extract[[c+1]][11],
                  percent_of_total_units_by_duration = area_extract[[c+1]][12],
                  total_housing_units_by_size = area_extract[[7]][n+1],
                  percent_of_total_units_by_size = area_extract[[8]][n+1],
                  count = area_extract[[c+1]][n+1]
                  )

        total_out <- rbind(total_out, row_out)
              
            }
          }

      table_32 <- rbind(table_32, total_out)  
    }        

    # cleanup numerics
    table_32 <- cbind(
          table_32 %>% dplyr::select(1:4),
          table_32 %>% dplyr::select(5:9) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_32$count[is.na(table_32$count)] <- 0
    table_32$total_housing_units_by_duration[is.na(table_32$total_housing_units_by_duration)] <- 0
    table_32$percent_of_total_units_by_duration[is.na(table_32$percent_of_total_units_by_duration)] <- 0
    table_32$total_housing_units_by_size[is.na(table_32$total_housing_units_by_size)] <- 0
    table_32$percent_of_total_units_by_size[is.na(table_32$percent_of_total_units_by_size)] <- 0

    if(d == 65){
    table_32$count[table_32$area_type == "URBAN"] <- NA
    table_32$total_housing_units_by_duration[table_32$area_type == "URBAN"] <- NA
    table_32$percent_of_total_units_by_duration[table_32$area_type == "URBAN"] <- NA
    table_32$total_housing_units_by_size[table_32$area_type == "URBAN"] <- NA
    table_32$percent_of_total_units_by_size[table_32$area_type == "URBAN"] <- NA
    }
    
    table_32 <- table_32 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_32$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_32, paste0(dist_path, "table_32.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_32 <- rbind(natl_table_32, table_32)
    
  } # end else 
  
  # Process Table 33, Housing Unit by Construction Material ---
  if("33" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "33.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "33.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "WALL",
                    rep("--    -    -    -    -    -", 4),
                    "ROOF", rep("--    -    -    -    -    -", 7))
    }
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    ownership_types <- c("OWNED", "RENTED", "RENT FREE")
    wall_material <- c("BAKED BRICKS/BLOCKS/STONES", "UNBAKED BRICKS/MUD", "WOOD/BAMBOO", "OTHERS")
    roof_material <- c("RCC/RBC", "CEMENT/IRON SHEETS", "GARDER/T.IRON", "WOOD/BAMBOO", "OTHERS")

    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    if(d == 33){
      pdf_trimmed <- gsub("OVERALL", "ALL LOCALITIES", pdf_trimmed)
    }

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_33 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      wall_start <- grep("WALL", area_extract)
      roof_start <- grep("ROOF", area_extract)
      wall_extract <- area_extract[wall_start:(wall_start+(as.integer(length(wall_material))))]
      roof_extract <- area_extract[roof_start:(roof_start+(as.integer(length(roof_material))))]
      total_out <- tibble()
      for(h in 1:length(ownership_types)){
        for(w in 1:length(wall_material)){
          
          wall_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[13]][h+1],
                  percent_by_tenure = area_extract[[14]][h+1],
                  housing_component = "WALL",
                  housing_material = wall_material[w],
                  total_by_material = wall_extract[[w+1]][5],
                  percent_by_material = wall_extract[[w+1]][6],
                  count = wall_extract[[w+1]][h+1]
                  )
          
        total_out <- rbind(total_out, wall_row_out) 
        
        }

        for(r in 1:length(roof_material)){
          
          roof_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[13]][h+1],
                  percent_by_tenure = area_extract[[14]][h+1],
                  housing_component = "ROOF",
                  housing_material = roof_material[r],
                  total_by_material = roof_extract[[r+1]][5],
                  percent_by_material = roof_extract[[r+1]][6],
                  count = roof_extract[[r+1]][h+1]
                  )
          
        total_out <- rbind(total_out, roof_row_out) 
        
        }
      }
      
      table_33 <- rbind(table_33, total_out)  
    }

    # cleanup numerics

    table_33 <- cbind(
          table_33 %>% dplyr::select(1:3),
          table_33 %>% dplyr::select(total_by_tenure, percent_by_tenure) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_33 %>% dplyr::select(6:7),
          table_33 %>% dplyr::select(total_by_material, percent_by_material, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_33$count[is.na(table_33$count)] <- 0
    table_33$area_type[table_33$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_33$count[table_33$area_type == "URBAN"] <- NA
    }

    table_33 <- table_33 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_33$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_33, paste0(dist_path, "table_33.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_33 <- rbind(natl_table_33, table_33)
    
  } # end else 
    
  # Process Table 34, Housing Unit by Material and Construction Duration ---

  if("34" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "34.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "34.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    
    if(d == 21){
      pdf_text <- c(pdf_text[1:6], "DERA ISMAIL KHAN DISTRICT", pdf_text[7:48])
    }

    if(d == 22){
      pdf_text <- c(pdf_text[1:22], pdf_text[24], "OUTER WALLS", pdf_text[25:36], pdf_text[38], "OUTER WALLS", pdf_text[39:50])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "OUTER WALLS",
                    rep("--    -    -    -    -    -   -    -", 4),
                    "ROOF", rep("--    -    -    -    -    -   -    -", 7))
    }
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- gsub("KILLAH ABDULLAH DISTRICT", "KILLA ABDULLAH DISTRICT", pdf_text)
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    if(d == 33){
      pdf_trimmed <- gsub("OVERALL", "ALL LOCALITIES", pdf_trimmed)
    }
    
        
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    construction_ages <- c("UNDER CONSTRUCTION", "LESS THAN 5 YEARS", "5-10 YEARS", "11-50 YEARS", "OVER 50 YEARS")
    wall_material <- c("BAKED BRICKS/BLOCKS/STONES", "UNBAKED BRICKS/MUD", "WOOD/BAMBOO", "OTHERS")
    roof_material <- c("RCC/RBC", "CEMENT/IRON SHEETS", "GARDER/T.IRON", "WOOD/BAMBOO", "OTHERS")
  
    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_34 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      wall_start <- grep("OUTER WALLS", area_extract)
      roof_start <- grep("ROOF", area_extract)
      wall_extract <- area_extract[wall_start:(wall_start+(as.integer(length(wall_material))))]
      roof_extract <- area_extract[roof_start:(roof_start+(as.integer(length(roof_material))))]
      total_out <- tibble()
      for(c in 1:length(construction_ages)){
        for(w in 1:length(wall_material)){
          
          wall_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[13]][c+1],
                  percent_by_age = area_extract[[14]][c+1],
                  housing_component = "WALL",
                  housing_material = wall_material[w],
                  total_by_material = wall_extract[[w+1]][7],
                  percent_by_material = wall_extract[[w+1]][8],
                  count = wall_extract[[w+1]][c+1]
                  )
          
        total_out <- rbind(total_out, wall_row_out) 
        
        }

        for(r in 1:length(roof_material)){
          
          roof_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[13]][c+1],
                  percent_by_age = area_extract[[14]][c+1],
                  housing_component = "ROOF",
                  housing_material = roof_material[r],
                  total_by_material = roof_extract[[r+1]][7],
                  percent_by_material = roof_extract[[r+1]][8],
                  count = roof_extract[[r+1]][c+1]
                  )
          
        total_out <- rbind(total_out, roof_row_out) 
        
        }
      }
      
      table_34 <- rbind(table_34, total_out)  
    }

    # cleanup numerics

    table_34 <- cbind(
          table_34 %>% dplyr::select(1:3),
          table_34 %>% dplyr::select(total_by_age, percent_by_age) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_34 %>% dplyr::select(6:7),
          table_34 %>% dplyr::select(total_by_material, percent_by_material, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_34$count[is.na(table_34$count)] <- 0
    table_34$area_type[table_34$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_34$count[table_34$area_type == "URBAN"] <- NA
    }
    
    table_34 <- table_34 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_34$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_34, paste0(dist_path, "table_34.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_34 <- rbind(natl_table_34, table_34)
    
  } # end else 
    
  # Process Table 35, Housing Unit by Water and Power Source ---

  if("35" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "35.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "35.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)    
    if(d == 37){
      pdf_text <- c(pdf_text[1:7], "HYDERABAD DISTRICT", pdf_text[8:91])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "SOURCE OF DRINKING WATER",
                    "INSIDE     -    -    -   -    -",
                    rep("---     -    -    -   -    -", 5),
                    "OUTSIDE     -    -    -   -    -",
                    rep("---     -    -    -   -    -", 7),
                    "COOKING FUEL USED",
                    rep("---     -    -    -   -    -", 4),
                    "SOURCE OF LIGHTING",
                    rep("---     -    -    -   -    -", 6)
                    )
      pdf_text[8] <- "ALL LOCALITIES"
    }
    if(d == 75){
      pdf_text[6] <- "LORALAI DISTRICT"
    }
    if(d == 100){
      pdf_text[7] <- "PANJGUR DISTRICT"
    }

      
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    ownership_types <- c("OWNED", "RENTED", "RENT FREE")
    inside_water_source <- c("TOTAL INSIDE", "TAP", "ELECTRIC/HAND PUMP", "PROTECTED WELL", "UN-PROTECTED WELL", "OTHERS")
    outside_water_source <- c("TOTAL OUTSIDE", "TAP", "ELECTRIC/HAND PUMP", "PROTECTED WELL", "UN-PROTECTED WELL", "SPRING", "CANAL/RIVER/POND", "OTHERS")
    fuel_source <- c("WOOD", "GAS", "KEROSENE OIL", "OTHERS")
    light_source <- c("ELECTRICITY", "KEROSENE OIL", "GAS LAMP", "OTHERS")

    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_35 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      inside_start <- grep("INSIDE", area_extract)
      outside_start <- grep("OUTSIDE", area_extract)
      fuel_start <- grep("COOKING FUEL USED", area_extract)
      light_start <- grep("SOURCE OF LIGHTING", area_extract)
      inside_extract <- area_extract[inside_start:(inside_start+(as.integer(length(inside_water_source))))]
      outside_extract <- area_extract[outside_start:(outside_start+(as.integer(length(outside_water_source))))]
      fuel_extract <- area_extract[fuel_start:(fuel_start+(as.integer(length(fuel_source))))]
      light_extract <- area_extract[light_start:(light_start+(as.integer(length(light_source))))]
      total_out <- tibble()
      for(h in 1:length(ownership_types)){
        for(w in 1:length(inside_water_source)){
          
          in_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[27]][h+1],
                  percent_by_tenure = area_extract[[28]][h+1],
                  utility = "INSIDE WATER",
                  source = inside_water_source[w],
                  total_by_source = inside_extract[[w]][5],
                  percent_by_source = inside_extract[[w]][6],
                  count = inside_extract[[w]][h+1]
                  )
          
        total_out <- rbind(total_out, in_row_out) 
        
        }

        for(o in 1:length(outside_water_source)){
          
          out_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[27]][h+1],
                  percent_by_tenure = area_extract[[28]][h+1],
                  utility = "OUTSIDE WATER",
                  source = outside_water_source[o],
                  total_by_source = outside_extract[[o]][5],
                  percent_by_source = outside_extract[[o]][6],
                  count = outside_extract[[o]][h+1]
                  )
          
        total_out <- rbind(total_out, out_row_out) 
        
        }
        
        for(f in 1:length(fuel_source)){
          
          fuel_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[27]][h+1],
                  percent_by_tenure = area_extract[[28]][h+1],
                  utility = "COOKING FUEL",
                  source = fuel_source[f],
                  total_by_source = fuel_extract[[f+1]][5],
                  percent_by_source = fuel_extract[[f+1]][6],
                  count = outside_extract[[f+1]][h+1]
                  )
          
        total_out <- rbind(total_out, fuel_row_out) 
        
        }

        for(l in 1:length(light_source)){
          
          light_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[27]][h+1],
                  percent_by_tenure = area_extract[[28]][h+1],
                  utility = "LIGHTING SOURCE",
                  source = light_source[l],
                  total_by_source = fuel_extract[[l+1]][5],
                  percent_by_source = fuel_extract[[l+1]][6],
                  count = fuel_extract[[l+1]][h+1]
                  )
          
        total_out <- rbind(total_out, light_row_out) 
        
        }        
        
      }
      
      table_35 <- rbind(table_35, total_out)  
    }

    # cleanup numerics

    table_35 <- cbind(
          table_35 %>% dplyr::select(1:3),
          table_35 %>% dplyr::select(total_by_tenure, percent_by_tenure) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_35 %>% dplyr::select(6:7),
          table_35 %>% dplyr::select(total_by_source, percent_by_source, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_35$count[is.na(table_35$count)] <- 0
    table_35$area_type[table_35$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_35$count[table_35$area_type == "URBAN"] <- NA
    }
    
    table_35 <- table_35 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_35$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_35, paste0(dist_path, "table_35.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_35 <- rbind(natl_table_35, table_35)
    
  } # end else   
    
  # Process Table 36, Housing Unit by Water and Power Source and Construction Duration---

  if("36" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "36.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "36.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    
    if(d == 2){
      pdf_text[9] <- "SOURCE OF DRINKING WATER"
      pdf_text[11] <- ""
      pdf_text[39] <- "SOURCE OF DRINKING WATER"
      pdf_text[41] <- ""
      pdf_text[69] <- "SOURCE OF DRINKING WATER"
      pdf_text[70] <- ""
    }
    if(d == 49){
      pdf_text <- c(pdf_text[1:7], "KARACHI EAST DISTRICT", pdf_text[8:91])
    }
    if(d == 52){
      pdf_text <- c(pdf_text[1:6], "KARAK DISTRICT", pdf_text[7:90])
    }
    if(d == 53){
      pdf_text <- c(pdf_text[1:7], "KASHMOR DISTRICT", pdf_text[8:91])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "SOURCE OF DRINKING WATER",
                    "INSIDE     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 5),
                    "OUTSIDE     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 7),
                    "COOKING FUEL USED",
                    rep("---     -    -    -   -    -   -   -", 4),
                    "SOURCE OF LIGHTING",
                    rep("---     -    -    -   -    -   -   -", 6)
                    )
    }
    if(d == 67){
      pdf_text[7] <- "KORANGI DISTRICT"
    }
    if(d == 80){
      pdf_text[6] <- "MANSEHRA DISTRICT"
    }
    
    pdf_text <- gsub("KILLA SAIFULLAH", "KILLA SAIFULLAH DISTRICT", pdf_text)
    pdf_text <- gsub("LORLAI", "LORALAI", pdf_text)
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)    
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    construction_ages <- c("UNDER CONSTRUCTION", "LESS THAN 5 YEARS", "5-10 YEARS", "11-50 YEARS", "OVER 50 YEARS")
    inside_water_source <- c("TAP", "ELECTRIC/HAND PUMP", "PROTECTED WELL", "UN-PROTECTED WELL", "OTHERS")
    outside_water_source <- c("TAP", "ELECTRIC/HAND PUMP", "PROTECTED WELL", "UN-PROTECTED WELL", "SPRING", "CANAL/RIVER/POND", "OTHERS")
    fuel_source <- c("WOOD", "GAS", "KEROSENE OIL", "OTHERS")
    light_source <- c("ELECTRICITY", "KEROSENE OIL", "GAS LAMP", "OTHERS")

    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_36 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      inside_start <- grep("INSIDE", area_extract)
      outside_start <- grep("OUTSIDE", area_extract)
      fuel_start <- grep("COOKING FUEL USED", area_extract)
      light_start <- grep("SOURCE OF LIGHTING", area_extract)
      inside_extract <- area_extract[inside_start:(inside_start+(as.integer(length(inside_water_source))))]
      outside_extract <- area_extract[outside_start:(outside_start+(as.integer(length(outside_water_source))))]
      fuel_extract <- area_extract[fuel_start:(fuel_start+(as.integer(length(fuel_source))))]
      light_extract <- area_extract[light_start:(light_start+(as.integer(length(light_source))))]
      total_out <- tibble()
      
      for(c in 1:length(construction_ages)){
        for(w in 1:length(inside_water_source)){
          
          in_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[27]][c+1],
                  percent_by_age = area_extract[[28]][c+1],
                  utility = "INSIDE WATER",
                  source = inside_water_source[w],
                  total_by_source = inside_extract[[w+1]][7],
                  percent_by_source = inside_extract[[w+1]][8],
                  count = inside_extract[[w+1]][c+1]
                  )
          
        total_out <- rbind(total_out, in_row_out) 
        
        }

        for(o in 1:length(outside_water_source)){
          
          out_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[27]][c+1],
                  percent_by_age = area_extract[[28]][c+1],
                  utility = "OUTSIDE WATER",
                  source = outside_water_source[o],
                  total_by_source = outside_extract[[o+1]][7],
                  percent_by_source = outside_extract[[o+1]][8],
                  count = outside_extract[[o+1]][c+1]
                  )
          
        total_out <- rbind(total_out, out_row_out) 
        
        }        

        for(f in 1:length(fuel_source)){
          
          fuel_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[27]][c+1],
                  percent_by_age = area_extract[[28]][c+1],
                  utility = "COOKING FUEL",
                  source = fuel_source[f],
                  total_by_source = fuel_extract[[f+1]][7],
                  percent_by_source = fuel_extract[[f+1]][8],
                  count = fuel_extract[[f+1]][c+1]
                  )
          
        total_out <- rbind(total_out, fuel_row_out) 
        
        }

        for(l in 1:length(light_source)){
          
          light_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  total_by_age = area_extract[[27]][c+1],
                  percent_by_age = area_extract[[28]][c+1],
                  utility = "LIGHTING SOURCE",
                  source = light_source[l],
                  total_by_source = light_extract[[l+1]][7],
                  percent_by_source = light_extract[[l+1]][8],
                  count = light_extract[[l+1]][c+1]
                  )
          
        total_out <- rbind(total_out, light_row_out) 
        
        }                    

      }
      
      table_36 <- rbind(table_36, total_out)  
    }

    # cleanup numerics

    table_36 <- cbind(
          table_36 %>% dplyr::select(1:3),
          table_36 %>% dplyr::select(total_by_age, percent_by_age) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_36 %>% dplyr::select(6:7),
          table_36 %>% dplyr::select(total_by_source, percent_by_source, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_36$count[is.na(table_36$count)] <- 0
    table_36$area_type[table_36$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_36$count[table_36$area_type == "URBAN"] <- NA
    }
    
    table_36 <- table_36 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_36$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_36, paste0(dist_path, "table_36.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_36 <- rbind(natl_table_36, table_36)
    
  } # end else   
    
  # Process Table 37, Housing Unit by Kitchen and Sanitary Facilities ---

  if("37" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "37.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "37.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
  
    # remove headers and footers from document
    
    # have to do a correction for Chaghai district, which is misspelled in this table
    pdf_text <- gsub("CHAGHI", "CHAGAI", pdf_text)
    # correct for Chiniot, which ommitted the district name
    if(d == 16){
      pdf_text <- c(pdf_text[1:5], "CHINIOT DISTRICT", pdf_text[6:59])
    }
    if(d == 30){
      pdf_text <- c(pdf_text[1:5], "GUJRANWALA DISTRICT", pdf_text[6:59])
    }
    if(d == 49){
      pdf_text[6] <- "KARACHI EAST DISTRICT"
    }
    if(d == 52){
      pdf_text[6] <- "KARAK DISTRICT"
    }
    if(d == 53){
      pdf_text <- c(pdf_text[1:5], "KASHMOR DISTRICT", pdf_text[6:59])
    }
    if(d == 58){
      pdf_text[6] <- "KHARAN DISTRICT"
    }
    if(d == 63){
      pdf_text[6] <- "KILLA SAIFULLAH DISTRICT"
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "KITCHEN",
                    rep("---     -    -    -   -    -", 3),
                    "BATHROOM",
                    rep("---     -    -    -   -    -", 3),
                    "LATRINE",
                    rep("---     -    -    -   -    -", 8)
                    )
    }
   if(d == 73){
      pdf_text[5] <- "LAYYAH DISTRICT"
    }
    if(d == 76){
      pdf_text[5] <- "LOWER DIR DISTRICT"
    }
    if(d == 80){
      pdf_text[5] <- "MANSEHRA DISTRICT"
    }
    if(d == 107){
      pdf_text[5] <- "SAHIWAL DISTRICT"
    }
    if(d == 109){
      pdf_text[5] <- "SARGODHA DISTRICT"
    }
    if(d == 113){
      pdf_text[5] <- "SHERANI DISTRICT"
    }
    
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]    
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    ownership_types <- c("OWNED", "RENTED", "RENT FREE")
    kitchen_types <- c("SEPARATE", "SHARED", "NONE")
    bath_types <- c("SEPARATE", "SHARED", "NONE")
    latrine_types <- c("SEWER", "SEPTIC TANK", "OPEN DRAIN", "PIT WITH SLAB", "OTHER", "NONE")

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_37 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      kitchen_start <- grep("KITCHEN", area_extract)
      bath_start <- grep("BATHROOM", area_extract)
      latrine_start <- grep("LATRINE", area_extract)
      kitchen_extract <- area_extract[kitchen_start:(kitchen_start+(as.integer(length(kitchen_types))))]
      bath_extract <- area_extract[bath_start:(bath_start+(as.integer(length(bath_types))))]
      latrine_extract <- area_extract[latrine_start:(latrine_start+(as.integer(length(latrine_types))))]
      total_out <- tibble()
      for(h in 1:length(ownership_types)){
        for(w in 1:length(kitchen_types)){
          
          kitchen_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[17]][h+1],
                  percent_by_tenure = area_extract[[18]][h+1],
                  facility = "KITCHEN",
                  source = kitchen_types[w],
                  total_by_facility = kitchen_extract[[w+1]][5],
                  percent_by_facility = kitchen_extract[[w+1]][6],
                  count = kitchen_extract[[w+1]][h+1]
                  )
          
        total_out <- rbind(total_out, kitchen_row_out) 
        
        }

        for(b in 1:length(bath_types)){
          
          bath_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[17]][h+1],
                  percent_by_tenure = area_extract[[18]][h+1],
                  facility = "BATH",
                  source = bath_types[b],
                  total_by_facility = bath_extract[[b+1]][5],
                  percent_by_facility = bath_extract[[b+1]][6],
                  count = bath_extract[[b+1]][h+1]
                  )
          
        total_out <- rbind(total_out, bath_row_out) 
        
        }

        for(l in 1:length(latrine_types)){
          
          latrine_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  ownership_status = ownership_types[h],
                  total_by_tenure = area_extract[[17]][h+1],
                  percent_by_tenure = area_extract[[18]][h+1],
                  facility = "LATRINE",
                  source = latrine_types[l],
                  total_by_facility = latrine_extract[[l+1]][5],
                  percent_by_facility = latrine_extract[[l+1]][6],
                  count = latrine_extract[[l+1]][h+1]
                  )
          
        total_out <- rbind(total_out, latrine_row_out) 
        
        }
        
      }
      
      table_37 <- rbind(table_37, total_out)  
    }

    # cleanup numerics

    table_37 <- cbind(
          table_37 %>% dplyr::select(1:3),
          table_37 %>% dplyr::select(total_by_tenure, percent_by_tenure) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_37 %>% dplyr::select(6:7),
          table_37 %>% dplyr::select(total_by_facility, percent_by_facility, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_37$count[is.na(table_37$count)] <- 0
    table_37$area_type[table_37$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_37$count[table_37$area_type == "URBAN"] <- NA
    }
    
    table_37 <- table_37 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_37$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_37, paste0(dist_path, "table_37.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_37 <- rbind(natl_table_37, table_37)
    
  } # end else   
          
  # Process Table 38, Housing Unit by Kitchen and Sanitary Facilities and Construction Duration ---

  if("38" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "38.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "38.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    # remove headers and footers from document
    # have to do a correction for Chaghai district, which is misspelled in this table
    pdf_text <- gsub("CHAGHI", "CHAGAI", pdf_text)
    # correct for Chiniot, which ommitted the district name
    if(d == 16){
      pdf_text <- c(pdf_text[1:6], "CHINIOT DISTRICT", pdf_text[7:60])
    }
    if(d == 30){
      pdf_text <- c(pdf_text[1:6], "GUJRANWALA DISTRICT", pdf_text[7:60])
    }
    if(d == 34){
      pdf_text <- c(pdf_text[1:6], "HANGU DISTRICT", pdf_text[7:60])
    }
    if(d == 38){
      pdf_text <- c(pdf_text[1:9], "ALL LOCALITIES", pdf_text[10:62])
    }
    if(d == 49){
      pdf_text <- c(pdf_text[1:8], "KARACHI EAST DISTRICT", pdf_text[9:62])
    }
    if(d == 52){
      pdf_text <- c(pdf_text[1:6], "KARAK DISTRICT", pdf_text[7:60])
    }
    if(d == 53){
      pdf_text <- c(pdf_text[1:8], "KASHMOR DISTRICT", pdf_text[9:62])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "KITCHEN",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "BATHROOM",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "LATRINE",
                    rep("---     -    -    -   -    -   -   -", 8)
                    )
    }
    if(d == 73){
      pdf_text[7] <- "LAYYAH DISTRICT"
    }
    if(d == 76){
      pdf_text[5] <- "LOWER DIR DISTRICT"
    }
    if(d == 107){
      pdf_text[5] <- "SAHIWAL DISTRICT"
    }
    if(d == 109){
      pdf_text[7] <- "SARGODHA DISTRICT"
    }
    if(d == 129){
      pdf_text[7] <- "TORGHAR DISTRICT"
      pdf_text <- c(pdf_text[1:8], "KITCHEN", pdf_text[9:60])
    }
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    construction_ages <- c("UNDER CONSTRUCTION", "LESS THAN 5 YEARS", "5-10 YEARS", "11-50 YEARS", "OVER 50 YEARS")
    kitchen_types <- c("SEPARATE", "SHARED", "NONE")
    bath_types <- c("SEPARATE", "SHARED", "NONE")
    latrine_types <- c("SEWER", "SEPTIC TANK", "OPEN DRAIN", "PIT WITH SLAB", "OTHER", "NONE")

    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_38 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      kitchen_start <- grep("KITCHEN", area_extract)
      bath_start <- grep("BATHROOM", area_extract)
      latrine_start <- grep("LATRINE", area_extract)
      kitchen_extract <- area_extract[kitchen_start:(kitchen_start+(as.integer(length(kitchen_types))))]
      bath_extract <- area_extract[bath_start:(bath_start+(as.integer(length(bath_types))))]
      latrine_extract <- area_extract[latrine_start:(latrine_start+(as.integer(length(latrine_types))))]
      total_out <- tibble()
      for(h in 1:length(construction_ages)){
        for(w in 1:length(kitchen_types)){
          
          kitchen_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[h],
                  total_by_age = area_extract[[17]][h+1],
                  percent_by_age = area_extract[[18]][h+1],
                  facility = "KITCHEN",
                  source = kitchen_types[w],
                  total_by_facility = kitchen_extract[[w+1]][7],
                  percent_by_facility = kitchen_extract[[w+1]][8],
                  count = kitchen_extract[[w+1]][h+1]
                  )
          
        total_out <- rbind(total_out, kitchen_row_out) 
        
        }

        for(b in 1:length(bath_types)){
          
          bath_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[h],
                  total_by_age = area_extract[[17]][h+1],
                  percent_by_age = area_extract[[18]][h+1],
                  facility = "BATH",
                  source = bath_types[b],
                  total_by_facility = bath_extract[[b+1]][7],
                  percent_by_facility = bath_extract[[b+1]][8],
                  count = bath_extract[[b+1]][h+1]
                  )
          
        total_out <- rbind(total_out, bath_row_out) 
        
        }

        for(l in 1:length(latrine_types)){
          
          latrine_row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[h],
                  total_by_age = area_extract[[17]][h+1],
                  percent_by_age = area_extract[[18]][h+1],
                  facility = "LATRINE",
                  source = latrine_types[l],
                  total_by_facility = latrine_extract[[l+1]][7],
                  percent_by_facility = latrine_extract[[l+1]][8],
                  count = latrine_extract[[l+1]][h+1]
                  )
          
        total_out <- rbind(total_out, latrine_row_out) 
        
        }
        
      }
      
      table_38 <- rbind(table_38, total_out)  
    }

    # cleanup numerics

    table_38 <- cbind(
          table_38 %>% dplyr::select(1:3),
          table_38 %>% dplyr::select(total_by_age, percent_by_age) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_38 %>% dplyr::select(6:7),
          table_38 %>% dplyr::select(total_by_facility, percent_by_facility, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_38$count[is.na(table_38$count)] <- 0
    table_38$area_type[table_38$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_38$count[table_38$area_type == "URBAN"] <- NA
    }
    
    table_38 <- table_38 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_38$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_38, paste0(dist_path, "table_38.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_38 <- rbind(natl_table_38, table_38)
    
  } # end else   
      
  # Process Table 39, Housing Unit by Period of Construction, Materials ---

  if("39" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "39.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "39.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    
    # remove headers and footers from document
    pdf_text <- gsub("CHAGHI", "CHAGAI", pdf_text)
    pdf_text <- gsub("SIAFULLAH", "SAIFULLAH", pdf_text)
    pdf_text <- gsub("STONES", "STONES  ", pdf_text)
    
    if(d == 16){
      pdf_text <- c(pdf_text[1:6], "CHINIOT DISTRICT", pdf_text[7:90])
    }
    if(d == 30){
      pdf_text <- c(pdf_text[1:9], "GUJRANWALA DISTRICT", 
                    pdf_text[10:15], "LESS THAN 5 YEARS", pdf_text[16:19], "5 - 10 YEARS", pdf_text[20:23], "11 - 50 YEARS", pdf_text[24:27], "OVER 50 YEARS", pdf_text[28:33],
                    "RURAL", "UNDER CONSTRUCTION",
                    pdf_text[34:37], "LESS THAN 5 YEARS", pdf_text[38:41], "5 - 10 YEARS", pdf_text[42:45], "11 - 50 YEARS", pdf_text[46:49], "OVER 50 YEARS", pdf_text[50:55],
                    "URBAN", "UNDER CONSTRUCTION",
                    pdf_text[56:59], "LESS THAN 5 YEARS", pdf_text[60:63], "5 - 10 YEARS", pdf_text[64:67], "11 - 5- YEARS", pdf_text[68:71], "OVER 50 YEARS", pdf_text[72:77])
    }
    if(d == 44){
      pdf_text <- c(pdf_text[1:7], "JHELUM DISTRICT", pdf_text[8:91])
    }
    if(d == 49){
      pdf_text <- c(pdf_text[1:7], "KARACHI EAST DISTRICT", pdf_text[8:91])
    }
    if(d == 52){
      pdf_text <- c(pdf_text[1:6], "KARAK DISTRICT", pdf_text[7:90])
    }    
    if(d == 53){
      pdf_text <- c(pdf_text[1:7], "KASHMOR DISTRICT", pdf_text[8:91])
    }
    if(d == 65){
      pdf_text <- c(pdf_text,
                    "URBAN", "UNDER CONSTRUCTION",
                    "BAKED BRICKS/BLOCKS/STONES     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "LESS THAN 5 YEARS",
                    "BAKED BRICKS/BLOCKS/STONES     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "5 - 10 YEARS",
                    "BAKED BRICKS/BLOCKS/STONES     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "11 - 50 YEARS",
                    "BAKED BRICKS/BLOCKS/STONES     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 3),
                    "OVER 50 YEARS",
                    "BAKED BRICKS/BLOCKS/STONES     -    -    -   -    -   -   -",
                    rep("---     -    -    -   -    -   -   -", 5)
                    )
    }
    if(d == 73){
      pdf_text[6] <- "LAYYAH DISTRICT"
    }
    if(d == 76){
      pdf_text[6] <- "LOWER DIR DISTRICT"
    }
    if(d == 80){
      pdf_text[6] <- "MANSEHRA DISTRICT"
    }
    if(d == 104){
      pdf_text[6] <- "RAHIM YAR KHAN DISTRICT"
    }
    if(d == 107){
      pdf_text[6] <- "SAHIWAL DISTRICT"
    }
    if(d == 131){
      pdf_text[6] <- "UPPER DIR DISTRICT"
    }
    
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- pdf_trimmed[pdf_trimmed != ""]
    
    area_types <- c("ALL LOCALITIES", "RURAL", "URBAN")
    construction_ages <- c("UNDER CONSTRUCTION", "LESS THAN 5 YEARS", "5 - 10 YEARS", "11 - 50 YEARS", "OVER 50 YEARS")
    wall_material <- c("BAKED BRICKS/BLOCKS/STONES", "UNBAKED BRICKS/MUD", "WOOD/BAMBOO", "OTHERS")
    roof_material <- c("RCC/RBC", "CEMENT/IRON SHEETS", "GARDER/T.IRON", "WOOD/BAMBOO", "OTHERS")

    # make sure slashes are consistently spaced
    pdf_trimmed <- gsub(" / ", "/", pdf_trimmed)
    pdf_trimmed <- gsub("/  STONES", "/STONES", pdf_trimmed) # for some reason lots of extra padding in FR DI Khan
    # make sure numbers are properly padded
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
    table_39 <- tibble()
    
    for(k in 1:length(area_types)){
      area_start <- grep(area_types[k], area_split)
      area_end <- ifelse(k != 3, (as.integer(grep(area_types[k+1], area_split)-1)), length(area_split))
      area_extract <- area_split[area_start:area_end]
      age_starts <- as.integer(grep("BAKED BRICKS/BLOCKS/STONES", area_extract) - 1)
      
      total_out <- tibble()
      
      for(r in 1:length(roof_material)){
        for(c in 1:length(construction_ages)){
          for(w in 1:length(wall_material)){
          
          row_out <- tibble(
                  area_name = district_name,
                  area_type = area_types[k],
                  construction_age = construction_ages[c],
                  roof_material = roof_material[r],
                  total_by_roof_material = area_extract[[27]][r+1],
                  percent_by_roof_material = area_extract[[28]][r+1],
                  wall_material = wall_material[w],
                  total_by_wall_material = area_extract[[age_starts[c]+w]][7],
                  percent_by_wall_material = area_extract[[age_starts[c]+w]][8],
                  count = area_extract[[age_starts[c]+w]][r+1]
                  )
          
        total_out <- rbind(total_out, row_out) 
        
          }
        }
      }

            
      table_39 <- rbind(table_39, total_out)        
    }


    # cleanup numerics

    table_39 <- cbind(
          table_39 %>% dplyr::select(1:4),
          table_39 %>% dplyr::select(total_by_roof_material, percent_by_roof_material) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric),
          table_39 %>% dplyr::select(7),
          table_39 %>% dplyr::select(total_by_wall_material, percent_by_wall_material, count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )
    
    # Convert NA values to 0 counts
    table_39$count[is.na(table_39$count)] <- 0
    table_39$area_type[table_39$area_type == "ALL LOCALITIES"] <- "TOTAL"
    if(d == 65){
      table_39$count[table_39$area_type == "URBAN"] <- NA
    }
    
    table_39 <- table_39 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_39$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_39, paste0(dist_path, "table_39.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_39 <- rbind(natl_table_39, table_39)
    
  } # end else   
    
  # Process Table 40, Household Information Source ---
    if("40" %in% missing_tables){
    missing_files <- rbind(missing_files,
                           tibble(district_number = district_number, district_name = district_name, missing_table = "40.pdf"))
    } else {

    # load the pdf file      
    target <- paste0("./district_tables_raw/", district_name, "/", district_number, "40.pdf")
    pdf_import <- pdf_text(target)
    pdf_text <- toString(pdf_import)
    pdf_text <- read_lines(pdf_text)
    pdf_text <- gsub("KASHMORE", "KASHMOR", pdf_text)
    pdf_text <- pdf_text[pdf_text != ""]
    
    # remove headers and footers from document
    header_end <- as.integer(grep(district_name, pdf_text)) - 1
    pdf_trimmed <- str_to_upper(pdf_text[- c(1:header_end)])
    pdf_trimmed <- gsub("OVERALL", "TOTAL", pdf_trimmed)
    pdf_trimmed <- gsub("(?<=\\d)\\s(?=\\d)", "   ", pdf_trimmed, perl = T)
    
    # find the breaks for each administrative area
    area_starts <- as.integer(grep("TOTAL", pdf_trimmed) - 1)
    area_types <- c("TOTAL", "RURAL", "URBAN")
    information_sources <- c("HOUSEHOLDS WITH ANY SOURCE OF INFORMATION", "RADIO", "TV", "NEWSPAPER", "MOBILE", "COMPUTER/INTERNET")
    table_40 <- tibble()
    
    for(k in 1:length(area_starts)){
      area_split <- strsplit(trimws(pdf_trimmed), "\\s{2,}")
      area_end <- ifelse(k != length(area_starts), (as.integer(area_starts[k+1]) - 1), length(area_split))
      area_extract <- area_split[area_starts[k]:area_end]
      missing_areas <- area_types[!(area_types %in% unlist(area_extract))]
      if("RURAL" %in% missing_areas){
          area_extract <- c(area_extract[1:2], list(rep("RURAL", 8), area_extract[[3]])) # if rural is missing, it throws off the following
        }
      total_out <- tibble()
      for(a in 1:3){
        for(h in 1:length(information_sources)){
                  row_out <- tibble(
                  area_name = area_extract[[1]][1],
                  area_type = area_types[a],
                  information_source = information_sources[h],
                  count = ifelse(area_types[a] %in% missing_areas, NA, area_extract[[a+1]][h+1])
                  )
                  
                total_out <- rbind(total_out, row_out)
            
          }
      }
      
      table_40 <- rbind(table_40, total_out)  
    }

    # cleanup numerics

    table_40 <- cbind(table_40 %>% dplyr::select(1:3),
          table_40 %>% dplyr::select(count) %>%
            mutate_all(funs(gsub(",", "", .))) %>%
            mutate_all(as.numeric)
    )

    table_40$count[is.na(table_40$count) & !(table_40$area_type %in% missing_areas)] <- 0

    table_40 <- table_40 %>%
      mutate(
        district_code = district_number,
        district_name = district_name
      ) %>%
      left_join(
        all_codes
      ) %>%
      dplyr::select(province_name, district_code, district_name,
                    everything())
    
    prov_path <- paste0("./processed_forms/", unique(table_40$province_name), "/")
    # create the appropriate province subdirectory if it doesn't already exist
    ifelse(!dir.exists(file.path(prov_path)), dir.create(file.path(prov_path)), FALSE)
    
    dist_path <- paste0(prov_path, paste0(district_number, " - ", district_name, "/"))
    # create the appropriate district subdirectory if it doesn't already exist                   
    ifelse(!dir.exists(file.path(dist_path)), dir.create(file.path(dist_path)), FALSE)
    
    # write table data to a csv copy
    write_csv(table_40, paste0(dist_path, "table_40.csv", na = ""))
    
    # write table data to a national aggregate table
    
    natl_table_40 <- rbind(natl_table_40, table_40)
    
  } # end else   

  
  } # end the cycle through all district table pdfs!

# clean up and check consolidated files -----------------------------------

missing_files <- missing_files %>% filter(!is.na(missing_table))
write_csv(missing_files, "./validity_checks/missing_tables.csv", na = "")

# check area names for gaps
# table_01_areas <- unique(natl_table_01$area_name)
# table_02_areas <- unique(natl_table_02$locality_name)
# table_03_areas <- unique(natl_table_03$area_name)
# table_04_areas <- unique(natl_table_04$area_name)
# table_05_areas <- unique(natl_table_05$area_name)
# table_06_areas <- unique(natl_table_06$area_name)
# table_07_areas <- unique(natl_table_07$area_name)
# table_08_areas <- unique(natl_table_08$area_name)
# table_09_areas <- unique(natl_table_09$area_name)
# table_10_areas <- unique(natl_table_10$area_name)
# table_11_areas <- unique(natl_table_11$area_name)
# table_12_areas <- unique(natl_table_12$area_name)
# table_13_areas <- unique(natl_table_13$area_name)
# table_14_areas <- unique(natl_table_14$area_name)
# table_15_areas <- unique(natl_table_15$area_name)
# table_16_areas <- unique(natl_table_16$area_name)
# table_17_areas <- unique(natl_table_17$area_name)
# table_18_areas <- unique(natl_table_18$area_name)
# table_19_areas <- unique(natl_table_19$area_name)
# table_20_areas <- unique(natl_table_20$area_name)
# table_21_areas <- unique(natl_table_21$area_name)
# table_22_lit_areas <- unique(natl_table_22_literacy$area_name)
# table_22_mar_areas <- unique(natl_table_22_marital$area_name)
# table_22_rel_areas <- unique(natl_table_22_religion$area_name)
# table_22_work_areas <- unique(natl_table_22_work$area_name)
# table_27_areas <- unique(natl_table_27$area_name)
# table_28_areas <- unique(natl_table_28$area_name)
# table_29_areas <- unique(natl_table_29$area_name)
# table_30_areas <- unique(natl_table_30$area_name)
# table_31_areas <- unique(natl_table_31$area_name)
# table_32_areas <- unique(natl_table_32$area_name)
# table_33_areas <- unique(natl_table_33$area_name)
# table_34_areas <- unique(natl_table_34$area_name)
# table_35_areas <- unique(natl_table_35$area_name)
# table_36_areas <- unique(natl_table_36$area_name)
# table_37_areas <- unique(natl_table_37$area_name)
# table_38_areas <- unique(natl_table_38$area_name)
# table_39_areas <- unique(natl_table_39$area_name)
# table_40_areas <- unique(natl_table_40$area_name)

# ----

tbl1_check <- natl_table_01 %>% 
  filter(area_name == district_name) %>%
  group_by(province_name, district_name) %>%
  summarize(total = total_pop[area_type == "TOTAL"],
            total_male = total_male[area_type == "TOTAL"],
            total_fem = total_female[area_type == "TOTAL"],
            total_trans = total_trans[area_type == "TOTAL"],
            total_rur = total_pop[area_type == "RURAL"],
            total_urb = total_pop[area_type == "URBAN"]) %>%
  rowwise %>% mutate(
    total_calc = sum(c(total_rur, total_urb), na.rm = T),
    total_gender = sum(c(total_male, total_fem, total_trans), na.rm = T),
    calc_error = ifelse(total != total_calc, "ERROR", "OK"),
    gender_error = ifelse(total != total_gender, "ERROR", "OK")
  )

write_csv(natl_table_01, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_01_consolidated.csv", na = "")

# ----

natl_table_02 %>% group_by(locality_name) %>%
  summarize(count = length(locality_name[gender == "ALL SEXES"])) %>% filter(count >= 2)

tbl2_check <- natl_table_02 %>% group_by(district_name, area_name, locality_name) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES"],
    total_calc = sum(total_population[gender != "ALL SEXES"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_02, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_02_consolidated.csv", na = "")

# ----
natl_table_03$locality_count <- as.numeric(natl_table_03$locality_count)

natl_table_03 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & locality_size == "TOTAL"])) %>% filter(count >= 2)

tbl3_check <- natl_table_03 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES" & locality_size == "TOTAL"],
    total_calc = sum(total_population[gender != "ALL SEXES" & locality_size == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))
  
natl_table_03$total_population[is.na(natl_table_03$locality_count)] <- 0
natl_table_03$locality_count[is.na(natl_table_03$locality_count)] <- 0

write_csv(natl_table_03, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_03_consolidated.csv", na = "")

# ----
natl_table_04 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl4_check <- natl_table_04 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = group_all_ages[gender == "ALL SEXES" & area_type == "TOTAL"],
    total_calc = sum(group_all_ages[gender != "ALL SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_04, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_04_consolidated.csv", na = "")

# ----
natl_table_05 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl5_check <- natl_table_05 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = group_all_ages[gender == "ALL SEXES" & area_type == "TOTAL"],
    total_calc = sum(group_all_ages[gender != "ALL SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_05, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_05_consolidated.csv", na = "")

# ----
natl_table_06 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & marital_status == "TOTAL" & age_group == "15 AND ABOVE"])) %>% filter(count >= 2)

tbl6_check <- natl_table_06 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & marital_status == "TOTAL" & age_group == "15 AND ABOVE"],
    total_calc = sum(count[age_group == "15 AND ABOVE" & gender == "ALL SEXES" & marital_status == "TOTAL" & area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_06, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_06_consolidated.csv", na = "")

# ----
natl_table_07 %>% group_by(district_name, area_name, household_head_relationship) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & marital_status == "TOTAL"])) %>% filter(count >= 2)

tbl7_check <- natl_table_07 %>% group_by(district_name, area_name, household_head_relationship) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & marital_status == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & marital_status != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_07, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_07_consolidated.csv", na = "")

# ----
natl_table_08 %>% group_by(district_name, area_name, household_head_relationship) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"])) %>% filter(count >= 2)

tbl8_check <- natl_table_08 %>% group_by(district_name, area_name, household_head_relationship) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(count[gender != "ALL SEXES" & age_group != "ALL AGES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_08, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_08_consolidated.csv", na = "")

# ----
natl_table_09 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & religious_affiliation == "TOTAL"])) %>% filter(count >= 2)

tbl9_check <- natl_table_09 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & religious_affiliation == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & religious_affiliation != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_09, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_09_consolidated.csv", na = "")

# ----
natl_table_10 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES" & nationality_status == "TOTAL"])) %>% filter(count >= 2)

tbl10_check <- natl_table_10 %>% group_by(district_name, area_name, age_group) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & nationality_status == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & nationality_status != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_10, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_10_consolidated.csv", na = "")

# ----
natl_table_11 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & mother_tongue == "TOTAL"])) %>% filter(count >= 2)

tbl11_check <- natl_table_11 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & mother_tongue == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & mother_tongue != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_11, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_11_consolidated.csv", na = "")

# ----
natl_table_12 %>% group_by(district_name, area_name, age_group, literacy_status) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl12_check <- natl_table_12 %>% group_by(district_name, area_name, age_group, literacy_status) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_12, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_12_consolidated.csv", na = "")

# ----
natl_table_13 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl13_check <- natl_table_13 %>% group_by(district_name, area_name, literacy_status) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_13, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_13_consolidated.csv", na = "")

# ----
natl_table_14 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL"])) %>% filter(count >= 2)

tbl14_check <- natl_table_14 %>% group_by(district_name, area_name, age_group) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES"  & ed_attainment != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

natl_table_14 <- natl_table_14 %>% dplyr::select(-district_code) # trying to keep this under 100MB

write_csv(natl_table_14, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_14_consolidated.csv", na = "")

# ----
natl_table_15 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL"])) %>% filter(count >= 2)

tbl15_check <- natl_table_15 %>% group_by(district_name, area_name, age_group) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES"  & ed_attainment != "TOTAL" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_15, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_15_consolidated.csv", na = "")

# ----
natl_table_16 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & work_activity == "TOTAL"])) %>% filter(count >= 2)

tbl16_check <- natl_table_16 %>% group_by(district_name, area_name, age_group) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & work_activity == "TOTAL POPULATION"],
    total_calc = sum(count[gender != "ALL SEXES"  & work_activity != "TOTAL POPULATION" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_16, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_16_consolidated.csv", na = "")

# ----
natl_table_17 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"])) %>% filter(count >= 2)

tbl17_check <- natl_table_17 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(count[gender != "ALL SEXES"  & age_group != "ALL AGES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_17, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_17_consolidated.csv", na = "")

# ----
natl_table_18 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL DISABLED POPULATION"])) %>% filter(count >= 2)

tbl18_check <- natl_table_18 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & ed_attainment == "TOTAL DISABLED POPULATION" & age_group == "5 AND ABOVE"],
    total_calc = sum(count[gender == "ALL SEXES"  & ed_attainment == "TOTAL DISABLED POPULATION" & area_type == "TOTAL" & age_group != "5 AND ABOVE"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_18, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_18_consolidated.csv", na = "")

# ----
natl_table_19 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & work_activity == "TOTAL POPULATION"])) %>% filter(count >= 2)

tbl19_check <- natl_table_19 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & work_activity == "TOTAL POPULATION" & age_group == "10 AND ABOVE"],
    total_calc = sum(count[gender == "ALL SEXES"  & work_activity == "TOTAL POPULATION" & area_type == "TOTAL" & age_group != "10 AND ABOVE"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_19, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_19_consolidated.csv", na = "")

# ----
natl_table_20 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "BOTH SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl20_check <- natl_table_20 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "BOTH SEXES" & area_type == "TOTAL"],
    total_calc = sum(count[gender != "BOTH SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_20, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_20_consolidated.csv", na = "")

# ----
natl_table_21 %>% group_by(district_name, area_name, age_group) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & cnic_status == "TOTAL POPULATION"])) %>% filter(count >= 2)

tbl21_check <- natl_table_21 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL" & cnic_status == "TOTAL POPULATION" & age_group == "18 AND ABOVE"],
    total_calc = sum(count[gender == "ALL SEXES"  & cnic_status == "TOTAL POPULATION" & area_type == "TOTAL" & age_group != "18 AND ABOVE"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_21, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_21_consolidated.csv", na = "")

# ----
natl_table_22_literacy %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"])) %>% filter(count >= 2)

tbl22lit_check <- natl_table_22_literacy %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(total_population[gender == "ALL SEXES"  & area_type == "TOTAL" & age_group != "ALL AGES"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_22_literacy, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_22_literacy_consolidated.csv", na = "")

# ----
tbl22mar_check <- natl_table_22_marital %>% group_by(district_name, area_name, marital_status) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(total_population[gender == "ALL SEXES"  & area_type == "TOTAL" & age_group != "ALL AGES"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_22_marital, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_22_literacy_marital.csv", na = "")

# ----
tbl22rel_check <- natl_table_22_religion %>% group_by(district_name, area_name, religious_affiliation) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(total_population[gender == "ALL SEXES"  & area_type == "TOTAL" & age_group != "ALL AGES"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_22_religion, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_22_literacy_religion.csv", na = "")

# ----
tbl22work_check <- natl_table_22_work %>% group_by(district_name, area_name, work_status) %>%
  summarize(
    total_pbs = total_population[gender == "ALL SEXES" & area_type == "TOTAL" & age_group == "ALL AGES"],
    total_calc = sum(total_population[gender == "ALL SEXES"  & area_type == "TOTAL" & age_group != "ALL AGES"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_22_work, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_22_literacy_work.csv", na = "")

# ----
tbl23_check <- natl_table_23 %>% 
  rowwise %>% mutate(
    total_gender = sum(c(pop_male, pop_fem, pop_trans), na.rm = T),
    check = ifelse(pop_all != total_gender, "ERROR", "OK")
  )

write_csv(natl_table_23, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_23_consolidated.csv", na = "")

# ---
tbl24_check <- natl_table_24 %>% 
  rowwise %>% mutate(
    total_housing = sum(c(pacca_housing, semi_pacca_housing, kacha_housing), na.rm = T),
    check = ifelse(total_housing != total_housing, "ERROR", "OK")
  )

write_csv(natl_table_24, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_24_consolidated.csv", na = "")

# ---

tbl25_check <- natl_table_25 %>% 
  rowwise %>% mutate(
    total_gender = sum(c(pop_male, pop_fem, pop_trans), na.rm = T),
    check = ifelse(pop_all != total_gender, "ERROR", "OK")
  )

write_csv(natl_table_25, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_25_consolidated.csv", na = "")

# ----
tbl26_check <- natl_table_26 %>% 
  rowwise %>% mutate(
    total_housing = sum(c(pacca_housing, semi_pacca_housing, kacha_housing), na.rm = T),
    check = ifelse(total_housing != total_housing, "ERROR", "OK")
  )

write_csv(natl_table_26, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_26_consolidated.csv", na = "")

# ----

tbl27_check <- natl_table_27 %>% group_by(district_name, area_name, gender_type) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL" & housing_type == "TOTAL"],
    total_calc = sum(count[area_type == "TOTAL" & housing_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_27, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_27_consolidated.csv", na = "")

# ----

tbl28_check <- natl_table_28 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL" & household_size == "TOTAL"],
    total_calc = sum(count[area_type == "TOTAL" & household_size != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_28, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_28_consolidated.csv", na = "")

# ----
tbl29_check <- natl_table_29 %>% group_by(district_name, area_name, household_sizes, number_of_rooms) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_29, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_29_consolidated.csv", na = "")

# ----
tbl30_check <- natl_table_30 %>% group_by(district_name, area_name, ownership_status, number_of_rooms) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_30, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_30_consolidated.csv", na = "")

# ----

natl_table_31 %>% group_by(district_name, area_name) %>%
  summarize(count = length(area_name[gender == "ALL SEXES" & area_type == "TOTAL"])) %>% filter(count >= 2)

tbl31_check <- natl_table_31 %>% group_by(district_name, area_name) %>%
  summarize(
    total_pbs = count[gender == "ALL SEXES" & area_type == "TOTAL"],
    total_calc = sum(count[gender != "ALL SEXES" & area_type == "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_31, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_31_consolidated.csv", na = "")

# ----

tbl32_check <- natl_table_32 %>% group_by(district_name, area_name, construction_duration, number_of_rooms) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_32, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_32_consolidated.csv", na = "")

# ----

tbl33_check <- natl_table_33 %>% group_by(district_name, area_name, ownership_status, housing_component, housing_material) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_33, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_33_consolidated.csv", na = "")

# ----

tbl34_check <- natl_table_34 %>% group_by(district_name, area_name, construction_age, housing_component, housing_material) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_34, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_34_consolidated.csv", na = "")

# ----

tbl35_check <- natl_table_35 %>% group_by(district_name, area_name, ownership_status, utility, source) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_35, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_35_consolidated.csv", na = "")

# ----

tbl36_check <- natl_table_36 %>% group_by(district_name, area_name, construction_age, utility, source) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_36, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_36_consolidated.csv", na = "")

# ----

tbl37_check <- natl_table_37 %>% group_by(district_name, area_name, ownership_status, facility, source) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_37, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_37_consolidated.csv", na = "")

# ----

tbl38_check <- natl_table_38 %>% group_by(district_name, area_name, construction_age, facility, source) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_38, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_38_consolidated.csv", na = "")

# ----

tbl39_check <- natl_table_39 %>% group_by(district_name, area_name, construction_age, roof_material, wall_material) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_39, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_39_consolidated.csv", na = "")

# ----

tbl40_check <- natl_table_40 %>% group_by(district_name, area_name, information_source) %>%
  summarize(
    total_pbs = count[area_type == "TOTAL"],
    total_calc = sum(count[area_type != "TOTAL"], na.rm = T)) %>%
  rowwise() %>%
  mutate(check = ifelse(total_pbs != total_calc, "ERROR", "OK"))

write_csv(natl_table_40, "./processed_forms/CONSOLIDATED NATIONAL TABLES/table_40_consolidated.csv", na = "")

# -------

sample_check <- list()
for(i in (c(1:21,23:40))){
  sample_list <- sample(
    length(get(paste0("natl_table_", str_pad(i, width = 2, side = "left", pad = 0)))[ , 1]),
    5)
  sample_out <- list(
    paste0("Table ", str_pad(i, width = 2, side = "left", pad = 0)),
    get(paste0("natl_table_", str_pad(i, width = 2, side = "left", pad = 0)))[sample_list, ])
  
  sample_check <- c(sample_check, sample_out)
}




# CLEAN UP RURAL / URBAN AREA CODING AND CREATE KEYFILES ---------------
district_codes <- natl_table_01 %>% dplyr::select(province_name, district_code, district_name) %>% unique()

area_types <- natl_table_23 %>% dplyr::select(area_name) %>% 
  full_join(
    natl_table_25 %>% dplyr::select(area_name)
  ) %>%
  unique() %>%
  rowwise %>%
  mutate(
    type_name = ifelse(
      length(str_split(area_name, " ")[[1]]) > 1, 
      str_split(area_name, " ")[[1]][length(str_split(area_name, " ")[[1]])],
      NA)
    )

area_codes <- natl_table_23 %>% dplyr::select(province_name, district_code, district_name, sublvl_01, sublvl_02, sublvl_03, area_name, summary_row) %>% 
  mutate(area_type = "RURAL") %>%
  full_join(
    natl_table_25 %>% dplyr::select(province_name, district_code, district_name, sublvl_01, sublvl_02, sublvl_03, area_name, summary_row) %>%
      mutate(area_type = "URBAN")
  ) %>%
  filter(summary_row != "YES") %>%
  rename(sublvl_04 = area_name) %>%
  arrange(district_name, sublvl_01, sublvl_02, sublvl_03, sublvl_04) %>%
  dplyr::select(-summary_row)
  
area_codes_out <- tibble()
for(n in 1:length(unique(area_codes$district_code))){
  district_code = unique(area_codes$district_code)[n]
  district_subset = area_codes[area_codes$district_code == district_code, ]

  adm1_names = tibble(sublvl_01 = unique(district_subset$sublvl_01)) %>% 
    mutate(
      sublvl_01_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )

  district_subset <- district_subset %>% left_join(adm1_names)

  for(m in 1:length(adm1_names$sublvl_01)){
    sublvl_1_code = unique(district_subset$sublvl_01_code)[m]
    sublvl_01_subset = district_subset[district_subset$sublvl_01_code == sublvl_1_code, ]
    
    adm2_names = tibble(sublvl_02 = unique(sublvl_01_subset$sublvl_02)) %>%
      mutate(
        sublvl_02_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
    
    sublvl_01_subset <- sublvl_01_subset %>% left_join(adm2_names)
    
    for(o in 1:length(adm2_names$sublvl_02)){
      sublvl_02_code = unique(sublvl_01_subset$sublvl_02_code)[o]
      sublvl_02_subset = sublvl_01_subset[sublvl_01_subset$sublvl_02_code == sublvl_02_code, ]
      
      adm3_names = tibble(sublvl_03 = unique(sublvl_02_subset$sublvl_03)) %>%
      mutate(
        sublvl_03_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
      
      sublvl_02_subset <- sublvl_02_subset %>% left_join(adm3_names)
      
      for(p in 1:length(adm3_names$sublvl_03)){
          sublvl_03_code = unique(sublvl_02_subset$sublvl_03_code)[p]
          sublvl_03_subset = sublvl_02_subset[sublvl_02_subset$sublvl_03_code == sublvl_03_code, ]
          
          adm4_names = tibble(sublvl_04 = unique(sublvl_03_subset$sublvl_04)) %>%
          mutate(
            sublvl_04_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
          )
          
          sublvl_03_subset <- sublvl_03_subset %>% left_join(adm4_names)
          
          area_codes_out <- rbind(area_codes_out, sublvl_03_subset)
      }
    }
  }
}

area_codes_out <- area_codes_out %>% dplyr::select(
  province_name, district_code, district_name,
  sublvl_01_code, sublvl_01,
  sublvl_02_code, sublvl_02,
  sublvl_03_code, sublvl_03,
  sublvl_04_code, sublvl_04,
  area_type
  ) %>%
  rowwise %>%
  mutate(
    uid = paste0(district_code, sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code)
  )

prov_area_out <- tibble()
for(n in 1:length(unique(provisional_areas$district))){
  district_name = unique(provisional_areas$district)[n]
  district_subset = provisional_areas[provisional_areas$district == district_name, ]

  adm1_names = tibble(sublvl_01 = unique(district_subset$sublvl_01)) %>% 
    mutate(
      sublvl_01_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )

  district_subset <- district_subset %>% left_join(adm1_names)

  for(m in 1:length(adm1_names$sublvl_01)){
    sublvl_1_code = unique(district_subset$sublvl_01_code)[m]
    sublvl_01_subset = district_subset[district_subset$sublvl_01_code == sublvl_1_code, ]
    
    adm2_names = tibble(sublvl_02 = unique(sublvl_01_subset$sublvl_02)) %>%
      mutate(
        sublvl_02_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
    
    sublvl_01_subset <- sublvl_01_subset %>% left_join(adm2_names)
    
    for(o in 1:length(adm2_names$sublvl_02)){
      sublvl_02_code = unique(sublvl_01_subset$sublvl_02_code)[o]
      sublvl_02_subset = sublvl_01_subset[sublvl_01_subset$sublvl_02_code == sublvl_02_code, ]
      
      adm3_names = tibble(sublvl_03 = unique(sublvl_02_subset$sublvl_03)) %>%
      mutate(
        sublvl_03_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
      
      sublvl_02_subset <- sublvl_02_subset %>% left_join(adm3_names)
      
      for(p in 1:length(adm3_names$sublvl_03)){
          sublvl_03_code = unique(sublvl_02_subset$sublvl_03_code)[p]
          sublvl_03_subset = sublvl_02_subset[sublvl_02_subset$sublvl_03_code == sublvl_03_code, ]
          
          adm4_names = tibble(sublvl_04 = unique(sublvl_03_subset$sublvl_04)) %>%
          mutate(
            sublvl_04_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
          )
          
          sublvl_03_subset <- sublvl_03_subset %>% left_join(adm4_names)
          
          prov_area_out <- rbind(prov_area_out, sublvl_03_subset)
      }
    }
  }
}

# Corrections to match provisional to final area names

prov_area_codes <- prov_area_out # %>% right_join(Pakistan_2017_Census_Provisional)
prov_area_codes$sublvl_04 <- str_to_upper(prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("M.C.", "MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("T.C.", "TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("CANTT.", "CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("CANTT", "CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("ATTC", "ATTOCK", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("ATTC", "ATTOCK", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("GHUR GHUSHTI TC", "GHUR GHUSHTI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("GAJJAR MASHKAY MC", "GUJAR MASHKAI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("AHMAD PUR MCOD GUNJ QH", "AHMAD PUR MECLOD GUNJ QH", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MCRP.", "MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_03 <- gsub("N0", "NO", prov_area_codes$sublvl_03)
prov_area_codes$sublvl_04 <- gsub("N0", "NO", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_04 <- gsub("BUD WANI WALA\\(QAID E AZAM SOLAR PARK\\) 637", "BUD WANI WALA(QAID E AZAM SOLAR PARK)", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_04 <- gsub("KOTHA BAFINDA \\(KOT IQBAL\\)", "KOT IQBAL", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("KHAIR PUR NATHAN SHAH TC", "KHAIR PUR NATHAN SHAH MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("\\(KANDH MURUNJ BORE\\) KANDH MURU 2,076", "(KANDH MURUNJ BORE) KANDH MURU", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("SUI TOWN MC", "SUI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("KOTCHUTTA", "KOT CHHUTTA", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("CHAK 008\\/JB PANJGARIANKAMALPUR 17,404", "CHAK 008/JB PANJGARIANKAMALPUR", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("KHURIAN WALA", "KHURIANWALA", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SAMMUNDARI", "SAMMUNDRI", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("CHAK 498\\/GB JOHKNOORMUQEEMANA 3,411", "CHAK 498/GB JOHKNOORMUQEEMANA", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("MCHALO STC", "MACHHALO STC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("QILA DEDAR SINGH MC", "QILA DIDAR SINGH MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("DHONKAL TC", "DHONKAL MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("GUJRAT MC", "GUJRAT MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("KEERAN - WALA KHAS", "KEERAN-WALA KHAS", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("KALEKE", "KALEKEY", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("JALAPUR BHATTIAN MC", "JALALPUR BHATTIAN MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHAHRUG MC", "SHAHRIG MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "HYDERABAD CITY TALUKA" & prov_area_codes$sublvl_02 == "HYDERABAD CANTONMENT"] <- "HYDERABAD CANTONMENT (PART OF HYDERABAD CITY TALUKA)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "HYDERABAD CITY TALUKA" & prov_area_codes$sublvl_02 == "HYDERABAD MCRP"] <- "HYDERABAD MUNICIPAL CORPORATION (PART OF HYDERABAD CITY TALUKA)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "LATIFABAD TALUKA" & prov_area_codes$sublvl_02 == "HYDERABAD CANTONMENT"] <- "HYDERABAD CANTONMENT (PART OF LATIFABAD TALUKA)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "LATIFABAD TALUKA" & prov_area_codes$sublvl_02 == "HYDERABAD MCRP"] <- "HYDERABAD MUNICIPAL CORPORATION (PART OF LATIFABAD TALUKA)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "QASIMABAD TALUKA" & prov_area_codes$sublvl_02 == "HYDERABAD CANTONMENT(PART)"] <- "HYDERABAD CANTONMENT (PART OF QASIMABAD TALUKA)"
prov_area_codes$sublvl_02 <- gsub("QASIMABAD M.C", "QASIMABAD MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("ISLAMABAD METROPOLITAN CORP", "ISLAMABAD METROPOLITAN CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MIRPUR BURRIRO", "MIRPUR BURRIRO TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("USTA MOHAMMAD MC", "USTA MUHAMMAD MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MCIWALA", "MOCHIWALA", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHORKOTCNTT", "SHORKOT CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHORKOT CANTONMENT QH", "SHORKOT CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHORKOTCTY", "SHORKOT CITY", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("JEHLUM MC", "JHELUM MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("JHELUMCNTT", "JHELUM CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MC MC", "MACH MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("KAMBAR TC", "KAMBAR MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("GULBERG", "GULBERG SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("GULBERG", "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF GULBERG SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("LIAQUATABAD", "LIAQUATABAD SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("LIAQUATABAD", "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF LIAQUATABAD SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "NAZIMABAD"] <- "NAZIMABAD SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "NAZIMABAD"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NAZIMABAD SUB-DIVISION)"
prov_area_codes$sublvl_01 <- gsub("NEW KARACHI", "NEW KARACHI SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("NEW KARACHI", "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NEW KARACHI SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("NORTH NAZIMABAD", "NORTH NAZIMABAD SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("NORTH NAZIMABAD", "DISTRICT MUNICIPAL CORPORATION KARACHI CENTRAL (PART OF NORTH NAZIMABAD SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("FEROZABAD", "FEROZABAD SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("FEROZABAD", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF FEROZABAD SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "GULSHAN-E-IQBAL"] <- "GULSHAN-E-IQBAL SUB-DIVISION"
prov_area_codes$sublvl_02 <- gsub("GULSHAN-E-IQBAL", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF GULSHAN-E-IQBAL SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "FAISAL CANTONMENT" & prov_area_codes$district == "KARACHI EAST DISTRICT"] <- "GULSHAN-E-IQBAL SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "FAISAL CANTONMENT" & prov_area_codes$district == "KARACHI EAST DISTRICT"] <- "FAISAL CANTONMENT (PART OF GULSHAN-E-IQBAL SUB-DIVISION)"
prov_area_codes$sublvl_01 <- gsub("GULZAR-E-HIJRI", "GULZAR-E-HIJRI SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("GULZAR-E-HIJRI", "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF GULZAR-E-HIJRI SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("ARAM BAGH", "ARAM BAGH SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("ARAM BAGH", "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF ARAM BAGH SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "CIVIL LINES"] <- "CIVIL LINES SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "CIVIL LINES"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF CIVIL LINES SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "CLIFTON CANTONMENT"] <- "CIVIL LINES SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "CLIFTON CANTONMENT"] <- "CLIFTON CANTONMENT (PART OF CIVIL LINES SUB-DIVISION)"
prov_area_codes$sublvl_01 <- gsub("GARDEN", "GARDEN SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("GARDEN", "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF GARDEN SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("LYARI", "LYARI SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("LYARI", "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF LYARI SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "SADDAR"] <- "SADDAR SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "SADDAR"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI SOUTH (PART OF SADDAR SUB-DIVISION)"
prov_area_codes$sublvl_01 <- gsub("BALDIA", "BALDIA SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("BALDIA", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF BALDIA SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("HARBOUR", "HARBOUR SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("HARBOUR", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF HARBOUR SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("MANGHOPIR", "MANGHOPIR SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("MANGHOPIR", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MANGHOPIR SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("MAURIPUR", "MAURIPUR SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("MAURIPUR", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MAURIPUR SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("MOMINABAD", "MOMINABAD SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("MOMINABAD", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF MOMINABAD SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "ORANGI" & prov_area_codes$district == "KARACHI WEST DISTRICT"] <- "ORANGI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "ORANGI" & prov_area_codes$district == "KARACHI WEST DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF ORANGI SUB-DIVISION)"
prov_area_codes$sublvl_01 <- gsub("SITE", "SITE SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("SITE", "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF S.I.T.E SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("MANORA CANTONMENT", "HARBOUR SUB-DIVISION", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("MANORA CANTONMENT", "MANORA CANTONMENT (PART OF HARBOUR SUB-DIVISION)", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("KASHMORE", "KASHMOR", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_03 <- gsub("KASHMORE", "KASHMOR", prov_area_codes$sublvl_03)
prov_area_codes$sublvl_04 <- gsub("KASHMORE", "KASHMOR", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_04 <- gsub("BUNGA SARDAR KAHAN SINGH\\(DEEN\\) 3,160", "BUNGA SARDAR KAHAN SINGH(DEEN)", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("PHOOL NAGAR TC", "PHOOL NAGAR MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("BULEDA MC", "BULAIDA MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("TUM MC", "TUMP MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("AJRA TC", "AGRA TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SOBHODERO TC", "SOBHO DERO TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04[prov_area_codes$district_name == "KHANEWAL DISTRICT" & prov_area_codes$sublvl_04 == "BAHAWAL PUR"] <- "BAHAWALPUR"
prov_area_codes$sublvl_02 <- gsub("NOORPUR THAL MC", "NOORPUR MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("ORNACH SUB-TEHSIl", "ORNACH SUB-TEHSIL", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("KOHATCNTONMENT", "KOHAT CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "ORANGI" & prov_area_codes$district == "KARACHI WEST DISTRICT"] <- "ORANGI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "ORANGI" & prov_area_codes$district == "KARACHI WEST DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI WEST (PART OF ORANGI SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "KORANGI" & prov_area_codes$district == "KORANGI DISTRICT"] <- "KORANGI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KORANGI" & prov_area_codes$district == "KORANGI DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF KORANGI SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "LANDHI" & prov_area_codes$district == "KORANGI DISTRICT"] <- "LANDHI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LANDHI" & prov_area_codes$district == "KORANGI DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF LANDHI SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "MODEL COLONY" & prov_area_codes$district == "KORANGI DISTRICT"] <- "MODEL COLONY SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "MODEL COLONY" & prov_area_codes$district == "KORANGI DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF MODEL COLONY SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "SHAH FAISAL" & prov_area_codes$district == "KORANGI DISTRICT"] <- "SHAH FAISAL SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "SHAH FAISAL" & prov_area_codes$district == "KORANGI DISTRICT"] <- "DISTRICT MUNICIPAL CORPORATION KORANGI (PART OF SHAH FAISAL SUB-DIVISION)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LAHORE METROPOLITAN CORP" & prov_area_codes$sublvl_01 == "LAHORE CANTT TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF LAHORE CANTONMENT TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LAHORE METROPOLITAN CORP" & prov_area_codes$sublvl_01 == "LAHORE CITY TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF LAHORE CITY TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LAHORE METROPOLITAN CORP" & prov_area_codes$sublvl_01 == "MODEL TOWN TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF MODEL TOWN TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KAHNA NAU TC" & prov_area_codes$sublvl_01 == "MODEL TOWN TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF MODEL TOWN TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LAHORE METROPOLITAN CORP" & prov_area_codes$sublvl_01 == "RAIWIND TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF RAIWIND TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "RAIWIND TC" & prov_area_codes$sublvl_01 == "RAIWIND TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF RAIWIND TEHSIL)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "LAHORE METROPOLITAN CORP" & prov_area_codes$sublvl_01 == "SHALIMAR TEHSIL"] <- "LAHORE METROPOLITAN CORPORATION (PART OF SHALIMAR TEHSIL)"
prov_area_codes$sublvl_02 <- gsub("LARKANA MCRP", "LARKANA MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("BORI TEHSIL", "LORALAI\\\\BORI TEHSIL", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_01 <- gsub("LORALAI TEHSIL", "LORALAI\\\\BORI TEHSIL", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_01[prov_area_codes$sublvl_02 == "AIRPORT" & prov_area_codes$sublvl_01 == "AIRPORT"] <- "AIRPORT SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "AIRPORT" & prov_area_codes$sublvl_01 == "AIRPORT SUB-DIVISION"] <- "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF AIRPORT SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_02 == "IBRAHIM HYDRI" & prov_area_codes$sublvl_01 == "IBRAHIM HYDRI"] <- "IBRAHIM HYDRI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "IBRAHIM HYDRI" & prov_area_codes$sublvl_01 == "IBRAHIM HYDRI SUB-DIVISION"] <- "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF IBRAHIM HYDRI SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_02 == "LANDHI STC" & prov_area_codes$sublvl_01 == "IBRAHIM HYDRI"] <- "IBRAHIM HYDRI SUB-DIVISION"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_02 == "KORANGI CREEK CANTONMENT" & prov_area_codes$sublvl_01 == "KORANGI CREEK CANTONMENT"] <- "IBRAHIM HYDRI SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KORANGI CREEK CANTONMENT" & prov_area_codes$sublvl_01 == "IBRAHIM HYDRI SUB-DIVISION"] <- "KORANGI CREEK CANTONMENT (PART OF IBRAHIM HYDRI SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "MURAD MEMON"] <- "MURAD MEMON SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "MURAD MEMON" & prov_area_codes$sublvl_01 == "MURAD MEMON SUB-DIVISION"] <- "DISTRICT MUNICIPAL CORPORATION MALIR (PART OF MURAD MEMON SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "SHAH MUREED" & prov_area_codes$district == "MALIR DISTRICT"] <- "SHAH MUREED SUB-DIVISION"
prov_area_codes$sublvl_02 <- gsub("MANDI BAHUDDIN MC", "MANDI BAHAUDDIN MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("ODEROLAL", "ODERO LAL", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MULTAN CANTONMENTQH", "MULTAN CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("KOHNA TAMBOO ANDROON\\(INTERNAL\\) 3,491", "KOHNA TAMBOO ANDROON(INTERNAL)", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("AMANGARH INDUSTRIAL AREA TC", "AMANGARH TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("CHERATCNTT.", "CHERAT CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("CHAK MANAK KAMBOH JAMUNWACCHAL2,957", "CHAK MANAK KAMBOH JAMUNWACCHAL", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("OKARA CANTONMENT QH", "OKARA CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("HARAM ZAI SUB-TEHSIL", "HURRAM ZAI SUB-TEHSIL", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("HURAMZAI MC", "HURRAM ZAI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("QUETTA MUNICIPAL CORPORATION", "QUETTA METROPOLITAN CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MCKA QH", "MACHKA QH", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("DAULTALA TC", "DAULTALA MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("\\(CHEHRIN ARAIN\\)KORAZADA MALYAR 2,437", "\\(CHEHRIN ARAIN\\)KORAZADA MALYAR", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("KALLAR SAYADDAN MC", "KALLAR SYEDAN MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("RAWALPINDI METROPOLITAN CORP", "RAWALPINDI MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SAHIWAL MCRP", "SAHIWAL MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("LILLIANI TC", "LILLIANI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_04 <- gsub("ANMAR MANGLAZAI\\(RAGHSAR MANGLA 2,732", "ANMAR MANGLAZAI(RAGHSAR MANGLA", prov_area_codes$sublvl_04)
prov_area_codes$sublvl_02 <- gsub("CHIWINDA TC", "CHAWINDA TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SIALKOTCNTT.", "SIALKOT CANTONMENT", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SIALKOT MUNICIPAL CORPORATIONRATION", "SIALKOT MUNICIPAL CORPORATION", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SOHBUT PUR MC", "SOHBATPUR MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "NEW SUKKUR TALUKA" & prov_area_codes$sublvl_02 == "SUKKUR MUNICIPAL CORPORATION"] <- "SUKKUR MUNICIPAL CORPORATION (PART OF NEW SUKKUR TALUKA)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "SUKKUR CITY TALUKA" & prov_area_codes$sublvl_02 == "SUKKUR MUNICIPAL CORPORATION"] <- "SUKKUR MUNICIPAL CORPORATION (PART OF SUKKUR CITY TALUKA)"
prov_area_codes$sublvl_02 <- gsub("MITHI TC", "MITHI MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHADIPALI STC", "SHADI PALI STC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("SHADIPALI TC", "SHADI PALI TC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02 <- gsub("MCHIAN WALA QH", "MACHHIAN WALA QH", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_01 <- gsub("SHAHOO GARHI SUB-TEHSIL", "SHAHGORI SUB-TEHSIL", prov_area_codes$sublvl_01)
prov_area_codes$sublvl_02 <- gsub("ZAIRAT MC", "ZIARAT MC", prov_area_codes$sublvl_02)
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "DARABAN TEHSIL" & prov_area_codes$sublvl_02 == "KOT JAI QH"] <- "CHAUDHAWAN QH"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "HARNAI TEHSIL" & prov_area_codes$sublvl_02 == "SHAHRIG MC"] <- "SHAHRIG TEHSIL"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KAMBER STC" & prov_area_codes$sublvl_03 == "ABRI TC"] <- "RANWATI STC"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KAMBER STC" & prov_area_codes$sublvl_03 == "KHAIRPUR JUSO TC"] <- "RANWATI STC"
prov_area_codes$district[prov_area_codes$sublvl_01 == "BHAG TEHSIL" & prov_area_codes$sublvl_02 == "BHAG MC"] <- "KACHHI DISTRICT"
#prov_area_codes$sublvl_04[prov_area_codes$sublvl_04 == "KALAR NO I|KALAR NO 2"] <- "KALAR"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_03 == "MOHD WARIS KEHAR TC"] <- "KANDHAR TC"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KERNAL SHER KILLI TC"] <- "COL. SHER KILLI/NAWAN KILLI TC"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "SANJAR CHANG STC"] <- "CHAMBER STC"
prov_area_codes$sublvl_04[prov_area_codes$sublvl_04 == "QADIR PURRAWAN SHARQI PC"] <- "QADIR PURRAWAN SHARQI"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "KARACHI CANTONMENT" & prov_area_codes$district == "KARACHI SOUTH DISTRICT"] <- "SADDAR SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "KARACHI CANTONMENT" & prov_area_codes$district == "KARACHI SOUTH DISTRICT"] <- "KARACHI CANTONMENT (PART OF SADAR SUB-DIVISION)"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "MALIR CANTONMENT" & prov_area_codes$district == "MALIR DISTRICT"] <- "AIRPORT SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "MALIR CANTONMENT" & prov_area_codes$district == "MALIR DISTRICT"] <- "MALIR CANTONMENT (PART OF AIRPORT SUB-DIVISION)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "UBAURO MC"] <- "UBAURO TC"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_02 == "DHADAR MC" & prov_area_codes$sublvl_03 == "CHARGE NO 11"] <- "CHARGE NO 09"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_02 == "MACH MC" & prov_area_codes$sublvl_03 == "CHARGE NO 12"] <- "CHARGE NO 10"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_02 == "BHAG MC" & prov_area_codes$sublvl_03 == "CHARGE NO 04"] <- "CHARGE NO 13"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_02 == "KAMBER STC" & prov_area_codes$sublvl_03 == "BAGODERO TC"] <- "BER TC"
prov_area_codes$sublvl_04[prov_area_codes$sublvl_04 == "KALAR NO I" & prov_area_codes$sublvl_03 == "KALAR TC"] <- "KALAR"
prov_area_codes$sublvl_04[prov_area_codes$sublvl_04 == "KALAR NO II" & prov_area_codes$sublvl_03 == "KALAR TC"] <- "KALAR"
prov_area_codes$sublvl_01[prov_area_codes$sublvl_01 == "JAMSHED QUARTERS" & prov_area_codes$sublvl_02 == "JAMSHED QUARTERS"] <- "JAMSHED QUARTERS SUB-DIVISION"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_01 == "JAMSHED QUARTERS SUB-DIVISION" & prov_area_codes$sublvl_02 == "JAMSHED QUARTERS"] <- "DISTRICT MUNICIPAL CORPORATION KARACHI EAST (PART OF JAMSHED QUARTERS SUB-DIVISION)"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "PIRJO GOTH TC"] <- "KINGRI (PIRJO GOTH) MC"
prov_area_codes$sublvl_04[prov_area_codes$sublvl_04 == "BAHAWAL PUR" & prov_area_codes$sublvl_03 == "CHAK SULTAN MAHMOOD PC"] <- "BAHAWALPUR"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "DARYA KHAN MARI TC"] <- "DARYA KHAN MARRI TC"
prov_area_codes$sublvl_03[prov_area_codes$sublvl_03 == "CHARGE NO 08." & prov_area_codes$sublvl_02 == "HURRAM ZAI MC"] <- "CHARGE NO 08"
prov_area_codes$sublvl_02[prov_area_codes$sublvl_02 == "NASIRPUR T.C"] <- "NASIRPUR TC"


code_check <- prov_area_codes %>% 
  dplyr::select(-c(sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code)) %>%
  left_join(area_codes_out) %>%
  filter(is.na(uid)) %>%
  filter(district != "KOHISTAN DISTRICT")

#code_check <- area_codes_out %>% 
#  dplyr::select(-c(sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code)) %>%
#  left_join(prov_area_codes %>% rowwise %>% mutate(uid = paste0(sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code))) %>%
#  filter(district != "KOHISTAN DISTRICT")
#
#code_check <- area_codes_out %>% left_join(prov_area_codes) %>% filter(is.na(sublvl_01))

code_join <- prov_area_codes %>% 
  rename(district_name = district) %>%
  mutate(provisional = "YES",
         district_name = ifelse(district_name == "LEHRI DISTRICT", "SIBI DISTRICT", district_name),
         district_name = ifelse(sublvl_01 == "BHAG TEHSIL", "KACHHI DISTRICT", district_name),
         sublvl_04 = ifelse(sublvl_04 %in% c("KALAR I", "KALAR II"), "KALAR", sublvl_04)         
         ) %>%
  left_join(area_codes %>% mutate(final = "YES"),
            ) %>%
  dplyr::select(province_name, district_name, district_code, provisional, final, area_type, 
                sublvl_01, sublvl_02, sublvl_03, sublvl_04) %>%
  mutate(final = ifelse(is.na(final), "NO", final)) %>%
  full_join(
    area_codes %>% mutate(final = "YES") %>% 
      left_join(
        prov_area_codes %>% 
          rename(district_name = district) %>%
          mutate(provisional = "YES",
                 district_name = ifelse(district_name == "LEHRI DISTRICT", "SIBI DISTRICT", district_name),
                 district_name = ifelse(sublvl_01 == "BHAG TEHSIL", "KACHHI DISTRICT", district_name),
                 sublvl_04 = ifelse(sublvl_04 %in% c("KALAR I", "KALAR II"), "KALAR", sublvl_04)
                 )
        ) %>% 
      mutate(provisional = ifelse(is.na(provisional), "NO", provisional)) %>%
      filter(provisional == "NO")
  ) %>%
  dplyr::select(-c(province_name, district_code)) %>%
  left_join(
    dplyr::select(area_codes_out, province_name, district_name, district_code) %>% unique()
  ) %>%
  dplyr::select(province_name, district_name, district_code, provisional, final, area_type, 
                sublvl_01, sublvl_02, sublvl_03, sublvl_04) %>%
  left_join(
    area_codes_out
  ) %>%
  arrange(
    district_code, sublvl_01, sublvl_02, sublvl_03, sublvl_04_code
  )

code_join$province_name[code_join$district_name == "KOHISTAN DISTRICT"] <- "KHYBER PAKTUNKHWA"
code_join$district_code[code_join$district_name == "KOHISTAN DISTRICT"] <- "010"

final_codes_renumber <- code_join %>% #filter(district_name != "KOHISTAN DISTRICT") %>%
  dplyr::select(-c(sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code, uid))

final_codes_out <- tibble()
for(n in 1:length(unique(final_codes_renumber$district_code))){
  district_code = unique(final_codes_renumber$district_code)[n]
  district_subset = final_codes_renumber[final_codes_renumber$district_code == district_code, ]

  adm1_names = tibble(sublvl_01 = unique(district_subset$sublvl_01)) %>% 
    mutate(
      sublvl_01_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )

  district_subset <- district_subset %>% left_join(adm1_names)

  for(m in 1:length(adm1_names$sublvl_01)){
    sublvl_1_code = unique(district_subset$sublvl_01_code)[m]
    sublvl_01_subset = district_subset[district_subset$sublvl_01_code == sublvl_1_code, ]
    
    adm2_names = tibble(sublvl_02 = unique(sublvl_01_subset$sublvl_02)) %>%
      mutate(
        sublvl_02_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
    
    sublvl_01_subset <- sublvl_01_subset %>% left_join(adm2_names)
    
    for(o in 1:length(adm2_names$sublvl_02)){
      sublvl_02_code = unique(sublvl_01_subset$sublvl_02_code)[o]
      sublvl_02_subset = sublvl_01_subset[sublvl_01_subset$sublvl_02_code == sublvl_02_code, ]
      
      adm3_names = tibble(sublvl_03 = unique(sublvl_02_subset$sublvl_03)) %>%
      mutate(
        sublvl_03_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
      )
      
      sublvl_02_subset <- sublvl_02_subset %>% left_join(adm3_names)
      
      for(p in 1:length(adm3_names$sublvl_03)){
          sublvl_03_code = unique(sublvl_02_subset$sublvl_03_code)[p]
          sublvl_03_subset = sublvl_02_subset[sublvl_02_subset$sublvl_03_code == sublvl_03_code, ]
          
          adm4_names = tibble(sublvl_04 = unique(sublvl_03_subset$sublvl_04)) %>%
          mutate(
            sublvl_04_code = str_pad(row_number(), width = 2, side = "left", pad = 0)
          )
          
          sublvl_03_subset <- sublvl_03_subset %>% left_join(adm4_names)
          
          final_codes_out <- rbind(final_codes_out, sublvl_03_subset)
      }
    }
  }
}

final_codes_out <- final_codes_out %>% dplyr::select(
  province_name, district_code, district_name,
  provisional, final, area_type,
  sublvl_01_code, sublvl_01,
  sublvl_02_code, sublvl_02,
  sublvl_03_code, sublvl_03,
  sublvl_04_code, sublvl_04,
  ) %>%
  rowwise %>%
  mutate(
    uid = paste0(district_code, sublvl_01_code, sublvl_02_code, sublvl_03_code, sublvl_04_code)
  ) %>% arrange(uid)

# Kalar I / II split corrections
final_codes_out$uid[final_codes_out$uid == "07701040503"] <- "07701040504"
final_codes_out$uid[final_codes_out$uid == "07701040502"] <- c("07701040502", "07701040503")
final_codes_out$sublvl_04_code[final_codes_out$uid == "07701040503"] <- "03"
final_codes_out$sublvl_04_code[final_codes_out$uid == "07701040504"] <- "04"

final_area_key <- final_codes_out %>% rename(area_code = uid)
write_csv(final_area_key, "./keyfiles/administrative_area_keyfile.csv", na = "")


prov_data <- prov_area_out %>% 
  cbind(prov_area_codes %>% 
          dplyr::select(sublvl_01, sublvl_02, sublvl_03, sublvl_04) %>%
          rename(lvl_01_corrected = sublvl_01,
                 lvl_02_corrected = sublvl_02,
                 lvl_03_corrected = sublvl_03,
                 lvl_04_corrected = sublvl_04) 
        ) %>%
  rowwise %>%
  mutate(
    corrected = ifelse(sublvl_01 != lvl_01_corrected | sublvl_02 != lvl_02_corrected | sublvl_03 != lvl_03_corrected | sublvl_04 != lvl_04_corrected, "YES", "NO")
  ) %>%
  left_join(
    Pakistan_2017_Census_Provisional %>%
      dplyr::select(sublvl_01, sublvl_02, sublvl_03, sublvl_04, census_block, population, households, avg_hh_size)
    )


final_codes_with_cb <- final_codes_out %>%
  left_join(
    prov_data %>% dplyr::select(lvl_01_corrected, lvl_02_corrected, lvl_03_corrected, lvl_04_corrected, corrected,
                                census_block, population, households, avg_hh_size) %>%
      rename(sublvl_01 = lvl_01_corrected,
             sublvl_02 = lvl_02_corrected,
             sublvl_03 = lvl_03_corrected,
             sublvl_04 = lvl_04_corrected,
             provisional_population = population,
             provisional_households = households,
             provisional_avg_hh = avg_hh_size)
  )

final_keycodes <- final_codes_with_cb %>% 
  dplyr::select(-c(provisional_population, provisional_households, provisional_avg_hh, corrected)) %>%
  dplyr::select(census_block, everything()) %>% rename(
    area_code = uid
  )

write_csv(final_keycodes, "./keyfiles/census_block_administrative_area_keyfile.csv", na = "")


final_data <- final_codes_out %>% left_join(
  natl_table_23 %>% 
    filter(summary_row != "YES") %>%
    dplyr::select(district_code, sublvl_01, sublvl_02, sublvl_03, area_name, pop_all, pop_male, pop_fem, pop_trans) %>%
    rename(sublvl_04 = area_name) %>%
    mutate(area_type = "RURAL") %>%
    full_join(
      natl_table_25 %>%
        filter(summary_row != "YES") %>%
        dplyr::select(district_code, sublvl_01, sublvl_02, sublvl_03, area_name, pop_all, pop_male, pop_fem, pop_trans) %>%
        rename(sublvl_04 = area_name) %>%
        mutate(area_type = "URBAN")
    )
  ) %>%
  left_join(
    prov_data %>% group_by(lvl_01_corrected, lvl_02_corrected, lvl_03_corrected, lvl_04_corrected) %>%
      summarize(
        pop_provisional = sum(population, na.rm = T),
        hh_provisional = sum(households, na.rm = T)
      ) %>%
      rename(sublvl_01 = lvl_01_corrected,
             sublvl_02 = lvl_02_corrected,
             sublvl_03 = lvl_03_corrected,
             sublvl_04 = lvl_04_corrected)
  ) %>%
  rowwise %>% 
  mutate(provisional_avg_hh = pop_provisional / hh_provisional,
         net_change = pop_all - pop_provisional)


write_csv(final_data, "./validity_checks/provisional_final_census_comparison.csv", na = "")



