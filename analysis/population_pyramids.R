library(tidyverse)
rm(list = ls())
table_04_consolidated <- read_csv("~/Google Drive/GitHub/pakistan_census/processed_forms/CONSOLIDATED NATIONAL TABLES/table_04_consolidated.csv")

table_04_restructured <- table_04_consolidated %>% 
  dplyr::select(-c(group_all_ages, group_00_04, group_05_09, group_10_14, group_15_19, 
                   group_20_24, group_25_29, group_30_34, group_35_39,
                   group_40_44, group_45_49, group_50_54, group_55_59,
                   group_60_64, group_65_69, group_70_74)) %>%
  filter(area_name != district_name & area_type == "TOTAL" & area_name != "ISLAMABAD DISTRICT")

table_04_tidied <- tibble()
for(i in 1:length(table_04_restructured$area_name)){
  row_header <- table_04_restructured[i,1:6]
  age_out <- tibble()
  for(n in 7:82){
    age_cell <- tibble(age = colnames(table_04_restructured[i,n]),
                       count = table_04_restructured[[i,n]])
    
    age_row <- cbind(row_header, age_cell)
    age_out <- rbind(age_out, age_row)
  }
  table_04_tidied <- rbind(table_04_tidied, age_out)
}

table_04_tidied$age[table_04_tidied$age == "age_below_1"] <- "< 1"
table_04_tidied$age <- gsub("age_", "", table_04_tidied$age)
table_04_tidied$age <- gsub("group_75_and_up", "75 +", table_04_tidied$age)


for(x in 1:length(unique(table_04_tidied$district_code))){
  district_target <- unique(table_04_tidied$district_code)[x]
  district_name <- unique(table_04_tidied$district_name[table_04_tidied$district_code == district_target])
  province_target <- unique(table_04_tidied$province_name[table_04_tidied$district_code == district_target])
  district_path <- paste0("./analysis/population_pyramids/", province_target, "/")
  district_pop = sum(table_04_tidied$count[table_04_tidied$gender == "ALL SEXES" & table_04_tidied$district_code == district_target], na.rm = T)
  district_male = sum(table_04_tidied$count[table_04_tidied$gender == "MALE" & table_04_tidied$district_code == district_target], na.rm = T)
  district_fem = sum(table_04_tidied$count[table_04_tidied$gender == "FEMALE" & table_04_tidied$district_code == district_target], na.rm = T)
  district_trans = sum(table_04_tidied$count[table_04_tidied$gender == "TRANSGENDER" & table_04_tidied$district_code == district_target], na.rm = T)

  facet_plot <- ggplot(data = subset(
    table_04_tidied, gender %in% c("MALE", "FEMALE") & district_code == district_target),
       aes(x = age, fill = gender,
                 y = ifelse(test = gender == "MALE",
                            yes = -count, no = count))) + 
    facet_wrap(~area_name) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(
      labels = abs, 
      limits = max(table_04_tidied$count[table_04_tidied$gender %in% c("MALE", "FEMALE") & table_04_tidied$district_code == district_target]) * c(-1,1)
      ) +
    labs(
      title = paste0("2017 CENSUS POPULATION PYRAMID - ", province_target, " - ", district_name),
      subtitle = paste0("Total District Population: ", district_pop, " | Total Male: ", district_male, " | Total Female: ", district_fem, " | Total Transgender: ", district_trans),
      fill = "Gender (Transgender Counts Omitted)",
      caption = "Data Source: https://github.com/colincookman/pakistan_census",
      y = "Census Count",
      x = "Age"
    ) +
    theme(
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    ) +
  coord_flip()

    
  # create the appropriate province subdirectory if it doesn't already exist                   
  ifelse(!dir.exists(file.path(district_path)), dir.create(file.path(district_path)), FALSE)
  
  ggsave(paste0(district_path, district_target, " - ", district_name, ".png"), 
         plot = facet_plot, dpi = 320, height = 11.7, width = 16.5, units = "in", limitsize = F)

}

