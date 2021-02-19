## CREATE EXCEL FROM SU GUO'S GOOGLE DOCS LINK

# setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

library(gsheet)

Zebrafish_Guo_xl_orig <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=155483271')
Zebrafish_Guo_xl_wcleannames <- Zebrafish_Guo_xl_orig %>%
  clean_names() 

Zebrafish_Guo_exclude <- Zebrafish_Guo_xl_wcleannames %>% # exclude the animals that do not have dna collected
  subset(grepl("Plate", fish_id, ignore.case = T)) %>% 
  mutate(dna_collected_y_n = toupper(dna_collected_y_n),
         dna_collected_y_n = replace(dna_collected_y_n, dna_collected_y_n == "Y\nY\nY\nY", "Y")) %>% 
  subset(dna_collected_y_n == "N") %>% 
  mutate(rfid = gsub("-", "_", fish_id))

## read in guo maintained sheet for casper
plates_casper_xl <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=1398440230')
plates_casper_xl_df <- plates_casper_xl %>% 
  clean_names() 
plates_casper_xl_df <- plates_casper_xl_df %>% 
  mutate(casper = ifelse(grepl("No casper", comment)|grepl("20201215|20201222", plate_id)|grepl("FC20200305|Fin", plate_id), 0, 1)) # exception for grepl plate_id as noted in Slack

gh_12 <- Zebrafish_Guo_xl %>% 
  distinct(unique_plate_id) %>% 
  slice(rep(1:n(), each = 2)) %>% 
  cbind(data.frame(letter = rep(c("12G","12H"), each = 1, times =35))) %>% 
  mutate(letter = as.character(letter)) %>% 
  mutate(rfid = paste0(unique_plate_id, "-", letter)) %>% 
  full_join(plates_casper_xl_df, by = c("unique_plate_id" = "plate_id")) %>% 
  # mutate(unique_date = gsub("-.*", "", unique_plate_id)) %>% 
  # left_join(plates_casper, by = "unique_date") %>% 
  left_join(Zebrafish_Guo_xl %>% select(fish_id, mother, father, dna_collected_y_n), by = c("rfid" = "fish_id")) %>% 
  mutate(comment = ifelse(grepl("20191209-Plate1|20191217-Plate1", unique_plate_id), "these plates' caspers were placed in Plate2 as 12E and 12F", comment),
         casper = ifelse(grepl("20191209-Plate1|20191217-Plate1", unique_plate_id), "0", casper),
         casper = ifelse(grepl("20191209-Plate2|20191217-Plate2", unique_plate_id), "1", casper))

# add the 12E and 12F cases for four plates 
gh_12 <- gh_12 %>% 
  rbind(gh_12 %>% 
          subset(grepl("20191209-Plate2|20191217-Plate2", unique_plate_id)) %>% 
          mutate(letter = replace(letter, rfid == "20191209-Plate2-12G", "12E"),
                 rfid = replace(rfid, rfid == "20191209-Plate2-12G", "20191209-Plate2-12E"), 
                 letter = replace(letter, rfid == "20191209-Plate2-12H", "12F"), 
                 rfid = replace(rfid, rfid == "20191209-Plate2-12H", "20191209-Plate2-12F"), 
                 letter = replace(letter, rfid == "20191217-Plate2-12G", "12E"), 
                 rfid = replace(rfid, rfid == "20191217-Plate2-12G", "20191217-Plate2-12E"), 
                 letter = replace(letter, rfid == "20191217-Plate2-12H", "12F"), 
                 rfid = replace(rfid, rfid == "20191217-Plate2-12H", "20191217-Plate2-12F"))) %>%
  mutate(casper = ifelse(grepl("20200114-Plate|20200121-Plate|20200721-Plate|20191209-Plate2|20191217-Plate2", rfid), 1, casper),
         casper = ifelse(grepl("20200811", rfid), 0, casper)) %>% 
  mutate(gh_cat = case_when(
    casper == 0 & !is.na(mother)&!is.na(father)&!grepl("FC20200305|Fin", unique_plate_id) ~ "EK fish",
    casper == 0 & is.na(mother)&is.na(father)&grepl("FC20200305|Fin", unique_plate_id) ~ "parent DNA",
    casper == 1 ~ "casper", 
    casper == 0 & is.na(mother)&is.na(father) ~ "empty well", 
    TRUE ~ NA_character_
  )) 
  
openxlsx::write.xlsx(gh_12, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/gh_classification_n147.xlsx")
gh_12$gh_cat %>% table()


read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/db_samplebarcodelib_02102021_n4403.csv") %>% mutate_all(as.character) %>% subset(grepl("guo", project_name)&library_name == "UMich08_Fish") %>% mutate(comment = "play set")
  

exclude_all <- Zebrafish_Guo_exclude %>% select(rfid) %>% mutate(comment = "no dna") %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/db_samplebarcodelib_02102021_n4403.csv") %>% mutate_all(as.character) %>% subset(grepl("guo", project_name)&library_name == "UMich08_Fish") %>% select(rfid) %>% mutate(comment = "play set")) %>% 
  rbind(gh_12 %>% subset(gh_cat %in% c("casper", "empty well")) %>% select(rfid, "comment" = gh_cat))

  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/exclude_zebrafish_n161.csv", row.names = F)




# read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/db_samplebarcodelib_02102021_n4403.csv") %>% mutate_all(as.character) %>% subset(grepl("Plate", rfid)) %>% mutate(rfid = gsub("-", "_", rfid)) %>% anti_join(Zebrafish_Guo_xl, by = "rfid") 
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/db_samplebarcodelib_02102021_n4403.csv") %>% mutate_all(as.character) %>% subset(grepl("guo", project_name)) %>% mutate(rfid = gsub("-", "_", rfid)) %>% anti_join(Zebrafish_Guo_xl, by = "rfid")
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/db_samplebarcodelib_02102021_n4403.csv") %>% mutate_all(as.character) %>% subset(grepl("guo", project_name)) %>% mutate(rfid = gsub("-", "_", rfid)) %>% anti_join(Zebrafish_Guo_xl, by = "rfid") %>% subset(!grepl("^Z2622", rfid)) %>% select(rfid, barcode, library_name, pcr_barcode, filename) %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/exclude_zebrafish_uptokn04_n81.csv", row.names = F)

Zebrafish_Guo_xl <- Zebrafish_Guo_xl_wcleannames %>% # start subsetting to get larvae phenotype
  subset(grepl("Plate", fish_id)) %>% 
  # subset(!is.na(date_of_beh_testing)) %>% 
  # mutate(fish_id = gsub("-", "_", fish_id), #to match the flowcell 09/22/2020 EVERYTHING SHOULD MATCH PHENOTYPES 
  #        fish_id = paste0(gsub("_\\d+", "_", fish_id), str_extract(fish_id, "[^_]+(?=\\D$)")),
         mutate(dna_collected_y_n = toupper(dna_collected_y_n),
         dna_collected_y_n = replace(dna_collected_y_n, dna_collected_y_n == "Y\nY\nY\nY", "Y")) %>% 
  mutate(rfid = gsub("-", "_", fish_id))

Zebrafish_Guo_xl %>% get_dupes(fish_id) %>% subset(!is.na(ld_choice_index))%>% select(well_id, fish_id) 

Zebrafish_Guo_xl %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/zebrafish_larvae_n6580_phenotypes.csv", row.names = F)

# source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/EXTRACT_SAMPLEBARCODELIBRARY.R")
# flowcell_df %>% subset(grepl("Plate", sample_id_demul)) %>% left_join(Zebrafish_Guo_xl, ., by = c("fish_id"= "rfid"))



# submitted plates
plates <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=1853834046")

convertplate.todf <- function(x){
  df_total <- data.frame()
  for(i in seq(1, 36, by = 3)){
    row <- x[i:(i+2)] 
    df <- data.frame(row)
    colnames(df) <- c("fish_id", "dna_conc", "num_larvae")
    df_total <- rbind(df_total,df)
  }
  return(df_total)
}


plates_df_1 <- convertplate.todf(plates[6:13, c(2:37)]) 
plates_df_2 <- convertplate.todf(plates[17:24, c(2:37)])
plates_df_3 <- convertplate.todf(plates[28:35, c(2:37)])
plates_df_4 <- convertplate.todf(plates[39:46, c(2:37)])




# current breeders
Zebrafish_breeders <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=1101471263')
Zebrafish_breeders_df <- Zebrafish_breeders %>% 
  clean_names %>% 
  gather("sex", "rfid_genotype", -alternative_id, -x4, -x5, -x6) %>% 
  mutate(rfid = gsub("(.*-\\D\\d+)\\D", "\\1", rfid_genotype)) %>%  # to join
  mutate(rfid_id = gsub(".*-", "", rfid_genotype),
         rfid_idstripped = gsub(".*-", "", rfid))

