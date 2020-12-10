
setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

## breeders that have been deep seq'ed as well as the larvae
oksana_fish_progress12092020_sample_bar_lib <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/oksana_fish_progress12092020_sample_bar_lib.csv") %>% 
  mutate_all(as.character) %>% 
  mutate(rfid = gsub("_", "-", rfid),
         rfid = gsub(" ", "", rfid)) %>% 
  select(larva_breeder, rfid, library_name) %>% 
  left_join(Zebrafish_Guo_xl %>% 
              mutate(fish_id = gsub(" ", "", fish_id)), by = c("rfid" = "fish_id")) %>% 
  left_join(plates_df_1, by = c("rfid" = "fish_id")) %>% 
  left_join(deep_8larvae %>% select(rfid) %>% 
              mutate(rfid = gsub("_", "-", rfid)) %>% 
              rbind(larvae_breeders %>% distinct(breeder_id) %>% subset(grepl("Z", breeder_id)) %>% rename("rfid" = "breeder_id")) %>% mutate(library_prep = "Kapa Hyper"), by = "rfid")

oksana_fish_progress12092020_sample_bar_lib_final <- oksana_fish_progress12092020_sample_bar_lib %>% 
  rbind(oksana_fish_progress12092020_sample_bar_lib %>% 
          subset(library_prep == "Kapa Hyper") %>% 
          mutate(library_prep = "Riptide")) %>% 
  mutate(library_prep = replace(library_prep, is.na(library_prep), "Riptide")) %>% 
  mutate(library_name = replace(library_name, library_prep == "Kapa Hyper", "NA")) %>% 
  select(larva_breeder, rfid, library_name, library_prep, everything()) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  subset(!is.na(larva_breeder))

openxlsx::write.xlsx(oksana_fish_progress12092020_sample_bar_lib_final, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/oksana_fish_progress12092020.xlsx")



## CREATE EXTRACTION TABLE
# 2) the list of larva that are the offspring of at least one of the breeders, with both breeders for each larva listed.
larvae_breeders <- read.csv(file = "16breeders+larvae_IGM20200402.csv") %>% 
  clean_names

# 07/15/2020
# Oksana: provide Riyan the list of larvae that have been sequenced and have at least one of the parents deep-sequenced
source("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/EXTRACT_SAMPLEBARCODELIBRARY.R")
larvae_breeders %>%
  mutate_all(as.character) %>% 
  subset(breeder_f %in% sequenced_breeders$breeder_id | breeder_m %in% sequenced_breeders$breeder_id) %>% 
  subset(breeder_f !="Z2622-F2a") %>% 
  mutate(larva_id = gsub("-", "_", larva_id), #to match the flowcell
         larva_id = paste0(gsub("_\\d+", "_", larva_id), str_extract(larva_id, "[^_]+(?=\\D$)"))) %>% 
  subset(larva_id %in% flowcell_df$rfid) %>% 
  openxlsx::write.xlsx("sequenced_larvae_20200715.xlsx")

## SENT TO SEQUENCING CORE
## sending founders to be sequences
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE")
sequenced_breeders <- read.csv(file = "16breeders_IGM20200402.csv") %>% 
  clean_names
