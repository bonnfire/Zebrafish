
setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

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
