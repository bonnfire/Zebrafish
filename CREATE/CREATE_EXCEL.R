## CREATE EXCEL FROM SU GUO'S GOOGLE DOCS LINK

# setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

library(gsheet)

Zebrafish_Guo_xl_orig <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=155483271')
Zebrafish_Guo_xl <- Zebrafish_Guo_xl_orig %>%
  clean_names() %>% 
  subset(grepl("Plate|-", fish_id)) %>% 
  subset(!is.na(date_of_beh_testing)) %>% 
  mutate(fish_id = gsub("-", "_", fish_id), #to match the flowcell
         fish_id = paste0(gsub("_\\d+", "_", fish_id), str_extract(fish_id, "[^_]+(?=\\D$)")),
         dna_collected_y_n = toupper(dna_collected_y_n),
         dna_collected_y_n = replace(dna_collected_y_n, dna_collected_y_n == "Y\nY\nY\nY", "Y"))

Zebrafish_Guo_xl %>% get_dupes(fish_id) %>% subset(!is.na(ld_choice_index))%>% select(well_id, fish_id) 

# source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/EXTRACT_SAMPLEBARCODELIBRARY.R")
# flowcell_df %>% subset(grepl("Plate", sample_id_demul)) %>% left_join(Zebrafish_Guo_xl, ., by = c("fish_id"= "rfid"))
