## CREATE EXCEL FROM SU GUO'S GOOGLE DOCS LINK

# setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

library(gsheet)

Zebrafish_Guo_xl_orig <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=155483271')
Zebrafish_Guo_xl <- Zebrafish_Guo_xl_orig %>%
  clean_names() %>% 
  subset(grepl("Plate|-", fish_id)) %>% 
  subset(!is.na(date_of_beh_testing))

Zebrafish_Guo_xl %>% get_dupes(fish_id) %>% subset(!is.na(ld_choice_index))%>% select(well_id, fish_id) 
