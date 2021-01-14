## CREATE EXCEL FROM SU GUO'S GOOGLE DOCS LINK

# setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

library(gsheet)

Zebrafish_Guo_xl_orig <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1NMMBpsPf4VDDckpZL2Qwrro5f3hqXpCTGGYYFKP4Hxc/edit#gid=155483271')
Zebrafish_Guo_xl_wcleannames <- Zebrafish_Guo_xl_orig %>%
  clean_names() 

Zebrafish_Guo_xl <- Zebrafish_Guo_xl_wcleannames %>% # start subsetting to get larvae phenotype
  subset(grepl("Plate", fish_id)) %>% 
  # subset(!is.na(date_of_beh_testing)) %>% 
  # mutate(fish_id = gsub("-", "_", fish_id), #to match the flowcell 09/22/2020 EVERYTHING SHOULD MATCH PHENOTYPES 
  #        fish_id = paste0(gsub("_\\d+", "_", fish_id), str_extract(fish_id, "[^_]+(?=\\D$)")),
         mutate(dna_collected_y_n = toupper(dna_collected_y_n),
         dna_collected_y_n = replace(dna_collected_y_n, dna_collected_y_n == "Y\nY\nY\nY", "Y"))

Zebrafish_Guo_xl %>% get_dupes(fish_id) %>% subset(!is.na(ld_choice_index))%>% select(well_id, fish_id) 

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

