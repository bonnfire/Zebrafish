## create csv for breeders
## mastertable 

## phenotypes 
plates_df_1 %>% 
  select(fish_id, num_larvae) %>% 
  mutate(fish_oid)
write.csv(plates_df_1, "guo_breeders_plate1.csv", row.names)

# for db - sample_tracking.sample_metadata
breeder_plates_df_db <- breeder_plates_df %>% 
  distinct(fish_id, sex) %>% 
  mutate(coatcolor = "NA", 
         project_name = "r01_su_guo_breeders", 
         organism = "zebrafish",
         strain = "Ekkwill fish", 
         comments = "NA") %>% 
  rename("rfid" = "fish_id")
write.csv(breeder_plates_df_db, "~/Desktop/Database/csv files/sample_tracking/zebrafish_breeder_n350.csv", row.names = F)

read.csv( "~/Desktop/Database/csv files/sample_tracking/zebrafish_breeder_n350.csv", stringsAsFactors = F) %>%
  select(rfid, sex) %>% 
  write.csv("~/Desktop/Database/csv files/r01_su_guo_breeders/zebrafish_breeder_n350.csv", row.names = F)


## list of breeders that have been deep seqed
# separate submissions, read in and combine
breeders_deepseq <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/2020_06_09_Palmer_Library_Manifest_10_2019.xlsx") %>% 
  mutate_all(as.character)
breeders_deepseq <- breeders_deepseq[20:nrow(breeders_deepseq), 1]

breeders_deepseq_2 <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/16breeders_IGM20200402.csv") %>% 
  mutate_all(as.character)
breeders_deepseq_2 <- breeders_deepseq_2[, "Breeder_ID"] %>% as.character

deepseq_breeders <- rbind(data.frame(breeder_id = breeders_deepseq), data.frame(breeder_id = breeders_deepseq_2)) %>% 
  mutate(breeder_id = gsub("-", "_", breeder_id))
