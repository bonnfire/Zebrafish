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
