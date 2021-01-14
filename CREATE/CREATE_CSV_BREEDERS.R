## create csv for breeders
## mastertable 

## phenotypes 
plates_df_1 %>% 
  select(fish_id, num_larvae) %>% 
  mutate(fish_oid)
write.csv(plates_df_1, "guo_breeders_plate1.csv", row.names)
