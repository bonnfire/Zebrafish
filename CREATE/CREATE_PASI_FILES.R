## create files for Pasi collaboration
# You will need to
# (1) remove 4 bad families
# (2) change M34 <->M35 for 3 families where switching fixed MER
# (3) for the remaining families, remove larva with MER >0.05
# (4) in the resulting list, remove all families which have less than 3 larva offspring


genotyped_fish_mer <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/fishes_QC_MER_20210224.csv") %>% 
  mutate_all(as.character) %>% 
  clean_names 

genotyped_fish_subset <- genotyped_fish_mer %>% 
  subset(!((father == "Z2622_M35" & mother == "Z2622_F32")|
             (father == "Z2622_M26a" & mother == "Z2622_F20")|
             (father == "Z2622_M38" & mother == "Z2622_F32")|
             (father == "Z2622_M39" & mother == "Z2622_F32")))

genotyped_fish_subset_replace = genotyped_fish_subset %>% 
  mutate(replace = ifelse(father %in% c("Z2622_M35", "Z2622_M34"), "yes", "no")) %>% 
  mutate(replace = replace(replace, father == "Z2622_M34", "no"), 
        father = replace(father, father == "Z2622_M34", "Z2622_M35"),
         father = replace(father, father == "Z2622_M35"&replace == "yes", "Z2622_M34")) 

genotyped_fish_subset_replace_mer <- genotyped_fish_subset_replace %>% 
  subset(as.numeric(mendelian_error_rate) < 0.05 & !(father %in% c("Z2622_M35", "Z2622_M34"))) %>% 
  add_count(father, mother) %>% 
  subset(n >= 3) %>% 
  select(larva_id, father, mother)

write.csv(genotyped_fish_subset_replace_mer, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/csv files/pasi_larvae_parents_n1247.csv", row.names = F)
