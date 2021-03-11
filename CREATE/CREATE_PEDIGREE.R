## extract and process pedigree


## process excel object from google sheets 
pedigree_ggl <- Zebrafish_Guo_xl_wcleannames %>% 
  subset(grepl("Plate", fish_id, ignore.case = T)) %>% 
  distinct(fish_id, dna_collected_y_n, mother, father) %>% 
  mutate_all(~gsub("-", "_", .)) %>% 
  mutate(replace = ifelse(father %in% c("Z2622_M35", "Z2622_M34"), "yes", "no")) %>% 
  mutate(replace = replace(replace, father == "Z2622_M35", "no"),
    father = replace(father, father == "Z2622_M35", "Z2622_M34"),
    father = replace(father, father == "Z2622_M34"&replace == "yes", "Z2622_M35")) %>% 
  select(-replace)

pedigree_ggl %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/csv files/pedigree_fish_n6580_20210114.csv", row.names = F)



pedigree_fish <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish/pedigree_fish.csv") %>% 
  mutate_all(as.character) 

Zebrafish_Guo_xl 

pedigree_fish %>%
  get_dupes(id) %>% 
  left_join(Zebrafish_Guo_xl[, c("fish_id", "mother", "father")] %>% 
              mutate_all(~gsub("-", "_", .)), by = c("id"="fish_id")) %>% 
  subset(mother.x != mother.y & father.x != father.y) %>% 
  select(id, "mother" = mother.x, "father" = father.y) %>% 
  anti_join(pedigree_fish, .) %>% dim

            
pedigree_fish %>%
  left_join(Zebrafish_Guo_xl[, c("fish_id", "mother", "father")] %>% 
              mutate_all(~gsub("-", "_", .)), by = c("id"="fish_id")) %>% 
  subset(mother.x == mother.y & father.x == father.y) %>% 
  select(id, "mother" = mother.x, "father" = father.y) %>%
  # subset(mother %in% unique(.$father)|father %in% unique(.$mother)) %>% # check if there are swaps
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/pedigree_fish_n3568_20210114.csv", row.names = F)

Zebrafish_Guo_xl %>% 
  distinct(fish_id, mother, father) %>% 
  mutate_all(~gsub("-", "_", .)) %>% 
  # subset(mother %in% unique(.$father)|father %in% unique(.$mother)|mother == father) %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_01052021.csv") %>% mutate_all(as.character) %>% 
          subset(grepl("guo", project_name)) %>% mutate(rfid = gsub("-", "_", rfid)) %>% select("fish_id" = rfid) %>% mutate(mother = "NA", father = "NA") %>% 
          subset(!(fish_id %in% gsub("-", "_", Zebrafish_Guo_xl$fish_id))),.) %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/pedigree_fish_n6187_20210114.csv", row.names = F)

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/pedigree_fish_n6187_20210114.csv") %>% 
  mutate_all(as.character)

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_01052021.csv") %>% mutate_all(as.character) %>% subset(grepl("guo", project_name)) %>% mutate(rfid = gsub("-", "_", rfid)) %>% left_join(Zebrafish_Guo_xl %>% 
                                                                                                                                                                                                                                                          distinct(fish_id, mother, father) %>% 
                                                                                                                                                                                                                                                          mutate_all(~gsub("-", "_", .)), by = c("rfid" = "fish_id")) %>% subset(is.na(mother)|is.na(father)) %>% select(rfid) %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/QC/genotyped_no_pedi_n171.csv", row.names = F)