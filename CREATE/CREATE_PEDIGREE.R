## extract and process pedigree
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
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/pedigree_fish_n6016_20210114.csv", row.names = F)

