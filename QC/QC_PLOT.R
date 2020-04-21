## QC and PLOT

# create a column for dead fish
Zebrafish_formats_cellbycell <- tidyxl::xlsx_cells("20190819- ZF EK GWAS database copy.xlsx") %>% 
  dplyr::filter(sheet == "Summary")
Zebrafish_formats <- tidyxl::xlsx_formats("20190819- ZF EK GWAS database copy.xlsx")

redhexa <- Zebrafish_formats$local$fill$patternFill$bgColor$rgb[Zebrafish_formats_cellbycell[which(grepl("Red", Zebrafish_formats_cellbycell$character)),]$local_format_id]
# nonblackhexa_indices <- which(Zebrafish_formats$local$font$color$rgb == redhexa)
dead_zebrafish_indices <- Zebrafish_formats_cellbycell[Zebrafish_formats_cellbycell$local_format_id %in% which(Zebrafish_formats$local$fill$patternFill$bgColor$rgb == redhexa), ] %>% 
  # dplyr::filter(data_type != "blank") %>% 
  subset(col == 2) %>% 
  select(character) %>% 
  unlist() %>% 
  as.vector()
# Zebrafish_formats_cellbycell %>% subset(col == 1 & row %in% dead_zebrafish_indices)

# filter out fish without data 
Zebrafish_Guo_xl_bev <- Zebrafish_Guo_xl %>% 
  mutate_at(vars(-one_of("z_number", "fish_id", "dna_collected_y_n", "unique_plate_id", "well_id", "date_of_birth", "date_of_beh_testing", "cross_no", "mother", "father", "gender", "date_of_shipping_to_ucsd")), as.numeric) %>% 
  subset(!is.na(ld_choice_index)) 


library(purrr)
library(tidyr)
library(ggplot2)


Zebrafish_Guo_xl_bev %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

Zebrafish_Guo_xl_bev %>% 
  select(one_of(zebrafish_graph_vars)) %>% 
  gather(key, value, -date_of_beh_testing) %>% 
  subset(!is.na(value)) %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(aes(color = date_of_beh_testing))


zebrafish_graph_vars <- c("date_of_beh_testing", "ld_choice_index", "thigmotaxis_index","entry_to_dark_number_center_point_frequency",
"entry_to_light_number_center_point_frequency","ave_duration_dark_entry_cum_duration_frequency_seconds",
"ave_duration_light_entry_cum_duration_frequency_seconds","latency_to_enter_dark_latency_to_first","latency_to_enter_light_latency_to_first",
"beh_flexibility_index_ci_last_30s_ci_first_30s","variance_of_ci_during_test_period","speed_in_dark_center_point_mean","speed_in_light_center_point_mean",
"body_length", "brain_length", "brain_width", "ucsd_genotyping_age")

library(corrplot)
Zebrafish_Guo_xl_bev_cor <- Zebrafish_Guo_xl_bev
names(Zebrafish_Guo_xl_bev_cor) <- gsub("point|frequency|duration|seconds|", "", names(Zebrafish_Guo_xl_bev_cor)) %>% gsub("_{2,3}", "", .)
M<-cor(Zebrafish_Guo_xl_bev_cor %>% select(which(colSums(is.na(.)) == 0)) %>% select_if(is.numeric))
corrplot(M, method="circle")

Zebrafish_Guo_xl_bev %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)

Zebrafish_Guo_xl %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)


setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/QC")
Zebrafish_Guo_xl %>% get_dupes(fish_id) %>% as.data.frame() %>% openxlsx::write.xlsx(., "Duplicated_FishIDs.xlsx")



## QC 
## check if all id's are represented in the extraction and in the flowcell


Zebrafish_Guo_xl

# extraction


# flowcell

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues")
flowcell_files <- list.files(path = ".", pattern = "^\\d{4}-\\d{2}-\\d{2}-Flowcell Sample-Barcode list.*[^)].xlsx")

flowcell <- lapply(flowcell_files, function(x){
  x <- u01.importxlsx(x)[[1]] 
  names(x) <- x[1,] %>% make_clean_names()
  x <- x[-1, ]
  return(x)
})
names(flowcell) <- flowcell_files 
flowcell_df <- flowcell %>% rbindlist(idcol = "flowcell_file", fill = T, use.names = T) %>% 
  mutate(comment = coalesce(comments, comments_2)) %>% 
  select(-c("na", "comments", "comments_2", "flow_cell_lane")) %>%  # columns that only contain NA or have been coalesced
  mutate(comment = toupper(comment)) %>% 
  subset(grepl("Plate", sample_id))


# in flowcell but not in "manifest" 
flowcell_df %>% select(sample_id) %>% left_join(., Zebrafish_Guo_xl[, c("fish_id", "dna_collected_y_n")] %>% mutate(fish_id = gsub("-", "_", fish_id)), by = c("sample_id" = "fish_id")) 


# in flowcell but not in "manifest" 
flowcell_df %>% select(sample_id) %>% left_join(., Zebrafish_Guo_xl[, c("fish_id", "dna_collected_y_n")] %>% mutate(fish_id = gsub("-", "_", fish_id)), by = c("sample_id" = "fish_id")) 


# in "manifest" but not in flowcell


## what is the plan for missing columns
Zebrafish_Guo_xl_bev %>% 
  select(one_of(zebrafish_graph_vars)) %>% 
  gather(key, value, -date_of_beh_testing) %>% 
  subset(is.na(value)) %>% select(key) %>% unique

## qc the id's from the flowcell
Zebrafish_Guo_xl %>% select(fish_id) %>% mutate(date = str_extract(fish_id,"(\\d+)") %>% lubridate::ymd()) %>% select(date) %>% unique


## birthdate to startdate qc 
Zebrafish_Guo_xl 