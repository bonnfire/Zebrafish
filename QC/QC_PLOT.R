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
Zebrafish_summary_df_bev <- Zebrafish_summary_df %>% 
  mutate_at(vars(-one_of("z_number", "fish_id", "dna_collected_y_n", "unique_plate_id", "well_id", "date_of_birth", "date_of_beh_testing", "cross_no", "mother", "father", "gender", "date_of_shipping_to_ucsd")), as.numeric) %>% 
  subset(!is.na(ld_choice_index)) 


library(purrr)
library(tidyr)
library(ggplot2)


Zebrafish_summary_df_bev %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


Zebrafish_summary_df_bev %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)

Zebrafish_summary_df %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)


setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/QC")
Zebrafish_summary_df %>% get_dupes(fish_id) %>% as.data.frame() %>% openxlsx::write.xlsx(., "Duplicated_FishIDs.xlsx")
