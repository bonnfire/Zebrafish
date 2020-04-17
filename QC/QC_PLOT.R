## QC and PLOT
Zebrafish_summary_df %>% get_dupes(fish_id)
  

Zebrafish_formats_cellbycell <- tidyxl::xlsx_cells("20190819- ZF EK GWAS database copy2020-02-05.xlsx") %>% 
  dplyr::filter(sheet == "Summary")
Zebrafish_formats <- tidyxl::xlsx_formats("20190819- ZF EK GWAS database copy2020-02-05.xlsx")

redhexa <- Zebrafish_formats$local$fill$patternFill$bgColor$rgb[Zebrafish_formats_cellbycell[which(grepl("Red", Zebrafish_formats_cellbycell$character)),]$local_format_id]
# nonblackhexa_indices <- which(Zebrafish_formats$local$font$color$rgb == redhexa)
dead_zebrafish_indices <- Zebrafish_formats_cellbycell[Zebrafish_formats_cellbycell$local_format_id %in% which(Zebrafish_formats$local$fill$patternFill$bgColor$rgb == redhexa), ] %>% 
  # dplyr::filter(data_type != "blank") %>% 
  select(row, col) 
nonblackrows_Jhou_Excel_ProgPun %>% dplyr::filter(!is.na(numeric))

Zebrafish_summary_df %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)

Zebrafish_summary_df %>% 
  ggplot() + 
  geom_histogram() + 
  facet_grid(~ date_of_beh_testing)

Zebrafish_summary_df %>% get_dupes(fish_id) %>% as.data.frame() %>% openxlsx::write.xlsx(., "Duplicated_FishIDs.xlsx")