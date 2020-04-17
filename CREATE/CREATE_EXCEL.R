## CREATE EXCEL FROM SU GUO'S DROPBOX

# setwd("~/Dropbox (Palmer Lab)/SuGuo_R01_Zebrafish")

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Zebrafish")
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
  names(df) <- path_sheetnames
  return(df)
}

# Zebrafish_original <- u01.importxlsx("20190819- ZF EK GWAS database copy2020-02-05.xlsx")
Zebrafish_original <- u01.importxlsx("20190819- ZF EK GWAS database copy.xlsx")
Zebrafish_summary <- Zebrafish_original[[1]] 
names(Zebrafish_summary) <- Zebrafish_summary[1,] %>% make_clean_names()
Zebrafish_summary <- Zebrafish_summary[-1,]

# process Zebrafish_summary
Zebrafish_summary_df <- Zebrafish_summary %>% 
  subset(grepl("Plate|-", fish_id)) 
# %>% 
  # mutate_at(vars(one_of("date_of_birth", "date_of_shipping_to_ucsd")), as.character(as.numeric)) 

            