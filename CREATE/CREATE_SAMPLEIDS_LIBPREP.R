## extract the sample_ids from khai's library prep
library(readxl)
library(tidyverse)
library(janitor)
library(data.table)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Khai-Minh Nguyen/Riptide library prep")
library_prep_files <- list.files(recursive = T, full.names = T)
zebrafish_libprep_files <- grep(".*Riptide.*/Riptide[- ]?(0[7]|1[078]|2[1-9]|30).*.xlsx", library_prep_files, value = T)
zebrafish_libprep_files <- zebrafish_libprep_files[which(zebrafish_libprep_files != "./Riptide 11-20/Riptide-18 (20191209 Plate2)/Riptide 18 sample.xlsx")] # exclude this sample file 

zebrafish_sample_info <- lapply(zebrafish_libprep_files, function(x){
  
  u01.importxlsx <- function(xlname){
    path_sheetnames <- excel_sheets(xlname)
    df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
    names(df) <- path_sheetnames
    return(df)
  }
  
  x <- u01.importxlsx(x)[["Sample_Information"]] 
  firstrow_names <- x[3, ] %>% unlist() %>% as.character()
  names(x) <- c(firstrow_names)

  # extract plate information
  if(is.na(x[1,4]) == T){
    x$plate <- rep(x[1,3] %>% as.character(), nrow(x))
  }
  else{
    x$plate <- rep(x[1,4] %>% as.character(), nrow(x))
  }
  x <- x[-c(1:3),] %>%
    clean_names
  return(x)
}) 

zebrafish_sample_info_df <- zebrafish_sample_info %>% rbindlist(fill = T) %>%
  subset(!is.na(sample_id)&!is.na(plate)) %>% 
  mutate(rfid = sample_id) # remove the template

## XX remove the extra 20191217 Plate1 from Riptide07 once Khai confirms
zebrafish_sample_info_df %>% get_dupes(rfid) %>% select(plate) %>% table()

if(zebrafish_sample_info_df %>% get_dupes(rfid) %>% nrow != 0){
  print("In zebrafish_sample_info_df, Duplicate combinations found of: rfid")
}

## extract the riptide