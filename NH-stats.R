# Mental health data

library(tidyverse)
library(readxl)
library(rvest)

website <- read_html("https://www.england.nhs.uk/publication/nhs-mental-health-dashboard/")
links <- website %>% html_nodes("a") %>% html_attr("href")
xlsx_links <- unique(links[grepl('xls', links)])

file_loc <- xlsx_links[1]
file_loc2 <- str_split(file_loc, '/')[[1]]
file_name <- file_loc2[length(file_loc2)]
new_file <- glue::glue('spreadsheets/{file_name}')
xlsx_file <- download.file(file_loc, destfile = newfile)
sheets <- excel_sheets(new_file)

pmh_df <- read_xlsx(new_file, sheet = "PMH(i_a)", col_types="text", skip = 9)
mhf_df <- read_xlsx(new_file, sheet = "MHF(iii)_FINAL CALC", col_types="text", skip = 9)

all_sheets <- sapply(sheets[10:length(sheets)], function(x) {
  read_xlsx(new_file, sheet = x, col_types="text", skip = 9)
})

type1 <- colnames(all_sheets$`SP(ii_b)_FINAL CALC`)
type1a <- colnames(all_sheets$`CR(iii_c)_FINAL CALC`) # remove col ...34
type2 <- colnames(all_sheets$`MHF(i)_FINAL CALC`)

parse_frames <- function(df, list_name, type) {
  if(all(colnames(df) == type)) {
    ln <- str_replace(list_name, '_FINAL CALC', '')
    if(all(type == type1a)) {
      df %>% select(-`...34`) %>% mutate(category = ln)
    } else {
      df %>% mutate(category = ln)
    }
  }
}


type1_df <- map2_df(all_sheets, names(all_sheets), ~parse_frames(.x, .y, type1))

type1a_df <- map2_df(all_sheets, names(all_sheets), ~parse_frames(.x, .y, type1a))

type_final_df <- bind_rows(type1_df, type1a_df)

type2_df <- map2_df(all_sheets, names(all_sheets), ~parse_frames(.x, .y, type2))

write_csv(type_final_df, 'mh-dashboard-datatype1.csv')
write_csv(type2_df, 'mh-dashboard-datatype2.csv')
