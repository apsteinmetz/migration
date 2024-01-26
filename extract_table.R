library(tidyverse)
library(pdftools)
migration_raw <- pdf_data("data/immigration.pdf")

footer_start = 690
migration <- migration_raw[[14]] %>% 
  filter(str_detect(text, " .",negate = TRUE)) %>% 
  # remove footnote numbers
  filter(height != 5) %>% 
  # select(y,text) %>%
  # remove headers
  filter(y > 94) %>%
  # remove footer
  filter(y < footer_start) %>%
  # tag row labels
  mutate(is_rowlabel = ifelse(x < 100,TRUE,FALSE)) %>% 
  # tag continent labels
  mutate(is_continent = ifelse(x %in% c(39,40),TRUE,FALSE)) %>% 
  # tag countries
  mutate(is_region = ifelse(x %in% c(43,44),TRUE,FALSE)) %>% 
  # tag sub-countries
  mutate(is_subregion = ifelse(x %in% c(48,49),TRUE,FALSE)) %>% 
  identity()
  
row_labels <- migration %>% 
  filter(is_rowlabel)  %>% 
  select(y,text) %>% 
  group_by(y) %>% 
  nest(region = text)


data_cells <- migration %>%
  filter(!is_rowlabel) %>% 
  select(y,text) %>% 
  group_by(y) %>% 
  nest()
  
make_row <- function(row_data){
  row_data %>% 
    unlist() %>% 
    paste(" ",collapse = '')
}


full_rows <- full_join(row_labels,data_cells,by = "y") %>% 
  mutate(region = str_trim(make_row(region))) %>% 
  mutate(data = make_row(data)) %>% 
  # separate data into columns
  separate_wider_delim(col=data,delim = "  ",names_sep = " decade")
