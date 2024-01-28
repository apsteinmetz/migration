library(tidyverse)
library(pdftools)
migration_raw <- pdf_data("data/immigration.pdf")


data_start = 95
footer_start = 690
col_label_row = 72

make_row <- function(row_data) {
  row_data %>%
    unlist() %>%
    paste(" ", collapse = '')
}


extract_page <- function(page) {
  # DEBUG
  # page <- migration_raw[[17]]
  # detect bottom of table
  footer_start <-
    page %>% filter(str_detect(text, "footnote|Prior")) %>%
    pull(y)
  col_labels <- page %>%
    filter(y == col_label_row) %>%
    pull(text) %>%
    paste0("s")
  
  migration <- page %>%
    filter(str_detect(text, " .", negate = TRUE)) %>%
    # remove footnote numbers
    filter(height != 5) %>%
    # select(y,text) %>%
    # remove headers
    filter(y >= data_start) %>%
    # remove footer
    filter(y < footer_start) %>%
    # tag row labels
    mutate(is_rowlabel = ifelse(x < 100, TRUE, FALSE)) %>%
    # tag continent labels
    mutate(is_continent = ifelse(x %in% c(39, 40), TRUE, FALSE)) %>%
    # tag countries
    mutate(is_region = ifelse(x %in% c(43, 44), TRUE, FALSE)) %>%
    # tag sub-countries
    mutate(is_subregion = ifelse(x %in% c(48, 49), TRUE, FALSE)) %>%
    identity()
  
  row_labels <- migration %>%
    filter(is_rowlabel)  %>%
    select(y, text, contains("is")) %>%
    group_by(y) %>%
    nest(region = text) %>%
    # this will create duplicates so use take just first
    mutate(region = str_trim(make_row(region))) %>%
    slice_head(n = 1)
  
  
  data_cells <- migration %>%
    filter(!is_rowlabel) %>%
    select(y, text) %>%
    rename(number = text) %>%
    group_by(y) %>%
    nest(data = number)
  
  # add decade labels
  data_cells$data <- data_cells$data %>%
    map(mutate, decade = col_labels, .before = "number")
  
  
  full_rows <- full_join(row_labels, data_cells, by = "y") %>%
    ungroup() %>%
    select(-is_rowlabel) %>%
    mutate(continent = "") %>%
    arrange(y) %>%
    # fix missing label rows
    mutate(across(starts_with("is_"), \(x) ifelse(is.na(x), FALSE, x)))
  
  current_continent <- ""
  for (i in 1:nrow(full_rows)) {
    if (full_rows$is_continent[i]) {
      current_continent <- full_rows$region[i]
    }
    full_rows$continent[i] <- current_continent
  }
  
  return(full_rows)
}

migration <- migration_raw[14:17] %>% map(extract_page) %>% 
  bind_rows() %>% 
  unnest(cols = data) %>% 
  group_by(region) %>% 
  nest(data = c(decade,number)) %>% 
  select(continent,region,data,everything())
