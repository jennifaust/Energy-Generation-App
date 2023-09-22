### global script for energy generation app

library(tidyverse)
library(lubridate)
library(corrr)
# library(ggridges)
library(factoextra)

### load energy_long
energy_long <- readr::read_csv("./Prepped_Data/energy_long.csv", col_names = TRUE)
                               
### Load energy_wide_gen_type
energy_wide_gen_type <- readr::read_csv("./Prepped_Data/energy_wide_gen_type.csv", col_names = TRUE)

### Load energy_wide_region
energy_wide_region <- readr::read_csv("./Prepped_Data/energy_wide_region.csv", col_names = TRUE)

### create a variable that stores all region names
region_names <- energy_wide_region %>% 
  purrr::keep(is.numeric) %>% 
  names()

### create a variable that stores all energy generation type names
energy_gen_names <- energy_wide_gen_type %>% 
  purrr::keep(is.numeric) %>% 
  names()

### load energy_week_ready
energy_week_ready <- readr::read_csv("./Prepped_Data/energy_week_ready.csv", col_names = TRUE)

### load wide_format
wide_format <- readr::read_csv("./Prepped_Data/wide_format.csv", col_names = TRUE)

### load energy_long_v (proportions data set)
energy_long_v <- readr::read_csv("./Prepped_Data/energy_long_v.csv", col_names = TRUE)

### load energy_pcp_lf_ready (used in time series)
# energy_pcp_lf_ready <- readr::read_csv("/Users/jennifaust/Documents/CMPINF2130/Dashboard\ Project/Prepped_Data/energy_pcp_lf_ready.csv", col_names = TRUE)

### create pcp model
energy_pca <- prcomp(wide_format %>% select(-group_name) %>% as.data.frame(),
                     scale. = TRUE)

### creating a separate variable to not interfere with pca scatter
var_contrib_pca <- prcomp(wide_format %>% select(-group_name) %>% as.data.frame(),
                     scale. = TRUE)

### creating energy_hclust for cluster plot
energy_pc_scores <- energy_pca$x %>% as.data.frame() %>% select(1:16)
energy_hclust <- hclust(d = dist(energy_pc_scores), method = "ward.D2")

### load wide_format_sep
wide_format_sep <- readr::read_csv("./Prepped_Data/wide_format_sep.csv", col_names = TRUE)

group_name <- wide_format_sep %>% 
  distinct(group_name) %>% 
  pull()

### laod energy_pcp_week_lf_ready
energy_pcp_week_lf_ready <- readr::read_csv("./Prepped_Data/energy_pcp_week_lf_ready.csv", col_names = TRUE)

### define a function which creates a simple data dictionary
basic_info_table_2 <- function(my_df){
  tibble::tibble(
    `variable_names` = names(my_df),
    `datatype` = purrr::map_chr(my_df, function(x){paste(class(x), collapse = '')}),
    `number unique values` = purrr::map_dbl(my_df, n_distinct),
    `number missing values` = purrr::map_dbl(my_df, ~sum(is.na(.)))
  ) %>% 
    mutate(`proportion unique` = `number unique values`/nrow(my_df),
           `proportion missing` = `number missing values`/nrow(my_df))
}

### apply the function to create the data dictionary for mpg
energy_dictionary <- energy_long %>% basic_info_table_2()