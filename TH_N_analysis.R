library(tidyverse)
library(janitor)

th_n <- readxl::read_excel("/Users/summerharper/NTRES 6100/gitHub/Oak_leaf_data/TH_N_data.xlsx")
View(th_n)

colnames(th_n)
th_n_clean <- clean_names(th_n) %>%
  rename(leaf_category = leaf_category_o_original_l_lammas) %>%
  select(sample_label, mass_mg, percent_n, leaf_category, tree_age)
View(th_n_clean)



