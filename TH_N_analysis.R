library(tidyverse)
library(dplyr)
library(janitor)
library(ggsignif)

th_n <- readxl::read_excel("/Users/summerharper/NTRES 6100/gitHub/Oak_leaf_data/TH_N_data.xlsx")
View(th_n)

## Cleaning Data and column names
colnames(th_n)
th_n_clean <- clean_names(th_n) %>%
  rename(leaf_category = leaf_category_o_original_l_lammas) %>%
  select(sample_label,percent_n, leaf_category, tree_age)



### T-test
t.test(formula = percent_n ~ leaf_category, data = th_n_clean) # p = 0.0004217

#difference between lammas and original in each age group
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "mature") #p = 0.5494
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "intermediate") # p = 1.025e-05
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "seedling") # p = 0.04883


## Data Visualization
# Differences in leaf types
th_n_clean %>%
  ggplot(mapping = aes(x = leaf_category, y = percent_n)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c("L", "O")), test = "t.test",
                        map_signif_level = TRUE, annotations = "p = 0.000427")


## differences in leaf type across age groups
th_n_clean %>%
  ggplot(mapping = aes(x = leaf_category, y = percent_n)) +
  geom_boxplot() +
  facet_wrap(~tree_age) +
  ggtitle("N% in Different Leaf Types and Age Groups") +
    ggsignif::geom_signif(comparisons = list(c("L", "O")), test = "t.test")

