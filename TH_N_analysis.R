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

View(th_n_clean)


### Welch Two Sample T-test
t.test(formula = percent_n ~ leaf_category, data = th_n_clean) # p = 0.0004217
#difference between lammas leaves and original in each age group
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "mature") #p = 0.5494
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "intermediate") # p = 1.025e-05
t.test(formula = percent_n ~ leaf_category, data = th_n_clean, subset = tree_age == "seedling") # p = 0.04883


## Data Visualization
# Differences in leaf types
L_vs_O <- th_n_clean %>%
  ggplot(mapping = aes(x = leaf_category, y = percent_n)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c("L", "O")), test = "t.test",
                        map_signif_level = TRUE, annotations = "p = 0.000427")

ggsave(path = "./plots", filename = "L_vs_O.png")

## differences in leaf type across age groups
box_age<- th_n_clean %>%
  ggplot(mapping = aes(x = leaf_category, y = percent_n)) +
  geom_boxplot() +
  facet_wrap(~tree_age) +
  ggtitle("N% in Different Leaf Types and Age Groups") +
    ggsignif::geom_signif(comparisons = list(c("L", "O")), test = "t.test")

ggsave(path = "./plots", filename = "box_age.png")

means_by_group <-th_n_clean %>%
  group_by(tree_age, leaf_category) %>%
  summarize(mean_N = mean(percent_n))

means_by_group_plot <- means_by_group %>%
  group_by(leaf_category) %>%
  ggplot(mapping = aes(x = tree_age, y = mean_N, fill = leaf_category)) +
  geom_col(position = position_dodge())

ggsave(path = "./plots", filename = "means_by_group_plot.png")


