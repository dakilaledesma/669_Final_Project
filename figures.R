
# setup -------------------------------------------------------------------

library(tidyverse)

full_dataset <- read_csv('no_pan_dataset.csv')

for_figures <- full_dataset %>% 
  filter(iucn_category %in% as.character(1:5)) %>% 
  mutate(iucn_category = factor(iucn_category, levels = 1:5)) %>% 
  distinct() %>% 
  mutate(iucn_text = case_when(
    iucn_category == 1 ~ 'Least Concern',
    iucn_category == 2 ~ 'Near Threatened',
    iucn_category == 3 ~ 'Vulnerable',
    iucn_category == 4 ~ 'Endangered',
    iucn_category == 5 ~ 'Critically Endangered') %>% 
      factor(
        levels = c('Least Concern', 'Near Threatened', 'Vulnerable', 'Endangered', 'Critically Endangered')))


# data plots --------------------------------------------------------------

# lat range
ggplot(for_figures) +
  geom_boxplot(
    mapping = aes(
      y = lat_range,
      color = iucn_text)) +
  labs(
    title = 'Latitudinal Range by IUCN Category for Marine Fish Species',
    y = 'Latitudinal Range (degrees)',
    color = 'IUCN Category') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# trophic level
ggplot(for_figures) +
  geom_boxplot(
    mapping = aes(
      y = trophic_level,
      color = iucn_text)) +
  labs(
    title = 'Trophic Level by IUCN Category for Marine Fish Species',
    y = 'Trophic Level',
    color = 'IUCN Category') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# longevity
ggplot(for_figures) +
  geom_boxplot(
    mapping = aes(
      y = longevity,
      color = iucn_text)) +
  labs(
    title = 'Mean Longevity by IUCN Category for Marine Fish Species',
    y = 'Mean Longevity (years)',
    color = 'IUCN Category') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# length
ggplot(for_figures) +
  geom_boxplot(
    mapping = aes(
      y = length_m,
      color = iucn_text)) +
  labs(
    title = 'Maximum Body Length by IUCN Category for Marine Fish Species',
    y = 'Maximum Body Length (m)',
    color = 'IUCN Category') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# depth zone
ggplot(for_figures) +
  geom_jitter(
    mapping = aes(
      x = depth_zone,
      y = iucn_category),
    height = 0.1,
    width = 0.2)


