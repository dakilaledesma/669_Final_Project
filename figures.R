
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggbreak)

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

# overall distribution
for_figures %>% 
  group_by(iucn_text) %>% 
  summarize(n = n())

# lat range

latrange_anova <- aov(
  lat_range~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(latrange_anova)

ggplot(
  data = for_figures,
  mapping = aes(
    x = iucn_text,
    y = lat_range,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Latitudinal Range by IUCN Category for Marine Fish Species',
    y = 'Latitudinal Range (degrees)',
    color = 'IUCN Category',
    x = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = 'gray85')) +
  geom_text(
    aes(y = 138, label = 'a', color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# trophic level
trophiclevel_anova <- aov(
  trophic_level~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(trophiclevel_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category %in% 2:5 ~ 'b')),
  mapping = aes(
    x = iucn_text,
    y = trophic_level,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Trophic Level by IUCN Category for Marine Fish Species',
    y = 'Trophic Level',
    color = 'IUCN Category',
    x = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = 'gray85')) +
  geom_text(
    aes(y = 5.5, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# longevity
longevity_anova <- aov(
  longevity~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(longevity_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == 2 ~ 'ab',
        iucn_category == 3 ~ 'b',
        iucn_category == 4 ~ 'c',
        iucn_category == 5 ~ 'd')),
  mapping = aes(
    x = iucn_text,
    y = longevity,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Mean Longevity by IUCN Category for Marine Fish Species',
    y = 'Mean Longevity (years)',
    color = 'IUCN Category',
    x = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = 'gray85')) +
  geom_text(
    aes(y = 105, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# length
length_anova <- aov(
  length_m~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(length_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category %in% 2:3 ~ 'b',
        iucn_category == 4 ~ 'c',
        iucn_category == 5 ~ 'd')),
  mapping = aes(
    x = iucn_text,
    y = length_m,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Maximum Body Length by IUCN Category for Marine Fish Species',
    y = 'Maximum Body Length (m)',
    color = 'IUCN Category',
    x = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = 'gray85')) +
  geom_text(
    aes(y = 12, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# depth zone
ggplot(for_figures) +
  geom_bar(aes(
    x = iucn_text,
    fill = depth_zone)) +
  labs(
    title = 'Depth Zone by IUCN Category for Marine Fish Species',
    y = 'Count',
    fill = 'Depth Zone',
    x = 'IUCN Category') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(
      vjust = 0.9,
      hjust = 1,
      angle = 45),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = 'gray85'))  +
  scale_colour_viridis_d()


