
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

ggplot(full_dataset[full_dataset$iucn_category %in% 1:5,]) +
  geom_histogram(aes(x = lat_range)) +
  facet_wrap(facets = vars(iucn_category))

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
  scale_colour_viridis_d()

# trophic level

ggplot(full_dataset[full_dataset$iucn_category %in% 1:5,]) +
  geom_histogram(aes(x = trophic_level)) +
  facet_wrap(facets = vars(iucn_category))

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

ggplot(
  full_dataset %>% 
    filter(
      iucn_category %in% 1:5,
      longevity > 0)) +
  geom_histogram(aes(x = log(longevity))) +
  facet_wrap(facets = vars(iucn_category))

longevity_anova <- aov(
  log(longevity)~iucn_category, 
  data = full_dataset %>% 
    filter(
      iucn_category %in% 1:5,
      longevity > 0))

TukeyHSD(longevity_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category %in% 2:3 ~ 'b',
        iucn_category == 4 ~ 'bc',
        iucn_category == 5 ~ 'c')) %>% 
    filter(longevity > 0),
  mapping = aes(
    x = iucn_text,
    y = log(longevity),
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Mean Longevity by IUCN Category for Marine Fish Species',
    y = 'Mean Log-Transformed Longevity in Years',
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
    aes(y = 5.2, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# length

ggplot(full_dataset[full_dataset$iucn_category %in% 1:5,]) +
  geom_histogram(aes(x = log(length_m))) +
  facet_wrap(facets = vars(iucn_category))

length_anova <- aov(
  log(length_m)~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(length_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == c(2,4) ~ 'b',
        iucn_category == 3 ~ 'c',
        iucn_category == 5 ~ 'd')),
  mapping = aes(
    x = iucn_text,
    y = log(length_m),
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Maximum Body Length by IUCN Category for Marine Fish Species',
    y = 'Log-Transformed Maximum Body Length in Meters',
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
  scale_fill_viridis_d()

# mature age

ggplot(
  data = for_figures,
  mapping = aes(
    x = iucn_text,
    y = mature_age,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Mature Age by IUCN Category for Marine Fish Species',
    y = 'Mature Age (years)',
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
  scale_colour_viridis_d()

# longitudinal range

ggplot(
  data = for_figures,
  mapping = aes(
    x = iucn_text,
    y = long_range,
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Longitudinal Range by IUCN Category for Marine Fish Species',
    y = 'Longitudinal Range (degrees)',
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
  scale_colour_viridis_d()

# mass

ggplot(full_dataset[full_dataset$iucn_category %in% 1:5,]) +
  geom_histogram(aes(x = log(mass_g))) +
  facet_wrap(facets = vars(iucn_category))

mass_anova <- aov(
  log(mass_g)~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5 & full_dataset$mass_g < 10000000,])

TukeyHSD(mass_anova)

ggplot(
  data = for_figures %>% 
    filter(mass_g < 10000000) %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == 2 ~ 'b',
        iucn_category %in% 3:4 ~ 'bc',
        iucn_category == 5 ~ 'c')),
  mapping = aes(
    x = iucn_text,
    y = log(mass_g),
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Maximum Body Mass by IUCN Category for Marine Fish Species',
    y = 'Maximum Body Mass (g)',
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
    aes(y = 15.5, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# diet

ggplot(for_figures %>% 
         filter(!is.na(diet))) +
  geom_bar(aes(
    x = iucn_text,
    fill = diet)) +
  labs(
    title = 'Diet Type by IUCN Category for Marine Fish Species',
    y = 'Count',
    fill = 'Diet Type',
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
  scale_fill_viridis_d()

# depth as a number

ggplot(full_dataset[full_dataset$iucn_category %in% 1:5,]) +
  geom_histogram(aes(x = log(depth_num))) +
  facet_wrap(facets = vars(iucn_category))

depthnum_anova <- aov(
  log(depth_num)~iucn_category, 
  data = full_dataset[full_dataset$iucn_category %in% 1:5,])

TukeyHSD(depthnum_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category %in% 1:2 ~ 'a',
        iucn_category %in% 4:5 ~ 'ab',
        iucn_category == 3 ~ 'b')),
  mapping = aes(
    x = iucn_text,
    y = log(depth_num),
    color = iucn_text)) +
  geom_boxplot() +
  labs(
    title = 'Depth by IUCN Category for Marine Fish Species',
    y = 'Log-Transformed Depth in Meters',
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
    aes(y = 9.1, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()
