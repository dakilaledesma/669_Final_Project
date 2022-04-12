
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggbreak)

full_dataset <- read_csv('no_pan_dataset.csv')

for_figures <- full_dataset %>%
  distinct() %>% 
  mutate(iucn_text = case_when(
    iucn_category == 1 ~ 'Least Concern',
    iucn_category == 2 ~ 'Near Threatened',
    iucn_category == 3 ~ 'Vulnerable',
    iucn_category == 4 ~ 'Endangered',
    iucn_category == 5 ~ 'Critically Endangered',
    iucn_category == 'dd' ~ 'Data Deficient',
    iucn_category == 'ne' ~ 'Not Evaluated') %>% 
      factor(
        levels = c('Least Concern', 'Near Threatened', 'Vulnerable', 'Endangered', 'Critically Endangered', 'Data Deficient', 'Not Evaluated')))


# data plots --------------------------------------------------------------

# overall distribution
for_figures %>% 
  group_by(iucn_text) %>% 
  summarize(n = n())

# lat range

# distributions are similar, but not symmetrical

full_dataset %>% 
ggplot() +
  geom_histogram(aes(x = lat_range)) +
  facet_wrap(facets = vars(iucn_category))

kruskal.test(lat_range ~ iucn_category, data = for_figures)

pairwise.wilcox.test(for_figures$lat_range, for_figures$iucn_category, p.adjust.method = 'BH')

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category %in% 1:3 ~ 'a',
        iucn_category %in% 4:5 ~ 'ab',
        iucn_category %in% c('dd','ne') ~ 'b')),
  mapping = aes(
    x = iucn_text,
    y = lat_range,
    color = iucn_text)) +
  geom_boxplot() +
  geom_point(
    aes(y = median(lat_range, na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
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
    aes(y = 155, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# trophic level

# distributions are not symmetrical, but are relatively similar

full_dataset %>% 
ggplot() +
  geom_histogram(aes(x = trophic_level)) +
  facet_wrap(facets = vars(iucn_category))

kruskal.test(trophic_level ~ iucn_category, data = for_figures)

pairwise.wilcox.test(for_figures$trophic_level, for_figures$iucn_category, p.adjust.method = 'BH')

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category %in% c(1,'ne') ~ 'a',
        iucn_category %in% 2:5 ~ 'b',
        iucn_category == 'dd' ~ 'c')),
  mapping = aes(
    x = iucn_text,
    y = trophic_level,
    color = iucn_text)) +
  geom_boxplot() +
  geom_point(
    aes(y = median(trophic_level, na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
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

# distributions can be normalized by log-transformation

ggplot(
  full_dataset %>% 
    filter(
      longevity > 0)) +
  geom_histogram(aes(x = log(longevity))) +
  facet_wrap(facets = vars(iucn_category))

longevity_anova <- aov(
  log(longevity)~iucn_category, 
  data = full_dataset %>% 
    filter(
      longevity > 0))

TukeyHSD(longevity_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == 'dd' ~ 'ab',
        iucn_category == 2 ~ 'bc',
        iucn_category == 'ne' ~ 'b',
        iucn_category == 3 ~ 'cd',
        iucn_category == 4 ~ 'de',
        iucn_category == 5 ~ 'e')) %>% 
    filter(longevity > 0),
  mapping = aes(
    x = iucn_text,
    y = log(longevity),
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(log(longevity), na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
    y = 'Log-Transformed Mean Longevity in Years',
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

# can be normalized by log-transformation

ggplot(full_dataset) +
  geom_histogram(aes(x = log(length_m))) +
  facet_wrap(facets = vars(iucn_category))

length_anova <- aov(
  log(length_m)~iucn_category, 
  data = full_dataset)

TukeyHSD(length_anova)

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == c(2,4) ~ 'b',
        iucn_category == 3 ~ 'c',
        iucn_category == 5 ~ 'd',
        iucn_category == 'dd' ~ 'e',
        iucn_category == 'ne' ~ 'f')),
  mapping = aes(
    x = iucn_text,
    y = log(length_m),
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(log(length_m), na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
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

for_figures %>% 
  filter(!is.na(depth_zone)) %>% 
  group_by(iucn_text,depth_zone) %>% 
  summarize(in_group = n()) %>% 
  left_join(
    for_figures %>% 
      filter(!is.na(depth_zone)) %>% 
      group_by(iucn_text) %>% 
      summarize(total = n()),
    by = 'iucn_text') %>% 
  mutate(percent = in_group / total * 100) %>% 
ggplot() +
  geom_col(aes(
    x = iucn_text,
    y = percent,
    fill = depth_zone)) +
  labs(
    y = 'Percent',
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

# similarly distributed but not symmetrical

full_dataset  %>% 
  ggplot() +
  geom_histogram(aes(x = mature_age)) +
  facet_wrap(facets = vars(iucn_category))

kruskal.test(mature_age ~ iucn_category, data = for_figures)

pairwise.wilcox.test(for_figures$mature_age, for_figures$iucn_category, p.adjust.method = 'BH')

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category == 'dd' ~ 'abc',
        iucn_category %in% c(2,'ne') ~ 'b',
        iucn_category == 3 ~ 'c',
        iucn_category == 4 ~ 'd',
        iucn_category == 5 ~ 'e')),
  mapping = aes(
    x = iucn_text,
    y = mature_age,
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(mature_age, na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
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
  geom_text(
    aes(y = 35, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()

# longitudinal range

# similarly distributed but not symmetrical

full_dataset  %>%  
  ggplot() +
  geom_histogram(aes(x = long_range)) +
  facet_wrap(facets = vars(iucn_category))

kruskal.test(long_range ~ iucn_category, data = for_figures)

pairwise.wilcox.test(for_figures$long_range, for_figures$iucn_category, p.adjust.method = 'BH')

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category %in% c(1,5) ~ 'a',
        iucn_category %in% 2:3 ~ 'ac',
        iucn_category %in% 4:5 ~ 'abc',
        iucn_category == 'dd' ~ 'b',
        iucn_category == 'ne' ~ 'c')),
  mapping = aes(
    x = iucn_text,
    y = long_range,
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(long_range, na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
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

# can be normalized by log-transformation

ggplot(full_dataset) +
  geom_histogram(aes(x = log(mass_g))) +
  facet_wrap(facets = vars(iucn_category))

mass_anova <- aov(
  log(mass_g)~iucn_category, 
  data = full_dataset[full_dataset$mass_g < 10000000,])

TukeyHSD(mass_anova)

ggplot(
  data = for_figures %>% 
    filter(mass_g < 10000000) %>% 
    mutate(
      sig = case_when(
        iucn_category %in% c(1,'ne') ~ 'a',
        iucn_category %in% c(2,'dd') ~ 'b',
        iucn_category == 3 ~ 'c',
        iucn_category == 4 ~ 'cd',
        iucn_category == 5 ~ 'd')),
  mapping = aes(
    x = iucn_text,
    y = log(mass_g),
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(log(mass_g), na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
    y = 'Log-Transformed Maximum Body Mass (g)',
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

for_figures %>% 
  filter(!is.na(diet)) %>% 
  group_by(iucn_text,diet) %>% 
  summarize(in_group = n()) %>% 
  left_join(
    for_figures %>% 
      filter(!is.na(diet)) %>% 
      group_by(iucn_text) %>% 
      summarize(total = n()),
    by = 'iucn_text') %>% 
  mutate(percent = in_group / total * 100) %>% 
ggplot() +
  geom_col(aes(
    x = iucn_text,
    y = percent,
    fill = diet)) +
  labs(
    y = 'Percent',
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

# distributions are similar but not symmetrical

ggplot(full_dataset) +
  geom_histogram(aes(x = depth_num)) +
  facet_wrap(facets = vars(iucn_category))

kruskal.test(depth_num ~ iucn_category, data = for_figures)

pairwise.wilcox.test(for_figures$depth_num, for_figures$iucn_category, p.adjust.method = 'BH')

ggplot(
  data = for_figures %>% 
    mutate(
      sig = case_when(
        iucn_category == 1 ~ 'a',
        iucn_category %in% c(2,5) ~ 'acd',
        iucn_category == 4 ~ 'ad',
        iucn_category == 3 ~ 'b',
        iucn_category == 'dd' ~ 'c',
        iucn_category == 'ne' ~ 'd')),
  mapping = aes(
    x = iucn_text,
    y = depth_num,
    color = iucn_text)) +
  geom_boxplot() + 
  geom_point(
    aes(y = median(depth_num, na.rm = T)),
    shape = 1,
    size = 3) +
  labs(
    y = 'Depth in Meters',
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
    aes(y = 8000, label = sig, color = NULL),
    show.legend = F) +
  scale_colour_viridis_d()
