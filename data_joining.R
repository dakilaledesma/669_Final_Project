
# setup -------------------------------------------------------------------

library(tidyverse)

iucn_full <- read_csv('iucn_data/iucn_full.csv')

fph <- read_csv('fph.csv')

pan <- read_csv('pan.csv') %>% 
  rename(
    mature_age = age.maturity,
    depth_zone = habitat,
    diet = feeding.mode) %>% 
  select(!...12)

fmd <- read_csv('fmd.csv') %>% 
  mutate(
    genus_species = str_replace(genus_species, '_', ' '),
    diet = str_to_lower(diet_content)) %>% 
  select(
    !c(length_cm,
       mass_g,
       mature_age,
       longevity,
       depth_zone,
       lat_range,
       long_range,
       diet_specificity,
       commercial,
       iucn,
       diet_content,
       Minimum_depth_m,
       Maximum_depth_m)) %>% 
  rename(trophic_level = trophic_leve)

fbs <- read_csv('fbs.csv') %>% 
  mutate(
    genus_species = str_c(genus, species, sep = ' '),
    length_m = length_m/100) %>% 
  select(date,genus_species,length_m,mass_g,depth_num,depth_zone)


# data joining ------------------------------------------------------------

traits <- fph %>% 
  distinct() %>% 
  rename(
    length_m_fph = length_m,
    trophic_level_fph = trophic_level) %>% 
  select(!date:family) %>% 
  full_join(
    fmd %>% 
      distinct() %>% 
      rename(
        length_m_fmd = length_m,
        trophic_level_fmd = trophic_level) %>% 
      select(!date:genus),
    by = 'genus_species') %>%
  mutate(
    length_m = if_else(
      is.na(length_m_fmd),
      true = length_m_fph,
      false = length_m_fmd),
    trophic_level = if_else(
      is.na(trophic_level_fmd),
      true = trophic_level_fph,
      false = trophic_level_fmd),
    year = if_else(
      length_m == length_m_fmd | trophic_level == trophic_level_fmd,
      true = 2015,
      false = 2013,
      missing = 2013)) %>% 
  select(!c(length_m_fph,length_m_fmd,trophic_level_fph,trophic_level_fmd)) %>% 
  rename(
    length_m_old = length_m,
    mature_age_old = mature_age,
    longevity_old = longevity,
    diet_old = diet) %>% 
  full_join(
    pan %>% 
      distinct() %>% 
      rename(
        length_m_pan = length_m,
        mature_age_pan = mature_age,
        longevity_pan = longevity,
        diet_pan = diet) %>% 
      select(!date:species),
    by = 'genus_species') %>%
  mutate(
    length_m = if_else(
      !is.na(length_m_pan),
      true = length_m_pan,
      false = length_m_old),
    mature_age = if_else(
      !is.na(mature_age_pan),
      true = mature_age_pan,
      false = mature_age_old),
    longevity = if_else(
      !is.na(longevity_pan),
      true = longevity_pan,
      false = longevity_old),
    diet = if_else(
      !is.na(diet_pan),
      true = diet_pan,
      false = diet_old),
    year = if_else(
      length_m == length_m_pan | mature_age == mature_age_pan | longevity == longevity_pan | diet == diet_pan,
      true = 2019,
      false = year,
      missing = year)) %>% 
  select(!c(ends_with('old'), ends_with('pan'))) 
rename(
  length_m_old = length_m,
  depth_num_old = depth_num,
  depth_zone_old = depth_zone) %>%
  full_join(
    fbs %>% 
      distinct() %>% 
      rename(
        length_m_fbs = length_m,
        depth_num_fbs = depth_num,
        depth_zone_fbs = depth_zone),
    by = 'genus_species') %>% 
  mutate(
    length_m = if_else(
      (date > year | is.na(year)) & !is.na(length_m_fbs),
      true = length_m_fbs,
      false = length_m_old),
    depth_num = if_else(
      (date > year | is.na(year)) & !is.na(depth_num_fbs),
      true = depth_num_fbs,
      false = depth_num_old),
    depth_zone = if_else(
      (date > year | is.na(year)) & !is.na(depth_zone_fbs),
      true = depth_zone_fbs,
      false = depth_zone_old)) %>% 
  select(!c(ends_with('old'), ends_with('fbs'), year, date))

with_traits <- iucn_full %>% 
  right_join(
    traits,
    by = 'genus_species') %>% 
  mutate(
    iucn_category = if_else(
      is.na(iucn_category),
      true = 'ne',
      false = iucn_category))

no_pan <- fph %>% 
  distinct() %>% 
  rename(
    length_m_fph = length_m,
    trophic_level_fph = trophic_level) %>% 
  select(!date:family) %>% 
  full_join(
    fmd %>% 
      distinct() %>% 
      rename(
        length_m_fmd = length_m,
        trophic_level_fmd = trophic_level) %>% 
      select(!date:genus),
    by = 'genus_species') %>%
  mutate(
    length_m = if_else(
      is.na(length_m_fmd),
      true = length_m_fph,
      false = length_m_fmd),
    trophic_level = if_else(
      is.na(trophic_level_fmd),
      true = trophic_level_fph,
      false = trophic_level_fmd),
    year = if_else(
      length_m == length_m_fmd | trophic_level == trophic_level_fmd,
      true = 2015,
      false = 2013,
      missing = 2013)) %>% 
  select(!c(length_m_fph,length_m_fmd,trophic_level_fph,trophic_level_fmd)) %>%
  rename(
    length_m_old = length_m,
    depth_num_old = depth_num) %>%
  full_join(
    fbs %>% 
      distinct() %>% 
      rename(
        length_m_fbs = length_m,
        depth_num_fbs = depth_num),
    by = 'genus_species') %>% 
  mutate(
    length_m = if_else(
      (date > year | is.na(year)) & !is.na(length_m_fbs),
      true = length_m_fbs,
      false = length_m_old),
    depth_num = if_else(
      (date > year | is.na(year)) & !is.na(depth_num_fbs),
      true = depth_num_fbs,
      false = depth_num_old)) %>% 
  select(!c(ends_with('old'), ends_with('fbs'), year, date))

no_pan_full <- iucn_full %>% 
  right_join(
    no_pan,
    by = 'genus_species') %>% 
  mutate(
    iucn_category = if_else(
      is.na(iucn_category),
      true = 'ne',
      false = iucn_category))

write.csv(with_traits, 'full_dataset.csv', row.names = F)

write.csv(no_pan_full, 'no_pan_dataset.csv', row.names = F)

with_traits %>% 
  group_by(iucn_category) %>% 
  summarize(n = n())

for_model <- with_traits %>% 
  filter(iucn_category %in% as.character(1:5)) %>% 
  mutate(iucn_category = factor(iucn_category, levels = 1:5))

ggplot(for_model) +
  geom_bar(aes(x = iucn_category))
