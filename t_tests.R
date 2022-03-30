library(tidyverse)

full_dataset <- read_csv('no_pan_dataset.csv')

# non-threatened sig smaller than threatened
t.test(
  full_dataset$length_m[full_dataset$iucn_category == 1],
  full_dataset$length_m[full_dataset$iucn_category %in% 2:5])

# non-threatened w/ sig lower mature age
t.test(
  full_dataset$mature_age[full_dataset$iucn_category == 1],
  full_dataset$mature_age[full_dataset$iucn_category %in% 2:5])

# non-threatened w/ sig shorter lifespan
t.test(
  full_dataset$longevity[full_dataset$iucn_category == 1],
  full_dataset$longevity[full_dataset$iucn_category %in% 2:5])

# no sig difference in lat range
t.test(
  full_dataset$lat_range[full_dataset$iucn_category == 1],
  full_dataset$lat_range[full_dataset$iucn_category %in% 2:5])

# no sig difference in long range
t.test(
  full_dataset$long_range[full_dataset$iucn_category == 1],
  full_dataset$long_range[full_dataset$iucn_category %in% 2:5])

# non-threatened at sig lower trophic level
t.test(
  full_dataset$trophic_level[full_dataset$iucn_category == 1],
  full_dataset$trophic_level[full_dataset$iucn_category %in% 2:5])

# no sig difference in mass
t.test(
  full_dataset$mass_g[full_dataset$iucn_category == 1],
  full_dataset$mass_g[full_dataset$iucn_category %in% 2:5])

# non-threatened sig deeper
t.test(
  full_dataset$depth_num[full_dataset$iucn_category == 1],
  full_dataset$depth_num[full_dataset$iucn_category %in% 2:5])

