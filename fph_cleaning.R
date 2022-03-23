
# setup -------------------------------------------------------------------

library(tidyverse)

parasite_hosts <- read_csv('fish_parasite_hosts.csv')


# data cleaning -----------------------------------------------------------

# removing duplicates is being weird, I'll fix it if it becomes necessary

cleaned_ph <- parasite_hosts %>% 
  # select only marine host species
  filter(marine == 1) %>%
  # add publication date, data source, and length in meters
  mutate(
    date = rep(as.Date('2013-02-01'), times = nrow(.)),
    data_src = rep('fph', times = nrow(.)),
    length_m = maximum_host_bodylength/100) %>% 
  # rename columns
  rename(
    class = host_class,
    order = host_order,
    family = host_family,
    genus_species = host_species,
    mature_age = host_age_first_maturity,
    longevity = host_life_span,
    lat_range = latitudinal_range,
    long_range = longitudinal_range,
    trophic_level = host_trophic_level) %>% 
  # take useful columns
  select(date,data_src,class,order,family,genus_species,length_m,mature_age,longevity,lat_range,long_range,trophic_level) %>% 
  # remove duplicates
  distinct() %>% 
  # remove duplicates again because the long_range column is weird
  slice(which(!1:2969 %in% which(duplicated(.$genus_species))))

write_csv(cleaned_ph, 'fph.csv')
