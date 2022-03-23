
# setup -------------------------------------------------------------------

library(tidyverse)

cr <- read_csv('redlist/redlist_cr/taxonomy.csv')

ex <- read_csv('redlist/redlist_ex/taxonomy.csv')

en <- read_csv('redlist/redlist_en/taxonomy.csv')

vu <- read_csv('redlist/redlist_vu/taxonomy.csv')

nt <- read_csv('redlist/redlist_nt/taxonomy.csv')

lc <- read_csv('redlist/redlist_lc/taxonomy.csv')

dd <- read_csv('redlist/redlist_dd/taxonomy.csv')

# data cleaning -----------------------------------------------------------

iucn_groups <- map2(
  .x = list(dd,lc,nt,vu,en,cr,ex),
  .y = c('dd',1,2,3,4,5,'ex'),
  ~ .x %>% 
    select(
      genus_species = scientificName,
      iucn_authority = authority) %>% 
    mutate(iucn_category = rep(.y, times = nrow(.))))

iucn_full <- bind_rows(iucn_groups)

write.csv(iucn_full, 'iucn_data/iucn_full.csv', row.names = F)
