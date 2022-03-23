
# setup -------------------------------------------------------------------

library(tidyverse)

animals <- read_csv('IUCN_animal_species.csv')


# assessment --------------------------------------------------------------

clean_animals <- animals %>% 
  rename(
    Order = Name,
    # extinct (EX) or extinct in the wild (EW)
    certain_extinct = 'Subtotal (EX+EW)',
    # extinct, extinct in the wild, or possible either (CR(PE) or CR(PEW))
    possible_extinct = 'Subtotal (EX+EW+ CR(PE)+CR(PEW))',
    # critically endangered (CR), endangered (EN), or vulnerable (VU)
    threatened = 'Subtotal (threatened spp.)',
    # lower risk, conservation dependent
    conservation_dependent = 'LR/cd',
    # near threatened (NT) or lower concern/near threatened (LR/nt)
    near_threatened = 'NT or LR/nt',
    least_concern = 'LC or LR/lc',
    data_deficient = DD) %>%
  select(
    Order, certain_extinct, possible_extinct, threatened, conservation_dependent, near_threatened, least_concern, data_deficient, Total) %>% 
  rowwise() %>% 
  mutate(Class = case_when(
    Order %in% .$Order[1:46] ~ 'Actinopterygii',
    Order %in% .$Order[47:49] ~ 'Amphibia',
    Order %in% .$Order[50:56] ~ 'Anthozoa',
    Order %in% .$Order[57:64] ~ 'Arachnida',
    Order == .$Order[65] ~ 'Asteroidea',
    Order %in% .$Order[66:101] ~ 'Aves',
    Order %in% .$Order[102:111] ~ 'Bivalvia',
    Order %in% .$Order[112:114] ~ 'Branchiopoda',
    Order == .$Order[115] ~ 'Cephalaspidomorphi',
    Order %in% .$Order[116:123] ~ 'Cephalopoda',
    Order %in% .$Order[124:127] ~ 'Chilopoda',
    Order %in% .$Order[128:140] ~ 'Chondrichthyes',
    Order %in% .$Order[141:144] ~ 'Clitellata',
    Order %in% .$Order[145:146] ~ 'Collembola',
    Order %in% .$Order[147:153] ~ 'Diplopoda',
    Order == .$Order[154] ~ 'Echinoidea',
    Order == .$Order[155] ~ 'Enopla',
    Order %in% .$Order[156:174] ~ 'Gastropoda',
    Order %in% .$Order[175:177] ~ 'Hexanauplia',
    Order == .$Order[178] ~ 'Holothuroidea',
    Order == .$Order[179] ~ 'Hydrozoa',
    Order %in% .$Order[180:200] ~ 'Insecta',
    Order %in% .$Order[201:207] ~ 'Malacostraca',
    Order %in% .$Order[208:234] ~ 'Mammalia',
    Order %in% .$Order[235:237] ~ 'Maxillopoda',
    Order == .$Order[238] ~ 'Merostomata',
    Order == .$Order[239] ~ 'Monoplacophora',
    Order == .$Order[239] ~ 'Myxini',
    Order %in% .$Order[240:243] ~ 'Ostracoda',
    Order %in% .$Order[244:245] ~ 'Polychaeta',
    Order == .$Order[246] ~ 'Polyplacophora',
    Order %in% .$Order[247:250] ~ 'Reptilia',
    Order %in% .$Order[251:253] ~ 'Sarcopterygii',
    Order == .$Order[254] ~ 'Solenogastres',
    Order == .$Order[255] ~ 'Turbellaria',
    Order == .$Order[256] ~ 'Udeonychophora',
    Order == .$Order[257] ~ 'Total')) %>% 
  slice(1:256) %>%
  cbind(Phylum = case_when(
    .$Class %in% c('Actinopterygii', 'Amphibia', 'Aves', 'Cephalaspidomorphi', 'Chondrichthyes', 'Mammalia', 'Reptilia', 'Sarcopterygii') ~ 'Chordata',
    .$Class %in% c('Anthozoa', 'Hydrozoa') ~ 'Cnidaria',
    .$Class %in% c('Arachnida', 'Branchiopoda', 'Chilopoda', 'Collembola', 'Diplopoda', 'Hexanauplia', 'Insecta', 'Malacostraca', 'Maxillopoda', 'Merostomata', 'Ostracoda') ~ 'Arthropoda',
    .$Class %in% c('Asteroidea', 'Echinoidea', 'Holothuroidea') ~ 'Echinodermata',
    .$Class %in% c('Bivalvia', 'Cephalopoda', 'Gastropoda', 'Monoplacophora', 'Polyplacophora', 'Solenogastres') ~ 'Mollusca',
    .$Class %in% c('Clitellata', 'Polychaeta') ~ 'Annelida',
    .$Class %in% c('Enopla') ~ 'Nemertea',
    .$Class %in% c('Turbellaria') ~ 'Platyhelminthes',
    .$Class %in% c('Udeonychophora') ~ 'Onychophora',
    TRUE ~ 'Total'))

phylum_summaries <- clean_animals %>% 
  group_by(Phylum) %>% 
  summarize(
    n_extinct = sum(possible_extinct),
    n_threatened = sum(threatened),
    n_least_concern = sum(least_concern),
    n_deficient = sum(data_deficient),
    total = sum(Total),
    p_extinct = n_extinct/total,
    p_threatened = n_threatened/total,
    p_least_concern = n_least_concern/total,
    p_deficient = n_deficient/total)

write.table(phylum_summaries, 'clipboard', sep = '\t', row.names = F)
    