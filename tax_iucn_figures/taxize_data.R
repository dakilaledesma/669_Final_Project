library(dplyr)
library(tidyverse)
#install.packages(taxize)
library(taxize)
#set wd only needed in your personal R
setwd("~/Documents/UNC 2021-22/Spring/BIOL669/tax_iucn_figures")

#full compiled dataset of all fish species
wdata <- read.csv("no_pan_dataset.csv")
wdata <- wdata %>% drop_na(genus_species)
#this line runs the code to get all the genus, family, order and class for each species
#this line will also take ~8hrs to run, I would suggest running it on a cluster or online source
wd_fulltax_itis <- tax_name(wdata$genus_species, c("genus","family","order", "class"), db = 'itis', ask = FALSE)
#you can always press esc to make it stop if you accidentally starting running this :)
#If you want to use NCBI as your database instead of itis, you need to have an NCBI ENTREZ key 
#labeled as "ENTREZ_KEY=####etc" in your .Renviron
#you don't need an ENTREZ key for using itis, and the taxonomy is better so use ITIS if you can!

#write all data from taxize NCBI db to a CSV
write.csv(wd_fulltax_itis, "wd_fulltax_itis.csv")

#editing and combining data after adding higher taxon level data
wd_fulltax_itis <- read.csv("wd_fulltax_itis.csv")
wd_fulltax_itis <- wd_fulltax_itis %>% rename(genus_species = query) %>% select(genus_species, genus, family, order, class)

#put all data frames into a list
all_tax_data <- list(wd_fulltax_itis, wdata)      

#merge all data frames together
all_tax_data <- all_tax_data %>% reduce(full_join, by='genus_species')
#take out two glaringly obvious incorrect taxonomy assignments
all_tax_data <- all_tax_data %>% 
  filter(!str_detect(class, 'Aves'))
all_tax_data <- all_tax_data %>% 
  filter(!str_detect(class, 'Insecta'))

#add text labels for IUCN category in dataframe
all_tax_data <- all_tax_data %>%
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

#save dataframe as csv
write.csv(all_tax_data, "all_tax_data_itis.csv")

#all numbers of each taxonomic rank (see tibble:dimensions for number)
all_tax_data %>% group_by(class) %>% summarize(n = n())
all_tax_data %>% group_by(order) %>% summarize(n = n())
all_tax_data %>% group_by(family) %>% summarize(n = n())
all_tax_data %>% group_by(genus) %>% summarize(n = n())
all_tax_data %>% group_by(genus_species) %>% summarize(n = n())

#plot all class data together
all_tax_data %>% 
  ggplot() +
  geom_histogram(aes(x = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class))

####Plots for each class (to scale the different variables)
#Teleostei
Teleostei <- all_tax_data %>% filter(class == "Teleostei") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")

#save plot as 300dpi image
ggsave("Teleostei.png", plot = Teleostei)

#Chondrichthyes
Chondrichthyes <- all_tax_data %>% filter(class == "Chondrichthyes") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")
ggsave("Chondrichthyes.png", plot = Chondrichthyes)

#Myxini
Myxini <- all_tax_data %>% filter(class == "Myxini") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")
ggsave("Myxini.png", plot = Myxini)

#Chondrostei
Chondrostei <- all_tax_data %>% filter(class == "Chondrostei") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")
ggsave("Chondrostei.png", plot = Chondrostei)

#Cephalaspidomorphi
Cephalaspidomorphi <- all_tax_data %>% filter(class == "Cephalaspidomorphi") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")

ggsave("Cephalaspidomorphi.png", plot = Cephalaspidomorphi)

#Coelacanthi
Coelacanthi <- all_tax_data %>% filter(class == "Coelacanthi") %>%
  ggplot() +
  geom_histogram(aes(x = iucn_category, fill = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Number of species") +
  labs(fill = "IUCN Category")

ggsave("Coelacanthi.png", plot = Coelacanthi)

#plot all order data by class
all_tax_data %>% 
  ggplot() +
  geom_histogram(aes(x = order), stat = "count") +
  facet_wrap(facets = vars(class))

#plot all observations of each iucn category by class
all_tax_data %>% 
  ggplot() +
  geom_histogram(aes(x = iucn_category), stat = "count") +
  facet_wrap(facets = vars(class))

#class_percent
class_percent <- all_tax_data %>% 
  group_by(iucn_text,class) %>% 
  summarize(in_group = n()) %>% 
  left_join(
    all_tax_data %>% 
      group_by(iucn_text) %>% 
      summarize(total = n()),
    by = 'iucn_text') %>% 
  mutate(percent = in_group / total * 100) %>% 
  ggplot() +
  geom_col(aes(
    x = iucn_text,
    y = percent,
    fill = class)) +
  scale_fill_viridis_d() +
  xlab("IUCN Category") +
  ylab("Percent") +
  labs(fill = "Class") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#save plot as 300 dpi image
ggsave("class_percent.png", class_percent)

#iucn_class
iucn_class <- all_tax_data %>% 
  group_by(class,iucn_text) %>% 
  summarize(in_group = n()) %>% 
  left_join(
    all_tax_data %>% 
      group_by(class) %>% 
      summarize(total = n()),
    by = 'class') %>% 
  mutate(percent = in_group / total * 100) %>% 
  ggplot() +
  geom_col(aes(
    x = class,
    y = percent,
    fill = iucn_text)) +
  scale_fill_viridis_d() +
  xlab("Class") +
  ylab("Percent") +
  labs(fill = "IUCN Category") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#save plot as 300 dpi image
ggsave("iucn_class.png", iucn_class) 
