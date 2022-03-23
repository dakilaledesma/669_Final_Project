
# setup -------------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(ordinal)

full_dataset <- read_csv('no_pan_dataset.csv')

for_model <- full_dataset %>% 
  filter(iucn_category %in% as.character(1:5)) %>% 
  mutate(iucn_category = factor(iucn_category, levels = 1:5)) %>% 
  distinct()


# data assessment --------------------------------------------------------

# rows with depth in meters = 5652
nrow(for_model[!is.na(for_model$depth_num),])

# rows with depth as zone = 9231
nrow(for_model[!is.na(for_model$depth_zone),])

# rows with trophic level = 1672
nrow(for_model[!is.na(for_model$trophic_level),])

# rows with diet = 501
nrow(for_model[!is.na(for_model$diet),])

# rows with lat and long range = 1724
nrow(for_model[!is.na(for_model$long_range),])

# rows with length = 8627
nrow(for_model[!is.na(for_model$length_m),])

# rows with mass = 891
nrow(for_model[!is.na(for_model$mass_g),])

# rows with mature age = 1802
nrow(for_model[!is.na(for_model$mature_age),])

# rows with longevity = 1802
nrow(for_model[!is.na(for_model$longevity),])

all_vars <- for_model %>% 
  filter(
    !is.na(lat_range),
    !is.na(long_range),
    !is.na(trophic_level),
    !is.na(mature_age),
    !is.na(longevity),
    !is.na(diet),
    !is.na(mass_g),
    !is.na(length_m),
    !is.na(depth_num),
    !is.na(depth_zone))

# rows with all variables = 101
nrow(all_vars)

# distribution with all variables
all_vars %>% 
  group_by(iucn_category) %>% 
  summarize(n = n())

# correlation matrix for numeric
pMatrix <- function(mat, ...){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p_vals <- pMatrix(
  for_model %>% 
    select(c(mature_age:long_range,trophic_level:mass_g,length_m:depth_num)) %>% 
    filter(
      !is.na(lat_range),
      !is.na(long_range),
      !is.na(trophic_level),
      !is.na(mature_age),
      !is.na(longevity),
      !is.na(mass_g),
      !is.na(length_m),
      !is.na(depth_num)))

for_model %>% 
  select(c(mature_age:long_range,trophic_level:mass_g,length_m:depth_num)) %>% 
  filter(
    !is.na(lat_range),
    !is.na(long_range),
    !is.na(trophic_level),
    !is.na(mature_age),
    !is.na(longevity),
    !is.na(mass_g),
    !is.na(length_m),
    !is.na(depth_num)) %>% 
  select(c(mature_age:long_range,trophic_level:mass_g,length_m:depth_num)) %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'number',
    type = 'upper',
    tl.col = 'black',
    p.mat = p_vals,
    sig.level = 0.01,
    insig = 'blank')

# correlation between diet and trophic level - p = 0, R^2 = 0.20
summary(lm(
  trophic_level ~ diet,
  data = for_model %>% 
    filter(
      !is.na(trophic_level),
      !is.na(diet))))

# correlation between diet and trophic level - p = 0, R^2 = 0.47
summary(lm(
  depth_num ~ depth_zone,
  data = for_model %>% 
    filter(
      !is.na(depth_num),
      !is.na(depth_zone))))

# modeling ----------------------------------------------------------------

# 1246 rows selected for vars with correlation R^2 <= 0.35 based on p < 0.01, not including diet because of sample limiting to 214 if included
base_data <- for_model %>% 
  filter(
    !is.na(lat_range),
    !is.na(trophic_level),
    !is.na(longevity),
    longevity > 0,
    !is.na(length_m),
    !is.na(depth_zone))

base_data %>% 
  group_by(iucn_category) %>% 
  summarize(n = n())

base_mod <- clm(
  iucn_category ~ lat_range + trophic_level + longevity + length_m + depth_zone,
  data = base_data)

# lat range insignificant at p < 0.05, others all significant at p < 0.001 or p = 0
anova(base_mod, type = 'III')

no_lat_mod <- clm(
  iucn_category ~ trophic_level + longevity + length_m + depth_zone,
  data = base_data)

# zones aren't really different
confint(no_lat_mod)

# if we group depth zones to pelagic, demersal, and reef-associated, Hessian is not numerically singular

three_depth_data <- base_data %>%  
  mutate(
    zone = case_when(
      depth_zone %in% c('benthopelagic','pelagic','bathypelagic','pelagic-oceanic','pelagic-neritic') ~ 'pelagic',
      depth_zone %in% c('demersal','bathydemersal') ~ 'demersal',
      depth_zone == 'reef-associated' ~ 'reef-associated'))

three_depth_mod <- clm(
  iucn_category ~ lat_range + trophic_level + longevity + length_m + zone, 
  data = three_depth_data)

# lat range significant at p < 0.05 based on ANODE w/ Wald chi-square, all other significant at p < 0.01
anova(three_depth_mod, type = 'III')

# AIC increases by 3.5 with dropping lat range - does not justify removal
drop1(three_depth_mod, test = 'Chi')

# only the 95% CIs for the reef-associated zone overlaps zero (pelagic differs from demersal)
confint(three_depth_mod)

# reef-associated does not differ from pelagic
confint(clm(
  iucn_category ~ lat_range + trophic_level + longevity + length_m + zone, 
  data = three_depth_data %>% 
    mutate(
      zone = factor(zone, levels = c('pelagic','demersal','reef-associated')))))

overallAccuracy <- function(data, model){
  data %>% 
    cbind(
      predict(
        model,
        type = 'class')) %>% 
    select(genus_species, fit) %>% 
    right_join(three_depth_data, by = 'genus_species') %>% 
    mutate(diff = abs(as.numeric(fit) - as.numeric(iucn_category))) %>% 
    summarize(
      percent_correct = sum(diff==0)/length(diff),
      mean_diff = mean(diff))
}

classAccuracy <- function(data, model){
  data %>% 
    cbind(
      predict(
        model,
        type = 'class')) %>% 
    select(genus_species, fit) %>% 
    right_join(three_depth_data, by = 'genus_species') %>% 
    mutate(diff = abs(as.numeric(fit) - as.numeric(iucn_category))) %>% 
    group_by(iucn_category) %>% 
    summarize(
      percent_correct = sum(diff==0)/length(diff),
      mean_diff = mean(diff))
}

# model sucks: 99.8% accurate for 1 predictions, all wrong for others except 21.7% accuracy for 5
classAccuracy(three_depth_data,three_depth_mod)

# indicates length_m should be nominal - still unsure of meaning
nominal_test(three_depth_mod)

mod2 <- clm(
  iucn_category ~ lat_range + trophic_level + longevity + zone, 
  nominal = ~ length_m,
  data = three_depth_data)

anova(mod2, type = 'III')

# somewhat better accuracy overall, but still 0% for 2 and less than 12% for others
classAccuracy(three_depth_data, mod2)

# indicates all except lat range could be scale effects (longev then zone then trophic) - still unsure of meaning
scale_test(mod2)

mod3 <- clm(
  iucn_category ~ lat_range + trophic_level + zone, 
  nominal = ~ length_m,
  scale = ~ longevity,
  data = three_depth_data)

# scale mod does worse
classAccuracy(three_depth_data, mod3)

set.seed(42)

# everything sucks, let's play with resampling ----------------------------

# 1079 is sample size of cat 1
boot_data <- map(
  2:5,
  function(x){
    sample_n(
      three_depth_data[three_depth_data$iucn_category == x,],
      size = 1079,
      replace = T)
  }) %>% 
  bind_rows() %>% 
  rbind(three_depth_data[three_depth_data$iucn_category == 1,])

boot_mod1 <- clm(
  iucn_category ~ lat_range + trophic_level + longevity + length_m + depth_zone,
  data = boot_data)

# everything matters
anova(boot_mod1, type = 'III')

# wow, some of the zones even matter
confint(boot_mod1)

# lowest AIC is with all included, next best is a 43 increase by removing lat range
drop1(boot_mod1, test = 'Chi')

classAccuracy(boot_data, boot_mod1)

overallAccuracy(boot_data, boot_mod1)

nominal_test(boot_mod1)

boot_nom1 <- clm(
  iucn_category ~ lat_range + trophic_level + longevity + depth_zone,
  nominal = ~ length_m,
  data = boot_data)

overallAccuracy(boot_data, boot_nom1)

classAccuracy(boot_data, boot_nom1)

nominal_test(boot_nom1)

boot_nom2 <- clm(
  iucn_category ~ lat_range + trophic_level + depth_zone,
  nominal = ~ length_m + longevity,
  data = boot_data)

overallAccuracy(boot_data, boot_nom2)

classAccuracy(boot_data, boot_nom2)

nominal_test(boot_nom2)

# bad
boot_nom3 <- clm(
  iucn_category ~ trophic_level + depth_zone,
  nominal = ~ length_m + longevity + lat_range,
  data = boot_data)

overallAccuracy(boot_data, boot_nom3)

classAccuracy(boot_data, boot_nom3)
