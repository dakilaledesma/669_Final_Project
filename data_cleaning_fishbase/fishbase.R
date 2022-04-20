setwd("~/Documents/UNC 2021-22/Fall/BIOL669")
#install.packages('rfishbase')
library("rfishbase")
library("dplyr")

#test to look at a table in fishbase
fb_tbl("ecosystem")

#shows all variables
fb_tables()

#I think this is a table of all species in fishbase
fb_tbl("species", "fishbase")

species_table <- fb_tbl("species", "fishbase")

write.csv(species_table, "fishbase_data.csv")

#use dataset that Ana cleaned already
setwd("~/Documents/UNC 2021-22/Spring/BIOL669")
clean_species_table <- read.csv("fishbase_data.csv")
library(dplyr)
library(tidyr)

#delete columns that aren't necessary
clean_species_table <- clean_species_table %>% select(-SpecCode, -SpeciesRefNo, -AuthorRef, -Remark, -Comments)

#omit data with NA in the genus
na.omit(clean_species_table$GenCode)

####Dates####
clean_species_table$DateEntered <- gsub("0:00","",as.character(clean_species_table$DateEntered))
clean_species_table$DateEntered <- gsub("/","-",as.character(clean_species_table$DateEntered))
clean_species_table$DateModified <- gsub("0:00","",as.character(clean_species_table$DateModified))
clean_species_table$DateModified <- gsub("/","-",as.character(clean_species_table$DateModified))
clean_species_table$DateChecked <- gsub("0:00","",as.character(clean_species_table$DateChecked))
clean_species_table$DateChecked <- gsub("/","-",as.character(clean_species_table$DateChecked))
#need to flip the dates around!! can do in excel after!

#importance
clean_species_table['Importance'][clean_species_table ['Importance'] == 'highly commercial'|clean_species_table ['Importance'] == 'commercial'|clean_species_table ['Importance'] == 'minor commercial'|clean_species_table ['Importance'] == 'of potential interest'] <- 1
clean_species_table['Importance'][clean_species_table ['Importance'] == 'of no interest'| clean_species_table ['Importance'] == 'subsistence fisheries'] <- 0
clean_species_table <- clean_species_table %>% select(-ImportanceRef)

#remove all fish that do not live in salt water (fresh, brackish)
clean_species_table <- subset(clean_species_table, clean_species_table$Saltwater !="0")
#then remove columns bc no longer needed
clean_species_table <- clean_species_table %>% select(-Fresh, -Brack, -Saltwater)

clean_species_table <- clean_species_table %>% select(-AnaCat, -MigratRef, -DepthRangeRef, -DepthRangeComShallow, -DepthRangeComDeep, DepthComRef, -LengthFemale, -LTypeMaxM, -LTypeMaxF, -LTypeComM, -LTypeComF, -MaxLengthRef, -CommonLengthF, -CommonLengthRef, -WeightFemale, -MaxWeightRef, -Modified, -DateModified, -DateChecked)
write.csv(clean_species_table, "fishbase_data.csv")

#average depth
clean_species_table$depth_num <- ((clean_species_table$DepthRangeShallow + clean_species_table$DepthRangeDeep)/2)

clean_species_table <- clean_species_table %>% rename(length_m = Length)
clean_species_table <- clean_species_table %>% select(-DepthRangeShallow, -DepthRangeDeep, -DepthComRef)
clean_species_table <- clean_species_table %>% select(-CommonLength)
clean_species_table <- clean_species_table %>% rename(depth_zone = DemersPelag)

clean_species_table <- clean_species_table %>% select(-BodyShapeI)
clean_species_table <- clean_species_table %>% select(-Source)
clean_species_table <- clean_species_table %>% select(-X.1)
clean_species_table <- clean_species_table %>% select(-subfamily)

clean_species_table <- clean_species_table %>% rename(commercial = Importance)

clean_species_table$data_src <- (clean_species_table$data_src = "fbs")

clean_species_table$Author <- gsub(")","",as.character(clean_species_table$Author))
write.csv(clean_species_table, "fishbase_data_new.csv")

clean_species_table <- read.csv("fishbase_data_new.csv")

library(stringr)
clean_species_table$date <- str_sub(clean_species_table$Author, -4)
clean_species_table <- clean_species_table %>% select(-Author)

clean_species_table <- clean_species_table %>% select(-FamCode, -GenCode)

clean_species_table <- clean_species_table %>% rename(mass_g = Weight)

clean_species_table <- clean_species_table %>% select(-Vulnerability)
clean_species_table <- clean_species_table %>% select(-X)
clean_species_table <- clean_species_table %>% select(-DateEntered)

write.csv(clean_species_table, "fishbase_data_clean_final.csv")
