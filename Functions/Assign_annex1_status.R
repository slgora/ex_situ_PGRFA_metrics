# ASSIGN ANNEX 1 DO THIS STEPS ONLY AFTER TAXA STANDARDIZATION
#### Install packages ####
# tidyverse already include tidyr , dplyr, readr, magrittr, stringr, readxl
install.packages("tidyverse")
library(tidyverse)
library(readxl)
# one would need to change the path for the file containing the list of Petota and Melongena species
#function taking a dataframe including a column taxa names and returning TRUE/FALSE 
assign_annex1status = function(df, standardize_taxa = 'Standardized_taxa') {
  df <- df %>%
    mutate(GENUS   = word(!!sym(standardize_taxa), 1),  # Extract the first word (genus)
           SPECIES = word(!!sym(standardize_taxa), 2))  # Extract the second word (species)
  
  df$GENUS_SPECIES <- trimws(paste(df$GENUS, df$SPECIES))
  
  Genus_annex1_food = c('Hordeum', 'Ipomoea', 'Lathyrus', 'Lens', 'Malus', 'Musa', 'Oryza', 'Pennisetum', 'Cenchrus', 'Phaseolus', 'Pisum', 
                        'Secale', 'Sorghum', 'Triticosecale', 'Triticum', 'Aegilops' , 'Agropyron', 'Elymus', 'Secale', 'Vicia', 'Vigna' , 'Zea',
                        'Asparagus' , 'Avena' , 'Beta' , 'Brassica' , 'Armoracia' , 'Barbarea' , 'Camelina' , 'Crambe' , 'Diplotaxis', 
                        'Eruca', 'Isatis' , 'Lepidium', 'Raphanobrassica', 'Raphanus', 'Rorippa', 'Sinapis' , 'Cajanus' , 'Cicer',
                        'Citrus' , 'Cocos' , 'Colocasia' , 'Xanthosoma' , 'Daucus' , 'Dioscorea' , 'Eleusine' , 'Fragaria' , 'Helianthus')
  species_annex1_food = c('Artocarpus altilis' , 'Manihot esculenta')
  
  species_annex1_forages = c('Astragalus chinensis', 'Astragaslus cicer' , 'Astragalus arenarius' , 'Canavalia ensiformis' , 
                             'Coronilla varia' , 'Hedysarum coronarium' , 'Lathyrus cicera' , 'Lathyrus ciliolatus' , 'Lathyrus hirsutus', 
                             'Lathyrus ochrus' , 'Lathyrus odoratus' , 'Lathyrus sativus', 'Lespedeza cuneata' , 'Lespedeza striata' , 
                             'Lespedeza stipulacea' , 'Lotus corniculatus', 'Lotus subbiflorus' , 'Lotus uliginosus' , 
                             'Lupinus albus', 'Lupinus angustifolius' , 'Lupinus luteus' , 'Medicago arborea' , 'Medicago falcata' , 
                             'Medicago sativa' , 'Medicago scutellata' , 'Medicago rigidula' , 'Medicago truncatula', 
                             'Melilotus albus', 'Melilotus officinalis' , 'Onobrychis viciifolia', 'Ornithopus sativus' , 
                             'Prosopis affinis', 'Prosopis alba', 'Prosopis chilensis' , 'Prosopis nigra', 'Prosopis pallida', 
                             'Pueraria phaseoloides', 'Trifolium alexandrinum' , 'Trifolium alpestre', 'Trifolium ambiguum', 'Trifolium angustifolium', 
                             'Trifolium arvense', 'Trifolium agrocicerum', 'Trifolium hybridum', 'Trifolium incarnatum', 'Trifolium pratense', 'Trifolium repens', 'Trifolium', 'Trifolium',
                             'Trifolium resupinatum', 'Trifolium rueppellianum', 'Trifolium semipilosum', 'Trifolium subterraneum', 'Trifolium vesiculosum', 
                             'Securigera varia', 'Sulla coronaria', 'Kummerowia striata', 'Kummerowia stipulacea', 'Neltuma alba', 
                             'Neltuma chilensis', 'Neltuma nigra', 'Neltuma pallida', 'Neustanthus phaseoloides')
  
  exclude = c('Lepidium meyenii' , 'Musa textilis' , 'Phaseolus polyanthus', 'Phaseolus dumosus' ,'Zea perennis' , 'Zea diploperennis' , 'Zea luxurians', 'Solanum phureja')
  
  section_Petota <- read_excel("../../Data_processing/Support_files/Annex1_crops/Solanum_section_Petota_Species_GRIN-Global.xlsx") %>%
    mutate(GENUS = word(Name, 1),
           SPECIES = word(Name, 2))
  Petota_species = as.list(trimws(section_Petota$SPECIES))
  
  section_Melongena <- read_excel("../../Data_processing/Support_files/Annex1_crops/Section_Melongena_Species_GRIN-Global.xlsx") %>%
    mutate(GENUS = word(Name, 1),
           SPECIES = word(Name, 2))
  section_Melongena = as.list(trimws(section_Melongena$SPECIES))
  
  df$Annex1 = FALSE
  df$Annex1 <- ifelse(df$GENUS %in% Genus_annex1_food, TRUE, df$Annex1)
  df$Annex1 <- ifelse(df$GENUS_SPECIES %in% species_annex1_forages, TRUE, df$Annex1)
  
  # INSERTED LINE: match species_annex1_food
  df$Annex1 <- ifelse(df$GENUS_SPECIES %in% species_annex1_food, TRUE, df$Annex1)
  
  df$Annex1 <- ifelse((df$GENUS == 'Solanum') & (df$SPECIES %in% Petota_species), TRUE, df$Annex1)
  df$Annex1 <- ifelse((df$GENUS == 'Solanum') & (df$SPECIES %in% section_Melongena), TRUE, df$Annex1)
  
  df$Annex1 <- ifelse(df$GENUS_SPECIES %in% exclude , FALSE, df$Annex1)
  df$Annex1 <- ifelse(grepl('Phaseolus × dumosus', df[[standardize_taxa]], ignore.case = TRUE), FALSE, df$Annex1)
  df$Annex1 <- ifelse(grepl('Phaseolus X dumosus', df[[standardize_taxa]], ignore.case = TRUE), FALSE, df$Annex1)
  
  return(df)
}
