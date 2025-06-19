# ASSIGN ANNEX 1 DO THIS STEPS ONLY AFTER TAXA STANDARIXATION
#### Install packages ####
# tidyverse already include tidyr , dplyr, readr, magrittr, stringr, readxl
install.packages("tidyverse")
library(tidyverse)
library(readxl)
# one would need to change the path for the file contaiing the list of Petota and Melongena species
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
  species_annex1_food = c('Artocarpus altilis' , 'Manihot esculenta' )

  # check against GRIN_taxonomy
  # generate a list of synonims from GRIN taxonomy to check against
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

  #Aegilops was included as assuming Triticum et al. includes it
  exclude = c('Lepidium meyenii' , 'Musa textilis' , 'Phaseolus polyanthus', 'Phaseolus dumosus' ,'Zea perennis' , 'Zea diploperennis' , 'Zea luxurians', 'Solanum phureja')
  # potato Section tuberosa included, except Solanum phureja.
  section_Petota <- read_excel("../data_6/processing/Solanum_section_Petota_Species_GRIN-Global.xlsx")
  # Split the "Name" column into "genus" and "species" 
  section_Petota <- section_Petota %>%
    mutate(GENUS   = word(Name, 1),  # Extract the first word (genus)
           SPECIES = word(Name, 2))  # Extract the second word (species)

  Petota_species = as.list(trimws(section_Petota$SPECIES))

  #eggplant Section melongena included species list in file Section_Melongena_GRIN-Global.xlsx
  section_Melongena <- read_excel("../data_6/processing/Section_Melongena_Species_GRIN-Global.xlsx")
  # Split the "fullSciName" column into "genus" and "species" without removing "fullSciName"
  section_Melongena <- section_Melongena %>%
    mutate(GENUS   = word(Name, 1),  # Extract the first word (genus)
           SPECIES = word(Name, 2))  # Extract the second word (species)

  section_Melongena = as.list(trimws(section_Melongena$SPECIES))

  # start with all FALSE
  df$Annex1 = FALSE
  df$Annex1 <- ifelse(df$GENUS %in% Genus_annex1_food, TRUE, df$Annex1) # if genus is in list genus annex 1 = True
  df$Annex1 <- ifelse(df$GENUS_SPECIES %in% species_annex1_forages, TRUE, df$Annex1) 
  
  # check if species is Potato or its CWRs
  df$Annex1 <- ifelse( (df$GENUS == 'Solanum') & (df$SPECIES %in% Petota_species), TRUE, df$Annex1) 
  
  # check if species is Eggplants or its CWRs
  df$Annex1 <- ifelse( (df$GENUS == 'Solanum') & (df$SPECIES %in% section_Melongena), TRUE, df$Annex1) 
  
  # exclude some species
  df$Annex1 <- ifelse(df$GENUS_SPECIES %in% exclude , FALSE, df$Annex1) 

  return(df)
} # end of function
