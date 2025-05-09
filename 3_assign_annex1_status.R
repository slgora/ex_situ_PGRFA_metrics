# work in progress
# do as a function taking a taxa and returning TRUE/FALSE 

# ASSIGN ANNEX 1 DO THIS STEPS ONLY AFTER TAXA STANDARIXATION
Genus_annex1_food = c('Hordeum', 'Ipomoea', 'Lathyrus', 'Lens', 'Malus', 'Musa', 'Oryza', 'Pennisetum', 'Phaseolus', 'Pisum', 
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
                            'Trifolium resupinatum', 'Trifolium rueppellianum', 'Trifolium semipilosum', 'Trifolium subterraneum', 'Trifolium vesiculosum')

#Aegilops was included as assuming Triticum et al. includes it
exclude = c('Lepidium meyenii' , 'Musa textilis' , 'Phaseolus polyanthus', 'Zea perennis' , 'Zea diploperennis' , 'Zea luxurians')
# potato Section tuberosa included, except Solanum phureja., eggplant Section melongena included species list in file Section_Melongena_GRIN-Global.xlsx
#species annex 1 here make list
# assign based on the list above

# start with all NA
combined_df2$Annex1 = NA
combined_df2$Annex1 <- ifelse(combined_df2$GENUS %in% Genus_annex1_food, TRUE, combined_df2$Annex1) # if genus is in list genus annex 1 = True
combined_df2$Annex1 <- ifelse(combined_df2$GENUS_SPECIES %in% species_annex1_forages, TRUE, combined_df2$Annex1) 
# if species is in included list = True
# if species is in excluded list = False
# add potato and eggplant separately