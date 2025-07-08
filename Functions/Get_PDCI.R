
# PDCI function

## Note: PDCI function takes into account 28 columns of passport data
# `GENUS`,`SPECIES`,`SPAUTHOR`,SUBTAXA`, SUBTAUTHOR`,`CROPNAME`,`ACQDATE`,`SAMPSTAT`,
# `DONORCODE`,`DONORNAME`,`DONORNUMB`,`OTHERNUMB`,`DUPLSITE`,`STORAGE`,`ACCEURL`,`MLSSTAT`= mlsStat
# `ORIGCTY`,`COLLSITE`,`DECLATITUDE`, `DECLONGITUDE`,`ELEVATION`, `COLLDATE`
# `BREDCODE`, `ANCEST`, `COLLSRC`, `ACCENAME`,`COLLNUMB`,`COLLCODE`

get_PDCI <- function(df) {
  # Initialize PDCI column with 0
  df$PDCI <- 0
  
  # Apply conditions and update PDCI
  df$PDCI <- ifelse(!is.na(df$GENUS), df$PDCI + 120, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !df$SPECIES %in% c('sp', 'sp.', 'spp.', 'spp'), df$PDCI + 80, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SPAUTHOR), df$PDCI + 5, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SUBTAXA), df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SUBTAXA) & !is.na(df$SUBTAUTHOR), df$PDCI + 5, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$CROPNAME), df$PDCI + 45, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ACQDATE), df$PDCI + 10, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$SAMPSTAT), df$PDCI + 80, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DONORCODE), df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DONORNAME) & is.na(df$DONORCODE), df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DONORNUMB) & (!is.na(df$DONORCODE) | !is.na(df$DONORNAME)), df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DONORNUMB) & is.na(df$DONORCODE) & is.na(df$DONORNAME), df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$OTHERNUMB), df$PDCI + 35, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DUPLSITE), df$PDCI + 30, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$STORAGE), df$PDCI + 15, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ACCEURL), df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$MLSSTAT), df$PDCI + 15, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$ORIGCTY) & df$SAMPSTAT %/% 100 < 4, df$PDCI + 80, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ORIGCTY) & (df$SAMPSTAT %/% 100 >= 4 | is.na(df$SAMPSTAT)), df$PDCI + 40, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 < 3 & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI + 70, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 == 3 & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI + 45, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 < 3 & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 == 3 & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI + 15, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)) & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI + 10, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSITE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)) & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI + 20, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & df$SAMPSTAT %/% 100 < 3, df$PDCI + 120, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 80, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)), df$PDCI + 30, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$ELEVATION) & df$SAMPSTAT %/% 100 < 3, df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ELEVATION) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 15, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ELEVATION) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)), df$PDCI + 5, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLDATE) & df$SAMPSTAT %/% 100 < 4, df$PDCI + 30, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ANCEST) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI + 40, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 < 3, df$PDCI + 30, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 50, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 4, df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 5, df$PDCI + 20, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLSRC) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI + 25, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 50, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 4, df$PDCI + 80, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 5, df$PDCI + 160, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$ACCENAME) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI + 40, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLNUMB) & df$SAMPSTAT %/% 100 < 3, df$PDCI + 60, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLNUMB) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLNUMB) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI + 20, df$PDCI)
  
  df$PDCI <- ifelse(!is.na(df$COLLCODE) & df$SAMPSTAT %/% 100 < 3, df$PDCI + 40, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLCODE) & df$SAMPSTAT %/% 100 == 3, df$PDCI + 30, df$PDCI)
  df$PDCI <- ifelse(!is.na(df$COLLCODE) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI + 20, df$PDCI)
  
  # Normalize PDCI by dividing by 100
  df$PDCI <- df$PDCI / 100
  
  return(df)
}
