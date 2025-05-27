# library(readxl) us needed
load_SGSV_data = function(filepath){
  SGSV_allcrops <- read_excel(filepath)
  # Change column names
  colnames(SGSV_allcrops) <- c( "source","instName","INSTCODE","instAcronym","ACCENUMB",
                                "fullTaxa","GENUS_SPECIES","origctyFullName") #origCtyFullName
  ## Keep fields
  SGSV_allcrops <- subset(SGSV_allcrops, select = c(INSTCODE, instAcronym, 
                                                    ACCENUMB, fullTaxa, GENUS_SPECIES, 
                                                    origctyFullName))
  # create SPECIES and GENUS field
  SGSV_allcrops  <- SGSV_allcrops  %>% separate(GENUS_SPECIES, c('GENUS', 'SPECIES'))
  return(SGSV_allcrops)
}

