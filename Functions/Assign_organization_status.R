assign_org_type = function(df, table_FAO_WIEWS){
  # take a dataframe with INSTCODE column and table_FAO_WIEWS and assign organization status based on the status reported in table_FAO_WIEWS
  # return dataframe with added column ORGANIZATIONTYPE
  organizations_type <- setNames(table_FAO_WIEWS$ORGANIZATIONTYPE, table_FAO_WIEWS$INSTCODE)
  df$ORGANIZATIONTYPE = NA
  df <- df %>%
    mutate(ORGANIZATIONTYPE = ifelse(is.na(ORGANIZATIONTYPE), organizations_type[INSTCODE], ORGANIZATIONTYPE))  
  return(df)
}
