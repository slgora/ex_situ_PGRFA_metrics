### 

library(tidyr)
library(httr)
library(jsonlite)

# read dataset 
df = read.csv("../../Data_processing/1_merge_data/DATE_OF_RUN/gen_wiews_df.csv", header = TRUE )

####################
# load functions
source('Functions/Query_taxa_resolver.R')   # import function query_taxa_resolver
source('Functions/Process_taxa_resolver_results.R') # import function extract_best_result

# taxa list to be standardised
taxa_list <- unique(trimws(na.omit(df$fullTaxa)))

# loop trough taxa list and query the API
result_queries_WFO <- list()
result_queries_GRIN <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))
  best_result_GRIN <- query_taxa_resolver(i, c('6'))
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  result_queries_GRIN <- append(result_queries_GRIN, list(best_result_GRIN))
  counter <- counter + 1
}

# extract best result from the result of the queries
res_WFO <- extract_best_result(result_queries_WFO)
res_GRIN <- extract_best_result(result_queries_GRIN)
# create a taxa dictionary to be used to map the standardised taxa to the taxa in the dataset
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
taxa_standardized_df_GRIN <- as.data.frame(do.call(rbind, res_GRIN))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
colnames(taxa_standardized_df_GRIN) <- c('input_name', 'matched_name_GRIN', 'match_type_GRIN', 'status_GRIN', 'output_name_GRIN')

taxa_standardized_df_WFO <- cbind(taxa_standardized_df_WFO, data_source = "WFO")
taxa_standardized_df_GRIN <- cbind(taxa_standardized_df_GRIN, data_source = "GRIN")

# Join results from GRIN with results from WFO
taxa_standardized_df <- taxa_standardized_df_GRIN %>%
  full_join(taxa_standardized_df_WFO, by = c("input_name" = "input_name")) 

# save table with results from both WFO and GRIN
df_save_results <- apply(taxa_standardized_df,2,as.character)
write.csv(df_save_results, '../../Data_processing/2_standardize_taxa/standardized_taxaDATE.csv', row.names = FALSE)
