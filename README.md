# REPOSITORY FOR EX SITU PGRFA METRICS
Repository with the methodology to produce a set of metrics on the ex-situ conservation of PGRFA

It includes: 

0)Downloading data (Genesys, WIEWS, BGCI, GLIS, SGSV)
Only GLIS data downloaded through API. 

1)Loading and merging data from different sources (Genesys, WIEWS, BGCI, GLIS, SGSV)

2)Standardising taxa

3)Computing the metrics
This work is currently on-going. 

# General
names of columns: use MCPD standard as much as possible rather than arbitrary labels. MCPD standard can be found here: https://cgspace.cgiar.org/server/api/core/bitstreams/7947d48c-5cf1-4164-8c61-fa276d658463/content![image](https://github.com/user-attachments/assets/b95cc8cb-f982-4a21-ab18-c4e36a6aeeef)

Always use INSTCODE to identify a collection holder instead of full name

# BGCI dataset issues identified

To be discussed with Colin:
country in BGCI dataset is not country of origin but it is country of institute holding the accession. Therefore, the code for this part was deleted. 

# To be done:
BGCI institute code to be added, we need to ask BGCI if they have a conversion table for their garden ID to INSTCODE, otherwise a conversion table has to be done manually by searching FAO WIEWS organization database (They are not too many so it is doable). 

SGSV data: Load (done) and clean separately


Load_PTFTW_dataset.R (in Functions folder): check why I don't have GCCS-Metrics_croplist.xlsx file that includes column PlantsthatFeedtheWorld_name (as in Sarah's code), edit as a function. 

Sarah: where is the GCCS-Metrics_croplist.xlsx file that includes column PlantsthatFeedtheWorld_name ?

# On-going
Cleaning up, updating, and streamlining the code to estimate the metrics

# To be discussed:
1)Intended use of "isCrop" Variable (Currently replaced with crop_status variable )

2)Assigning variable Crop strategy: currently, this is done before standardizing taxa, but I think this should be done after. TBD with Colin.  


# Notes on current implementation
Country codes: country codes for Yugoslavia (YUG) and Czechoslovakia (CSK) are not recoded to new countries as this would lead to multiple countries for one accession, which would cause problems when computing the metrics. Instead, assess the number of accession with YUG and CSK in ORGICTY field and their impact on the metric.  

Regions of diversity variables: 
Function assign True/False value based on country of collection. Then the metrics (metric 6) count only accessions with SAMPSTAT < 399 and SAMPSTAT with missing values (NA), i.e. landraces, wild, and weedy material. 

GLIS data:
Fetched JSON data with GLIS API for each of the genus. Selected fields were then extracted from each JSON to create a dataframe, this was then saved as a csv file. 



