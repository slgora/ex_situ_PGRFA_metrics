# ex_situ_PGRFA_metrics
names of columns: use MCPD standard as much as possible rather than arbitrary labels. MCPD standard can be found here: https://cgspace.cgiar.org/server/api/core/bitstreams/7947d48c-5cf1-4164-8c61-fa276d658463/content![image](https://github.com/user-attachments/assets/b95cc8cb-f982-4a21-ab18-c4e36a6aeeef)

Always use INSTCODE to identify a collection holder instead of full name

# BGCI dataset issues identified

To be discussed with Colin:
country in BGCI dataset is not country of origin but it is country of institute holding the accession. Therefore the code for this part was deleted. 

# To be done:
BGCI institute code to be added, we need to ask BGCI if they have a conversion table for their garden ID to INSTCODE, otherwise a conversion table has to be done manually. 

SGSV data: Load (done) and clean separately

GLIS data: Fetch data with API, and clean separately

# To be discussed:
1)Regions of diversity variables: 
Function assign True/False value based on country of collection, SAMPSTAT not taken into account, the distinction of Landraces (i.e. on the metric count only Landraces) with other material could be done when computing the metrics. To be further thought. Or for Breeding and research material the value could be NA rather than false (i.e. not applicable).  

2)Intended use of "isCrop" Variable (Currently replaced with crop_status variable )






