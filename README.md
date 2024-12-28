# Canadian-Drug-Waste-Water
Based off Stats Canada data

Original visualization here: https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024021-eng.htm

In the original visualization, statscan has used facet_wrap to compare different cities & automatically set the scaling to "free scale", which means that data from TO & Vancouver have different y-axes.

This builds off of Stats Canada's original visualization. 

**Imputation rate:**
I have also added an additional variable for imputation rate (basically replacing missing values based on pre-existing values)
Most of the time this is not an issue, but there are a few extreme cases (Edmonton 2022-08, Prince Albert 2022-09, Saskatoon 2022-10), which are worth flagging.

Statistic Canada's own explanation of the process of imputation (see citation below):

"Missing samples were multiply imputed to create valid monthly estimates and variances. The imputation model accounts for the location, month, day of week of the missing samples and substances’ levels in the wastewater for other days of the week. From the daily results, the CWS produces monthly estimates of levels (mass loads per capita) of drug metabolites in the wastewater of each municipality."

**Data Citation & Disclaimer:**

Data pulled from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310087101&request_locale=en

Statistics Canada. Table 13-10-0871-01  Drug metabolites in wastewater in select Canadian cities, by month, 2022 to 2023
DOI: https://doi.org/10.25318/1310087101-eng

Data (wastewater.csv) made available under Statistics Canada's Open Licence Agreement. I do not make any claims to owning the data, Statistics Canada does not endorse any of the claims made in the visualization. 

"Reproduced and distributed on an "as is" basis with the permission of Statistics Canada." For more information see: https://www.statcan.gc.ca/en/reference/licence. 

