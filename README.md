# Canadian-Drug-Waste-Water
Based off Stats Canada data

Original visualization here: https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024021-eng.htm

In the original visualization, statscan has used facet_wrap to compare different cities & automatically set the scaling to "free scale", which means that data from TO & Vancouver have different y-axes.

This builds off of Stats Canada's original visualization. 

**Imputation rate:**
I have also added an additional variable for imputation rate (basically replacing missing values based on pre-existing values)
Most of the time this is not an issue, but there are a few extreme cases (Edmonton 2022-08, Prince Albert 2022-09, Saskatoon 2022-10), which are worth flagging.

**Data Citation:**

Data pulled from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310087101&request_locale=en

Statistics Canada. Table 13-10-0871-01  Drug metabolites in wastewater in select Canadian cities, by month, 2022 to 2023
DOI: https://doi.org/10.25318/1310087101-eng
