
drug_info <- function(){shiny::mainPanel(
p("More info, including Stats Can citation, disclaimer & code on", a("github", href = "https://github.com/ADrugResearcher/Canadian-Drug-Waste-Water")),
h3("Info about wastewater epidemiology"),
p("Values in this dataset are, 'estimates of the excreted amount of drug metabolites in the wastewater and do not correspond directly to consumption levels of the parent drugs'. See Statistics Canada's", 
  a("'Additional Information'",
                                  href = "https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024021-eng.htm"),
  "section for more details on limitations & methods. Wastewater sources only cover the population of people served by said wastewater treatment plant."),
p("Lastly, some substances (such as morphine) are excreted either as metabolites of other drugs (e.g., heroin, hydromorphone, codeine), whereas others are more specific (e.g., cannabis, methamphetamine). Many of these drugs are also not necessarily always used illicitly"),
h3("Some thanks!"),
p("Thanks to Karen Ward & Molly R. for their feedback, and pointing me towards the data sources"),
p("Molly also helped with thinking through how to visualize the imputatation rate")
)}