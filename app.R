# install.packages(c("Shiny", "readr", "tidyverse",
# "clocks", "renv"), dependencies = TRUE)
library(readr)
library(tidyverse)

#Some functions for managing the y-axes, so they're consistent
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
`%notin%` <- Negate(`%in%`)
####-------------------------Data Import---------------------------------####
# data sourced from: 
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310087101&request_locale=en
# Original Visualization:
# https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024021-eng.htm
wastewater <- read_csv("wastewater.csv", 
                       col_types = cols(DGUID = col_skip(), 
                                        UOM_ID = col_skip(),
                                        SCALAR_ID = col_skip(),
                                        VECTOR = col_skip(),
                                        COORDINATE = col_skip(),
                                        SYMBOL = col_skip(),
                                        STATUS = col_skip(),
                                        
                                        TERMINATED = col_skip()))
####-------------------------Data Cleaning---------------------------------####
colnames(wastewater) <- c("ref_date", "site", "drug", "name", 
                          "title", "unit", "value",
                          "decimals")
#Creating a real date variable + shorting measure names
waste <- wastewater |> 
  mutate(tdate = ymd(paste0(ref_date, "-01"))) |> 
  mutate(name = str_replace_all(name, c(
    "\\QLoad per capita (milligrams per one thousand people per day)\\E" = "Load",
    "Low 95% confidence interval, load per capita" = "low_conf",
    "High 95% confidence interval, load per capita" = "high_conf",
    "Standard error, load per capita" = "std_error",
    "Imputation rate, load per capita" = "imputation_rate"
  ))) |>
  select(-c(unit, decimals))

# Turns the CI's into their own columns & makes the imputation rate ordinal
# Mostly the imputation rate is 0, but there are a few outliers
waste1 <- waste |> 
  pivot_wider(names_from = name, values_from = value)
waste1 <- waste1 |> 
  mutate(imputation_rate1 = case_when(
    imputation_rate == 0 ~ "None",
    imputation_rate < 20  ~ "Low",
    between(imputation_rate, 20,50) ~ "Medium",
    imputation_rate == 100 ~ "High"
  )) |> 
  mutate(imputation_rate1 = factor(imputation_rate1, 
                                   levels = c("None", "Low", "Medium", "High"),
                                   ordered = TRUE))
#Shortening province names
waste1 <- waste1 |> 
  mutate(site = str_replace_all(site, c(
    "Weighted average, cities measured" = "Weighted average\n(all cities)",
    "Alberta" = "AB",
    "British Columbia" = "BC",
    "Nova Scotia" = "NS",
    "Quebec" = "QC",
    "Ontario" = "ON",
    "Saskatchewan" = "SK"
  )))

####--------------------Create In Shiny Variables--------------------------####
provinces <- c(", AB", ", BC", ", NS", ", QC", ", ON", ", SK")


#Rounding for the max for the y-axis - by substance
minmax_d <- waste1 |> 
  group_by(drug) |> 
  summarise(min = min(Load), max = max(high_conf)) |> 
  ungroup() |> 
  mutate(max = case_when(
    max < 100 ~ round((max+10)/10)*10,
    between(max, 100, 500) ~ round((max+30)/10)*10,
    between(max, 500, 800) ~ round((max+50)/100)*100,
    max > 800 ~ round((max+100)/100)*100)
  )
# Same thing but substance & city
# Make sure to use the high_conf variable as otherwise the error bar wil not
# fit!!!!
minmax_ct <- waste1 |> 
  group_by(drug, site) |> 
  summarise(min = min(Load), max = max(high_conf)) |> 
  ungroup() |> 
  mutate(max2 = case_when(
    max < 100 ~ round((max+10)/10)*10,
    between(max, 100, 500) ~ round((max+30)/100)*100,
    between(max, 500, 800) ~ round((max+50)/100)*100,
    max > 800 ~ round((max+100)/100)*100)
  )

#Pretty y axis
y_labs <- gsub("day ", "day\n", waste1$title)

#Using Wong's palette for distinct colourblind friendly colours
# sourced from:
# https://davidmathlogic.com/colorblind/#%23D81B60-%231E88E5-%23FFC107-%23004D40
mycolours <- c("#000","#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(mycolours) <- unique(waste1$site)

myshapes <- c(19, 25, 15, 17)

date_range <- seq.Date(min(waste1$tdate), max(waste1$tdate), by = "1 month")


# Breaking the Site options into 2 groups for the 2nd tab
site1 <- c("Weighted average\n(all cities)", "Vancouver, BC",
           "Edmonton, AB", "Toronto, ON")
site2 <- unique(waste1$site[waste1$site %notin% site1])


####---------------------------------UI------------------------------------####
library(shiny)
library(shinythemes)
source("text_info.R")
ui <- navbarPage(title = "Select Drug Waste Water Analysis",
                 theme = shinytheme("flatly"),
                 #####------------------------Tab Panel 1---------------------------------####
                 tabPanel("By Location",
                          fluidRow(
                            column(width = 4,
                                   selectInput("site", "Site", choices = unique(waste1$site),
                                               selected = "Weighted average, cities measured")),
                            column(width = 4,
                                   selectInput("drug", "drug", choices = unique(waste1$drug),
                                               selected = "Fentanyl (Norfentanyl)")),
                            column(width = 4,
                                   radioButtons("comparey", "Compare Year?", choices = c("Yes", "No"),
                                                selected = "No"))),
                          fluidRow(
                            mainPanel(width = 12,
                                      plotOutput("wasteplot", click = "plot_click")
                            )),
                          fluidRow(
                            tableOutput("extra_info")
                          ),
                          fluidRow(
                            drug_info()
                          )
                 ),
                 #####------------------------Tab Panel 2---------------------------------####
                 tabPanel("Comparison by City",
                          fluidRow(
                            column(width = 1,
                                   offset = 0,
                                   checkboxGroupInput("periods", "year", choices = c(2022,2023),
                                                      selected = c("2022","2023"))),
                            column(width = 2,
                                   offset = 0,
                                   checkboxGroupInput("site_f", "Site", 
                                                      choices = site1,
                                                      selected = c("Edmonton, AB", 
                                                                   "Vancouver, BC" ))),
                            column(width = 2,
                                   offset = 0,
                                   checkboxGroupInput("site_s", "",
                                                      choices = site2)),
                            column(width = 3,
                                   offset = 0,
                                   selectInput("drug2", "drug", choices = unique(waste1$drug),
                                               selected = "Fentanyl (Norfentanyl)"))
                          ),
                          fluidRow(
                            mainPanel(width = 12,
                                      plotOutput("crossplot", click = "plot_click2")
                            )
                          ),
                          fluidRow(
                            tableOutput("info2")
                          ),
                 )
)

####-----------------------------Server------------------------------------####

server <- function(input, output, session) {
  
  ####----------------------------Graph 1------------------------------------####
  #####------------------Reactive Variables-----------------------------####
  drugs <- reactive(input$drug)
  thesite <- reactive(input$site)
  # Filtering data here to allow for a comparison by year
  the_waste <- reactive({
    t = waste1 |> 
      filter(site == thesite(), drug == drugs())
    if(input$comparey == "No"){
      return(t)
    } else{
      t = t |>
        mutate(tyear = as.character(year(tdate)),
               tmonths = clock::date_month_factor(tdate)) |> 
        mutate(tyear = factor(tyear))
      return(t)
    }
    
  })
  #####----------Graph output-------------------------------------------####
  output$wasteplot <- renderPlot({
    
    max_y <- minmax_ct |> 
      filter(drug == drugs() & site == input$site) |> 
      summarise(max = max(max)) |>
      pull(max)
    # Since the grouping is a bit different, pass the initial aesthetics
    # to p
    if(input$comparey == "No"){
      p <- ggplot(the_waste(), 
                  aes(x = tdate, y = Load)) +
        geom_line(group = 1, colour = "black") +
        geom_errorbar(group = 1, colour = "black",
                      aes(ymin = low_conf, ymax = high_conf),
                      width = 20) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y",
                     expand = c(0,0))
    } else{
      p<- ggplot(the_waste(), 
                 aes(x = tmonths, y = Load, group = tyear, 
                     colour = tyear)) +
        geom_line() +
        geom_errorbar(aes(ymin = low_conf, ymax = high_conf),
                      width = 0.4) +
        scale_colour_manual(values = c("#332288","#88CCEE"), name = "Year")
    }
    p +
      scale_y_continuous(limits = c(0, max_y), 
                         breaks = integer_breaks()) +
      labs(title = paste ("Waste Water Load in", thesite(), " by ", input$drug),
           x = "Date",
           y = y_labs,
           caption = "Note: y-axis change is relative to substance & city chosen
           E.g., y-axis for fentanyl in Toronto will be different than y for Vancouver") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            plot.caption = element_text(size = 10),
            legend.title=element_text(size=13),
            legend.text = element_text(size = 10)) +
      geom_point(aes(shape = imputation_rate1), size = 3,
                 colour = "black") +
      scale_shape_manual(values = myshapes, name = "Imputation Rate",
                         guide = guide_legend(reverse = T))
    
  })
  #####------------------Table Output p1------------------------------------####
  output$extra_info <- renderTable({
    if(is.null(input$plot_click)){
      tbdf <- data.frame(Date = "Click a point for more info",
                         Load = "",
                         low_conf = "",
                         high_conf = "",
                         imputation_rate = "")
    } else{
      tbdf <- nearPoints(the_waste(), input$plot_click)
      tbdf <- tbdf |>
        select(tdate, Load, low_conf, high_conf, imputation_rate) |> 
        mutate(tdate = paste(month(tdate, label = T), year(tdate)))
    }
    colnames(tbdf) <- c("Date", "Load Per Capita", "Low 95% Confidence Interval",
                        "High 95% confidence interval", "Imputation Rate")
    tbdf
  })
  ####----------------------------Graph 2------------------------------------####
  doc <- reactive(input$drug2)
  places <- reactive({
    c(input$site_f, input$site_s)
  })
  years <- reactive(input$periods)
  
  g2df <- reactive({
    waste1 |> 
      filter(site %in% places() & drug == doc() &
               year(tdate) %in% years())
  })
  
  output$crossplot <- renderPlot({
    #Checks to make sure we have some values
    validate(
      need(years(), "Must Include at least one year"),
      need(places(), "Must include at least one location")
    )
    # Making the names for the title
    places2 <- paste0(places(), collapse = " & ")
    places2 <- gsub(paste0(provinces, collapse = "|"), "", places2)
    max_y <- minmax_ct |> 
      filter(drug == doc() & site %in% places()) |> 
      summarise(max = max(max)) |>
      pull(max)
    
    #Still trying to figure out dynamic setting for width of the error bars
    errorwidth <- ifelse(length(years()) == 2, 4, 2)
    p<- g2df() %>%
      ggplot(., 
             aes(x = tdate, y = Load,
                 group = site, colour = as.factor(site)) ) +
      geom_line() +
      geom_errorbar(
        aes(ymin = low_conf, ymax = high_conf)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
      scale_colour_manual(values = mycolours, name = "Cities") +
      scale_y_continuous(limits = c(0, max_y), 
                         breaks = integer_breaks()) +
      labs(title = paste("Waste Water Load for", places2, "for", doc()),
           x = "Date",
           y = y_labs,
           caption = "Note: y-axis change is relative to substance & cities chosen") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            plot.caption = element_text(size = 10),
            legend.title=element_text(size=12),
            legend.text = element_text(size = 10))
    p + geom_point(size = 3, aes(shape = imputation_rate1)) +
      scale_shape_manual(values = myshapes, name = "Imputation Rate",
                         guide = guide_legend(reverse = T))
  })
  #####------------------Table Output p2------------------------------####

  output$info2 <- renderTable({
    if(is.null(input$plot_click2)){
      tbdf <- data.frame(Site = "Click a point for more info",
                         Date = "",
                         Load = "",
                         low_conf = "",
                         high_conf = "",
                         imputation_rate = "")
    } else{
      df <- nearPoints(g2df(), input$plot_click2)
      tbdf <- waste1 |> 
        filter(site %in% places() & drug == doc() &
                 ref_date %in% df$ref_date) |> 
        select(site, tdate, Load, low_conf, high_conf, imputation_rate) |> 
        mutate(tdate = paste(month(tdate, label = T), year(tdate)))
    }
    colnames(tbdf) <- c("Site", "Date", "Load Per Capita", "Low 95% Confidence Interval",
                        "High 95% confidence interval", "Imputation Rate")
    print(tbdf)
    tbdf
  })
  
  
  
  
}
####----------------------------Run App------------------------------------####
shinyApp(ui = ui, server = server)
