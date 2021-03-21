library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(scales)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)
library(leaflet)
library(leaflet.extras)

# Load and clean data ----------------------------------------------
# assign NAs to missing values
df = read.csv("coraopolis_business_inventory.csv", header = T, na.strings = c("", "NA"))

# categorical variables
cat_cols = c("address", "street", "business_ownership",
             "business_type", "naics_description",
             "sic_description", "rent_expenses",
             "sq_footage")

# continuous variables
cont_cols = c("sales")

df[cat_cols] = lapply(df[cat_cols], factor) # make categorical variables as factors
df[cont_cols] = sapply(df[cont_cols], as.numeric) # makes sales variable as numeric

# change the levels of rent_expenses variable from descending to ascending
df$rent_expenses = factor(df$rent_expenses,
                          levels = c("Less than 10k", "10k-25k", "25k-50k", 
                                     "50k-100k", "100k-250k", "500k+"))

# change the levels of sq_footage variable from descending to ascending
df$sq_footage = factor(df$sq_footage,
                       levels = c("1-1499", "1500-2499", "2500-4999", 
                                  "5000-9999", "10000-19999", "20000-39999",
                                  "40000-99999", "100000+"))

total_sales = sum(df$sales) # total sales of all businesses

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Small Businesses in Coraopolis",
                          titleWidth = 300)

# Dashboard Sidebar -------------------

#Exploratory Data Analysis

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Three Menu Items ----------------------------------------------
    menuItem("Coraopolis Map", icon = icon("map-marked-alt"), tabName = "coraopolisMap"),
    menuItem("Data Table", icon = icon("table"), tabName = "table"),
    menuItem("Business Landscape", icon = icon("industry"), tabName = "visualizations"),
    
    hr(), # horizontal line for visual separation
    
    # Data to Show (Build your Own Table) ------------------------------------
    # users will have the flexibility to create their own table before they download it
    conditionalPanel("input.tabs === 'table'", # conditional panel for table tab
                     
                     downloadButton(outputId = "write_csv", # download button
                                    label = "Download data"),
                     
                     sliderInput(inputId = "table_sales", # slider input 
                                 label = "Annual Sales (in USD)",
                                 min = min(df$sales), max = max(df$sales),
                                 value = c(0, 50000000),
                                 step = 100000),
                     
                     checkboxGroupInput(inputId = "table_street", # group input to select the street
                                        label = "Street",
                                        choices = sort(unique(df$street)),
                                        selected = c("5th Avenue")),
                     
                     hr(),
                     
                     checkboxInput(inputId = "table_selectall", # radio button to select/deselect all business types
                                   label = "Select/Deselect all business types"),
                     
                     checkboxGroupInput(inputId = "table_business_type", # group input to select the business type
                                        label = "Business Type",
                                        choices = sort(unique(df$business_type)),
                                        selected = c("Professional Service & Consulting",
                                                     "Restaurants & Taverns",
                                                     "Breweries & Distilleries"))
                     ),
    
    # Data Visualization ------------------------------------
    conditionalPanel("input.tabs === 'visualizations'", # conditional panel for visualizations tab
       # Inputs: select categorical variables to plot ----------------------------------------------
       
       # see how revenues differ by street and business type
       selectInput(inputId = "visual_revenue",
                   label = "Revenues by:",
                   choices = c("street", "business_type"),
                   multiple = FALSE,
                   selectize = TRUE,
                   selected = "street"),
       hr(),
       
       # these features are for the visualization between the relationship of rent, square foot, and street
       selectInput(inputId = "visual_rent_x", # x-axis
                   label = "Select your X-axis:",
                   choices = c("street", "sq_footage", "rent_expenses"),
                   multiple = FALSE,
                   selectize = TRUE,
                   selected = "sq_footage"),
       
       selectInput(inputId = "visual_rent_facet", # variable for facet wrap
                   label = "Select your facet:",
                   choices = c("street", "sq_footage", "rent_expenses"),
                   multiple = FALSE,
                   selectize = TRUE,
                   selected = "street")),
                    
    
    conditionalPanel("input.tabs === 'coraopolisMap'", # conditional panel for map
                     checkboxInput(inputId = "show_heatmap", # heat map for the map
                                   label = "Show Revenue Heatmap",
                                   ),
                     hr(),
                     
                     checkboxGroupInput(inputId = "map_street", # input id for map
                                        label = "Street",
                                        choices = sort(unique(df$street)), # choices to filter the map
                                        selected = c("5th Avenue")),
                     
                     hr(),
                     
                     # radio button to select all the business types
                     checkboxInput(inputId = "all_business_types",
                                label = "Select/Deselect all business types"),
                     
                     # group inputs to select the business types users would like to see
                     checkboxGroupInput(inputId = "map_business_type",
                                        label = "Business Type",
                                        choices = sort(unique(df$business_type)),
                                        selected = c("Professional Service & Consulting",
                                                     "Restaurants & Taverns",
                                                     "Breweries & Distilleries")))
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Map Page -----
  tabItem("coraopolisMap",
          fluidRow(leafletOutput("coraopolis_map", height = "100vh"))),
  
  # Data Table Page
  tabItem("table",
          fluidPage(
            box(title = "Build Your Own Table (Select Features from the Sidebar)", 
                dataTableOutput("table"), width = 12))),
  
  tabItem("visualizations",
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("num_business"),
            valueBoxOutput("highest_industry", width = 5)
          ),
          
          # Bar plot to show the relationship between rent, square footage, and streets ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot", # build two tabs in this exploration tab
                   width = 12,
                   tabPanel("Revenue", plotlyOutput("visual_rev_barplot")),
                   tabPanel("Rent & Square Footage by Streets", plotlyOutput("visual_rent_barplot")))
          ))
  
))

# UI
ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Define server function required to create plots and value boxes -----
server <- function(input, output, session) {
  
  # Coraopolis Map ---
  # reaction function to subset the dataframe by inputs users put in
  coraopolis_map = reactive({ 
    subset(df, subset = street %in% input$map_street &
             business_type %in% input$map_business_type)
  })
  
  # select all the business types if user clicks to select all the business types
  observe({
    if(input$all_business_types) {
      updateCheckboxGroupInput(session,
                               inputId = "map_business_type",
                               label = "Business Type",
                               choices = sort(unique(df$business_type)),
                               selected = unique(df$business_type))
    }
    else { # reset the selection field if user deselect
      updateCheckboxGroupInput(session,
                               inputId = "map_business_type",
                               label = "Business Type",
                               choices = sort(unique(df$business_type)))
    }

  })
  
  # draw the leaflet map
  output$coraopolis_map = renderLeaflet({
    
    leaflet() %>% 
      setView(lng = -80.164298, lat = 40.517182, zoom = 15.3) %>% 
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
               attribution = "Google", group = "Google") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addLayersControl(baseGroups = c("Google", "Toner")) %>% 
      addCircles(data = coraopolis_map(),
                 lat = ~ latitude,
                 lng = ~ longitude,
                 weight = 1,
                 label = ~as.character(paste("Company: ", company_name, "|",
                                             "Business Type: ", business_type, "|",
                                             "Sales: ", sales, "USD")),
                 # group = ~ business_type,
                 fillOpacity = 0.4)
  })
  
  # leaflet proxy to draw the heat map
  observe({
    proxy = leafletProxy("coraopolis_map", data = coraopolis_map())
    proxy %>% clearMarkers()
    if (input$show_heatmap) { # activate the heatmap if user clicks to show the heatmap
      proxy %>% addHeatmap(data = coraopolis_map(),
                           lng = ~longitude,
                           lat = ~latitude,
                           intensity = ~sales,
                           blur = 15,
                           radius = 12)
    }
    else{
      proxy %>% clearHeatmap() # clear the heatmap once user deselects the heatmap
    }
  })
  
  # Data Table ---
  # option to select all business types
  observe({
    if(input$table_selectall) {
      updateCheckboxGroupInput(session,
                               inputId = "table_business_type",
                               label = "Business Type",
                               choices = sort(unique(df$business_type)),
                               selected = unique(df$business_type))
    }
    else {
      updateCheckboxGroupInput(session,
                               inputId = "table_business_type",
                               label = "Business Type",
                               choices = sort(unique(df$business_type)))
    }
    
  })
  
  # data table to be downloaded as CSV
  coraopolis_datatable = reactive({
    subset(df, subset = street %in% input$table_street &
             business_type %in% input$table_business_type &
             sales >= min(input$table_sales) &
             sales <= max(input$table_sales))
  })
  
  # Data Visualizations ---
  
  # reactive function to subset the data frame by user-defined inputs
  coraopolis_subset = reactive({
    req(input$visual_revenue)
    select(df, c("sales", input$visual_revenue))
  })
  
  coraopolis_rent = reactive({
    req(input$visual_rent_x != input$visual_rent_facet)
  })
  
  # OUTPUTS ---
  
  # Data Table ---
  # show the data table
  output$table = renderDataTable({
    coraopolis_datatable()
  })
  
  # write sampled data as csv file -------
  output$write_csv <- downloadHandler(
    filename <- function(){paste0("coraopolis_business_inventory", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv", sep = "")}, # asks user where to save the csv file
    content = function(filname){
      write.csv(coraopolis_datatable(), file = filname, row.names = FALSE)
    }
  )
  
  # Data Visualizations ---
  
  # draw value box to show the number of business in Coraopolis and the industry with the highest sales
  output$num_business <- renderValueBox({
    valueBox("# of Business in Coraopolis", value = dim(df)[1],
             icon = icon("store"), color = "red")
  })
  
  output$highest_industry <- renderValueBox({
    valueBox("Industry with the Highest Sales", value = "Automotive",
             icon = icon("car"), color = "blue")
  })
  
  output$visual_rev_barplot = renderPlotly({
    
    dat = coraopolis_subset() %>% group_by(get(input$visual_revenue)) %>% # group by the user's input
                                                                        # and get the corresponding sales and total sales
      summarize(count = n(),
                sales = sum(sales)) %>%
      mutate(sales_pct = sales / total_sales) %>% 
      arrange(desc(sales_pct))
    
    colnames(dat) = c(input$visual_revenue, "count", "sales", "sales_pct")

    # bar plot to show the industries' sales in descending order
    ggplot(data = dat, mapping = aes(x = reorder(eval(as.name(input$visual_revenue)), -sales_pct),
                                     y = sales_pct)) + geom_bar(stat = "identity", fill = "#007B7F") +
      labs(x = ifelse(input$visual_revenue == "business_type", "Business Type", "Street"),
           y = "Sales Percentage", title = "Breakdown of Revenue") + scale_color_hue(l = 40, c = 85) +
      theme_bw() +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.text.x = element_text(angle = ifelse(input$visual_revenue == "business_type",
                                                      90, 0))) +
      scale_y_continuous(labels = percent)
  })
  
  # bar plot with facet wrap, both defined by the users
  output$visual_rent_barplot = renderPlotly({
    dat = na.omit(df)
    
    ggplot(data = dat, mapping = aes_string(x = input$visual_rent_x)) + geom_bar(fill = "#457b9d") + 
      facet_wrap(input$visual_rent_facet, nrow = 3) +
      theme_bw() + theme(axis.text.x = element_text(angle = 60)) +
      labs(x = "", y = "# of Small Businesses", title = paste(input$visual_rent_x)) +
      theme(plot.title = element_text(size = 15, hjust = 0.5))
    
  
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
