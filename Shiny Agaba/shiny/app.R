# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("DT")
# install.packages("bslib")
# install.packages("bsicons")

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinythemes)  # For Bootswatch themes
library(ggplot2)
library(DT)
library(bslib)
library(bsicons)

# Change the "source([type the File-path for `Data_Cleaned.R` for your computer])"
source("C:/Users/-/OneDrive - University of Bolton/Documents/selenium-r-demo/Team Alpha/Portfolio Files/Folder 1 Agaba/Data_Cleaned.R", local = TRUE)


ui <- fluidPage(

  navbarPage(
    title = "Alpha Team Portfolio Dashboard",
    collapsible = TRUE,
    inverse = FALSE,
    theme = shinythemes::shinytheme("flatly"), # Optional: Adds a light-blue theme
    header = tags$style(HTML("
    body { background-color: #170e3f;
          color: white;
    }
    #PriceHisto > img {
    padding: 0px 0px 20px 0px;
    }
    
    #MileageHisto > img {
    padding: 0px 0px 20px 0px;
    }
    #CarAgeHisto > img {
    padding: 0px 0px 20px 0px;
    }
    #CarBrand > img {
    padding: 0px 0px 20px 0px;
    }
    #EngineSize > img {
    padding: 0px 0px 20px 0px;
    }
    #GearDistro > img {
    padding: 0px 0px 20px 0px;
    }
    #SPlotMvP > img {
    padding: 0px 0px 20px 0px;
    }
    #GPlotPvC > img {
    padding: 0px 0px 20px 0px;
    }
    #GPlotBvP > img {
    padding: 0px 0px 20px 0px;
    }
    .navbar-default { background-color: #113768; }
    .navbar-default .navbar-brand:hover{
      color: grey;
    }
    .navbar-default .navbar-nav > li > a:hover {
      background-color: dark-grey;
      color: white;
   
    }
    
    
     #car_table{
    background-color: dark-grey;
      color: white;
    }
    
    
    @property --num {
  syntax: '<integer>';
  initial-value: 0;
  inherits: false;
    }

    @keyframes counter{
    from{ content: 0}
    to{ content: 100 }
    }
    
    p.value-box-value{
    animate: counter 5s infinite alternate ease-in-out;
    }
  ")),
    tabPanel("Summary",
             layout_columns(
               card(
                   plotOutput("PriceHisto2"),
                   layout_columns(
                     value_box(
                       title = "Total Car Data",
                       value = lendf,
                       showcase = bsicons::bs_icon("bar-chart"),
                       theme = value_box_theme(bg = "#007bff", fg = "white")
                     ),
                     value_box(
                       title = "Total Car Brands",
                       value = length(Brand_grouped$Brand),
                       showcase = bsicons::bs_icon("bar-chart"),
                       theme = value_box_theme(bg = "#007bff", fg = "white")
                     ),
                     value_box(
                       title = "Price-Mileage Coorelation",
                       value = spearman_corrr ,
                       showcase = bsicons::bs_icon("bar-chart"),
                       theme = value_box_theme(bg = "#007bff", fg = "white")
                     )
                   )
                   
             )
             ),
             layout_columns(
               card(
                 card_header("Car Price Summary"),
                 layout_columns(
                   value_box(
                     title = "Mininum Price (£)",
                     value = priceMin,
                     showcase = bsicons::bs_icon("cash-coin"),
                     theme = value_box_theme(bg = "#3e587c", fg = "white")
                   ),
                   value_box(
                     title = "Maximum Price (£)",
                     value = priceMax,
                     showcase = bsicons::bs_icon("cash-coin"),
                     theme = value_box_theme(bg = "#007bff", fg = "white")
                   ),
                   value_box(
                     title = "Average Price (£)",
                     value = priceAve,
                     showcase = bsicons::bs_icon("cash-coin"),
                     theme = value_box_theme(bg = "#3e587c", fg = "white")
                   ),
                 ))),
             layout_columns(
               card(
                 card_header("Car Milage Summary"),
                 layout_columns(
                   value_box(
                     title = "Mininum Car Mileage",
                     value = MileageMin,
                     showcase = bsicons::bs_icon("speedometer"),
                     theme = value_box_theme(bg = "#3e587c", fg = "white")
                   ),
                   value_box(
                     title = "Maximum Car Mileage",
                     value = MileageMax,
                     showcase = bsicons::bs_icon("speedometer"),
                     theme = value_box_theme(bg = "#007bff", fg = "white")
                   ),
                   value_box(
                     title = "Average Car Mileage",
                     value = MileageAve,
                     showcase = bsicons::bs_icon("speedometer"),
                     theme = value_box_theme(bg = "#3e587c", fg = "white")
                   ),
                 )))
             ),
    tabPanel("Plots",
             layout_columns(
               card(
                 card_header("Metrics Distributions in Plots"),
                 layout_columns(
                   plotOutput("CarBrand"),
                   # plotOutput("EngineSize"),
                   plotOutput("GearDistro"),
                   plotOutput("SPlotMvP"),
                   plotOutput("GPlotPvC"),
                   plotOutput("GPlotBvP")
                 )))
             ),
    tabPanel("Histograms", 
             plotOutput("PriceHisto"),
             plotOutput("MileageHisto"),
             plotOutput("CarAgeHisto")
             ),
    tabPanel("Tables",
      selectInput("brand_summary", "Brand:", choices = c("All", unique(full_clean_data$Brand))),
      # Update Button
      actionButton("update_btn_summary", "Update Dashboard", class = "btn-primary"),
      # Display Filtered Car Details and Table
      column(
        width = 12,
        card(
          card_header("Filtered Car Details"),
          DTOutput("car_table")
        )
      )
      
    )
      
    )
  )


server <- function(input, output, session) {
  # Calling histograms function here
  output$PriceHisto <- renderPlot({
    PriceHist(dat = clean_data$Price)
  })
  output$plot_histograms <- renderPlot({
    plot_histograms(mileage = clean_data$Mileage, price = clean_data$Price)
  })
  output$PriceHisto2 <- renderPlot({
    PriceHist2(dat = clean_data$Price)
  })
  output$MileageHisto <- renderPlot({
    MileageHist(dat = clean_data$Mileage)
  })
  output$CarAgeHisto <- renderPlot({
    CarAgePlot(dat = clean_data$`Car Age`)
  })
  
  # Calling GGPlots function here
  output$CarBrand <- renderPlot({
    CarBrandPlot(dat = Brand_grouped)
  })
  output$EngineSize <- renderPlot({
    EngineSizePlot(dat = EngineSize_grouped)
  })
  output$GearDistro <- renderPlot({
    GearDist(dat = Gear_grouped)
  })
  output$SPlotMvP <- renderPlot({
    SPlotMvP(dat = clean_data)
  })
  output$GPlotPvC <- renderPlot({
    GPlotPvC(dat = clean_data)
  })
  output$GPlotBvP <- renderPlot({
    GPlotBvP(dat = clean_data)
  })
  
  # Calling Price Summary Statistics
  output$priceMin <- renderPlot({
    priceMin
  })
  output$priceMax <- renderPlot({
    priceMax
  })
  output$priceAve <- renderPlot({
    priceAve
  })
  
  # Calling Mileage Summary Statistics
  output$MileageMin <- renderPlot({
    MileageMin
  })
  output$MileageMax <- renderPlot({
    MileageMax
  })
  output$MileageAve <- renderPlot({
    MileageAve
  }) 
  
  # Reactive Data Filtering for Summary Statistics Page
  filtered_data_summary <- eventReactive(input$update_btn_summary, {
    data <- full_clean_data
    
    # Filter by Brand
    if (input$brand_summary != "All") {
      data <- data[data$Brand == input$brand_summary, ]
    }
    
    # # Filter by Price Range
    # data <- data[data$Price >= input$price_range_summary[1] & data$Price <= input$price_range_summary[2], ]
    # 
    # # Filter by Mileage Range
    # data <- data[data$Mileage >= input$mileage_range_summary[1] & data$Mileage <= input$mileage_range_summary[2], ]
    # 
    return(data)
  })
  
  # Key Metrics
  output$total_cars <- renderText({
    nrow(filtered_data_summary())
  })
  
  output$total_brands <- renderText({
    length(unique(filtered_data_summary()$Brand))
  })
  
  output$min_price <- renderText({
    label_dollar(prefix = "£")(min(filtered_data_summary()$Price, na.rm = TRUE))
  })
  
  output$max_price <- renderText({
    label_dollar(prefix = "£")(max(filtered_data_summary()$Price, na.rm = TRUE))
  })
  
  # Responsive Table
  output$car_table <- renderDT({
    datatable(
      filtered_data_summary(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  # Reactive Data Filtering for Visualization Page
  filtered_data_visuals <- eventReactive(input$update_btn_visuals, {
    data <- full_clean_data
    
    # Filter by Brand
    if (input$brand_viz != "All") {
      data <- data[data$Brand == input$brand_viz, ]
    }
    
    # Filter by Price Range
    data <- data[data$Price >= input$price_range_viz[1] & data$Price <= input$price_range_viz[2], ]
    
    # Filter by Mileage Range
    data <- data[data$Mileage >= input$mileage_range_viz[1] & data$Mileage <= input$mileage_range_viz[2], ]
    
    return(data)
  })
  
  
}

shinyApp(ui, server)
