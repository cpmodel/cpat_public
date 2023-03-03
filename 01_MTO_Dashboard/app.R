

# Installing needed packages, if they're not already installed
check.packages <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
        install.packages(new.pkg,
            dependencies = TRUE,
            repos = "https://cran.rstudio.com"
        )
    }
    sapply(pkg, require, character.only = TRUE)
}

check.packages(c("shiny", "shinyjs", "shinydashboard", "tidyverse", "ggplot2", 
                 "ggpubr", "readxl", "leaflet", "leafpop", "sf", "countrycode", 
                 "officer"))



# Get the file path of the current R file
filePath <- rstudioapi::getSourceEditorContext()$path

# Set the working directory to the directory containing the R file
setwd(dirname(filePath))

source("files/plot_functions.R")
source("files/report_download.R")

# Reading in geojson file for the map
sf <- sf::st_read("files/ne_110m_admin_0_countries.geojson")

# Reading in the MT Outputs data
load("files/mt_dashboard.rda")


plot_class <- c("Time Series", "Single Year")

plots <- c(
    "Welfare Benefits",
    "Change in GHG Emissions",
    "Revenue Gains",
    "Price Increase",
    "Cost Burden Distribution",
    "Healthcare Savings",
    "Averted Deaths (Net)",
    "Averted Deaths % baseline",
    "Averted Deaths (Age Group)",
    "Road Fatalities"
)



ui <- shinyUI(dashboardPage(
  
  skin = "green",
  title = "MT Output",
  
  dashboardHeader(
    title = span(img(src = "CPAT.svg", height = 35), "MT Outputs"),
    titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    
    conditionalPanel(
      condition = "input.tab != 'tab_map' & input.tab != 'tab_download'  & input.tab != 'tab_scenario' & input.tab != 'tab_welcome'",
      selectInput(
        inputId = "countries",
        label = "Select countries:",
        choices = unique(df$Country),
        multiple = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.tab != 'tab_download'  & input.tab != 'tab_scenario' & input.tab != 'tab_welcome'",
      selectInput(
      inputId = "plots",
      label = "Select Indicator:",
      choices = plots,
      selected = "Change in GHG Emissions"
    )),
    conditionalPanel(
      condition = "input.tab != 'tab_download'  & input.tab != 'tab_scenario' & input.tab != 'tab_welcome'",
      selectInput(
        inputId = "plot_class",
        label = "Select Plot Type:",
        choices = plot_class,
        selected = 'Single Year'
      )
    )
    ,
    conditionalPanel(
      condition = "input.plot_class == 'Single Year' & input.plots != 'Cost Burden Distribution' & input.tab != 'tab_download'  & input.tab != 'tab_scenario' & input.tab != 'tab_welcome'",
      numericInput("single_year",
                   label = "Year",
                   value = 2023
      )
    ),
    conditionalPanel(
      condition = "input.plot_class != 'Single Year' & input.tab != 'tab_download'  & input.tab != 'tab_scenario' & input.tab != 'tab_welcome'",
      numericInput("start_year",
                   label = "Start Year",
                   value = 2020,
                   min=2019,
                   max=2033
      ),
      numericInput("end_year",
                   label = "End Year",
                   value = 2031,
                   min=2019,
                   max=2033
      )
    ),
    h5(paste(" ", df_name[1, 1], df_name[1, 2]), sep = " "),
    h5(paste(" ", df_name[2, 1], df_name[2, 2]), sep = " ")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(
        HTML('.content-wrapper {
              overflow: scroll;
              max-height: 90vh;
           }'
        )
      )
    ),
    tabsetPanel(
      id = "tab",
      tabPanel(
        "Welcome",
        value = "tab_welcome",
        fluidRow(
          column(
            width = 12,
            h1("CPAT - Multiscenario Tool Outputs Dashboard"),
            p("This dashboard displays the outputs and projections of the Climate Policy Assessment Tool."),
            p("The following indicators are available for display in the dashboard:"),
            tags$ul(
              tags$li("Welfare Benefits - The welfare benefits and costs as % of GDP under different carbon pricing scenarios."),
              tags$li("Change in GHG Emissions - The change in greenhouse gas emissions relative to baseline emissions."),
              tags$li("Revenue Gains - The increase in revenue as a % of GDP due to carbon pricing."),
              tags$li("Price Increase - The change in prices of different energy sources under the carbon pricing scenarios."),
              tags$li("Cost Burden Distribution - The distribution of carbon pricing costs across different income deciles."),
              tags$li("Healthcare Savings - The healthcare savings due to air pollution reduction."),
              tags$li("Averted Deaths (Net) - The number of premature deaths averted due to air pollution reduction."),
              tags$li("Averted Deaths % baseline - The percentage change in premature deaths averted compared to the baseline."),
              tags$li("Averted Deaths (Age Group) - The distribution of premature deaths averted across different age groups."),
              tags$li("Road Fatalities - The changes in road fatalities comparing to baseline.")
            ),
            p("The dashboard consists of five main tabs:"),
            tags$ol(
              tags$li("Welcome - Current tab."),
              tags$li("Scenario Details - The parameters of different scenarios available. Presently only two scenarios are listed."),
              tags$li("World Dashboard - Interactive world map to generate indicators for individual countries. Please note you will have to click on the country twice to generate the plot"),
              tags$li("Country Comparisons - Generate inter-country comparisons of the indicators."),
              tags$li("Download Reports - Tool to generate default and custom indicator reports as PDF/DOCX files.")
            )
          )
        )
      )
      ,
      tabPanel(
        "Scenario Details",
        value = "tab_scenario",
        mainPanel(
          dataTableOutput("scenario_settings")
        )
      ),
      tabPanel(
        "World Dashboard",
        value = "tab_map",
        mainPanel(
          fillPage(
            leafletOutput("Map", width = "100vw", height = "100vh"))
        )
      ),
      tabPanel(
        "Country Comparisons",
        value = "tab_multi",
        tabsetPanel(
          id = "multi_country",
          tabPanel(
            "Plot",
            value = "multi_plot",
            mainPanel(
              plotOutput(outputId = "plot", height = "800px", width = "80%")
            )
          ),
          tabPanel(
            "Data",
            value = "multi_data",
            mainPanel(
              tags$div(
                class = "pull-right",
                downloadButton("downloadPlotData", "Download Plot Data")
              ),
              dataTableOutput("plot_data")
            )
          )
        )
      ),
      tabPanel(
        "Download Reports",
        value = "tab_download",
        tabsetPanel(
          tabPanel(
            "Download Country Report",
            fluidRow(
              column(width = 6,
                     selectInput(
                       inputId = "def_report_countries",
                       label = "Select countries:",
                       choices = unique(df$Country),
                       multiple = TRUE
                     )
              )
            ),
            
            fluidRow(
              column(width = 3,
                     downloadButton(
                       outputId = "download_report",
                       label = "Download PDF"
                     )
              ),
              column(width = 3,
                     downloadButton(
                       outputId = "download_docx",
                       label = "Download DOCX"
                     )
              )
            )

          ),
          tabPanel(
            "Download Custom Report",
            fluidRow(
              column(width = 4, 
                     selectInput(
                       inputId = "download_plot",
                       label = "Select Indicator:",
                       choices = plots,
                       selected = "Change in GHG Emissions"
                     )
              ),
              column(width = 4, 
                     selectInput(
                       inputId = "download_country",
                       label = "Select countries:",
                       choices = unique(df$Country)
                     )
              ),
              column(width = 4, 
                     selectInput(
                       inputId = "download_plot_class",
                       label = "Select Plot Type:",
                       choices = plot_class,
                       selected = 'Single Year'
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     conditionalPanel(
                       condition = "input.download_plot_class == 'Single Year'",
                       numericInput(
                         inputId = "download_single_year",
                         label = "Year",
                         value = 2023
                       )
                     ),
                     conditionalPanel(
                       condition = "input.download_plot_class != 'Single Year'",
                       numericInput(
                         inputId = "download_start_year",
                         label = "Start Year",
                         value = 2020,
                         min=2019,
                         max=2033
                       )
                     )
              ),
              column(width = 6,
                     conditionalPanel(
                       condition = "input.download_plot_class != 'Single Year'",
                       numericInput(
                         inputId = "download_end_year",
                         label = "End Year",
                         value = 2031,
                         min=2019,
                         max=2033
                       )
                     )
              )
            ),
            actionButton(
              inputId = "add_download_plot",
              label = "Add Plot",
              icon = icon("plus")
            ),
            dataTableOutput("user_selections"),
            downloadButton(
              outputId = "download_pdf",
              label = "Download PDF"
            ),
            downloadButton(
              outputId = "download_custom_docx",
              label = "Download DOCX"
            )
          )
        )
      )
    )
    
  )
    
  
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    output$version_details <- renderPrint({
        print(df_name, row.names = F, col.names = F)
    })

    output$scenario_settings <- renderDataTable({
        df_scenario_settings
    })

    output$plot <- renderPlot({
        if (input$plot_class == 'Single Year' & input$plots != "Cost Burden Distribution") {
            get_barplot(
                df,
                input$single_year,
                input$countries,
                input$plots
            )$p
        } else if (input$plots == "Cost Burden Distribution") {
            get_costdistnplot(
                df_cost_distn,
                input$countries
            )$p
        } else {
            get_multiyear_plot(
                df,
                seq(input$start_year, input$end_year),
                input$countries,
                input$plot_class,
                input$plots
            )$p
        }
    })
    
    output$plot_data <- renderDataTable({
      if (input$plot_class == 'Single Year' & input$plots != "Cost Burden Distribution") {
        get_barplot(
          df,
          input$single_year,
          input$countries,
          input$plots
        )$df
      } else if (input$plots == "Cost Burden Distribution") {
        get_costdistnplot(
          df_cost_distn,
          input$countries
        )$df
      } else {
        get_multiyear_plot(
          df,
          seq(input$start_year, input$end_year),
          input$countries,
          input$plot_class,
          input$plots
        )$df
      }
    })
    
    output$downloadPlotData <- downloadHandler(
      
      filename = function() {
        paste("plot_data", ".csv", sep = "")
      },
      
      content = function(file) {
        if (input$plot_class == 'Single Year' & input$plots != "Cost Burden Distribution") {
          df_plot <- get_barplot(
            df,
            input$single_year,
            input$countries,
            input$plots
          )$df
        } else if (input$plots == "Cost Burden Distribution") {
          df_plot <- get_costdistnplot(
            df_cost_distn,
            input$countries
          )$df
        } else {
          df_plot <- get_multiyear_plot(
            df,
            seq(input$start_year, input$end_year),
            input$countries,
            input$plot_class,
            input$plots
          )$df
        }
        
        write_csv(df_plot, file)
      }
    )
    
    
    
    output$Map <- renderLeaflet({
  
      leaflet(filter(sf, iso_a3 %in% unique(df$iso3))) %>%
            # Add a world map tile layer
            addProviderTiles("Stamen.TonerLite",
                options = providerTileOptions(noWrap = TRUE)
            ) %>%
            # Set the initial zoom level and center of the map
            setView(lng = 0, lat = 0, zoom = 2.5) %>%
            addPolygons(
                weight = 2,
                opacity = 1,
                color = "green",
                dashArray = "3",
                fillOpacity = 0.7,
                layerId = ~name,
                highlightOptions = highlightOptions(
                    fillColor = "blue",
                    fillOpacity = 0.7,
                    bringToFront = TRUE,
                    stroke = TRUE)
            )
    })
    
    rv <- reactiveVal(NULL)
    popup_plr <- reactiveVal(NULL)
    
    observeEvent(input$Map_shape_click, {
      
      rv(input$Map_shape_click$id)
      
      
      if(!is.null(rv())){
        if (input$plot_class == 'Single Year' & input$plots != "Cost Burden Distribution") {
          popup_plot <- get_barplot(
            df,
            input$single_year,
            as.vector(rv()),
            input$plots
          )$p
          
        } else if (input$plots == "Cost Burden Distribution") {
          popup_plot <- get_costdistnplot(
            df_cost_distn,
            as.vector(rv())
          )$p
        } else {
          popup_plot <- get_multiyear_plot(
            df,
            seq(input$start_year, input$end_year),
            as.vector(rv()),
            input$plot_class,
            input$plots
          )$p
        }
      }
      
      popup_plr(popupGraph(popup_plot, width = 600, height = 400))
      
    })
    
    
    observe({
      
      req(popup_plr())
      
      print(paste("2", as.vector(rv())))
      
      leafletProxy("Map", data = filter(sf, iso_a3 %in% unique(df$iso3))) %>%
        clearShapes() %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "green",
          dashArray = "3",
          fillOpacity = 0.7,
          layerId = ~name,
          highlightOptions = highlightOptions(
            fillColor = "blue",
            fillOpacity = 0.7,
            bringToFront = TRUE,
            stroke = TRUE),
          popup = popup_plr(),
          popupOptions = popupOptions(minWidth = 400, autoClose=FALSE)
        )
    })
    
      
       
      
      
        


    
    # Create an empty data frame to store user inputs
    user_inputs <- reactiveValues(data = data.frame(Plot_Type = character(),
                                                    Countries = character(),
                                                    Plot_Class = character(),
                                                    Single_Year = integer(),
                                                    Start_Year = integer(),
                                                    End_Year = integer(),
                                                    stringsAsFactors = FALSE))
    
    # Update the user_inputs data frame based on user selections
    observeEvent(input$add_download_plot, {
      plot_Type <- input$download_plot
      countries <- input$download_country
      plot_Class <- input$download_plot_class
      
      if (input$download_plot_class == "Single Year") {
        single_Year <- input$download_single_year
        start_Year <- NA
        end_Year <- NA
      } else {
        single_Year <- NA
        start_Year <- input$download_start_year
        end_Year <- input$download_end_year
      }
      
      user_inputs$data <- rbind(user_inputs$data, data.frame(Plot_Type = plot_Type,
                                                             Countries = countries,
                                                             Plot_Class = plot_Class,
                                                             Single_Year = single_Year,
                                                             Start_Year = start_Year,
                                                             End_Year = end_Year,
                                                             stringsAsFactors = FALSE))
    })
    
    output$user_selections <- renderDataTable({
      user_inputs$data
    })
    
    
    output$download_report <- downloadHandler(
        filename = function() {
            paste("Country_Default_Report", ".pdf", sep="")
        },
        content = function(file) {
          generate_default_report(df, df_cost_distn, input$def_report_countries)
          file.rename("Country_Report_Default.pdf", file)
        }
    )
    
    output$download_docx <- downloadHandler(
      filename = function() {
        paste("Country_Default_Report", ".docx", sep="")
      },
      content = function(file) {
        generate_default_report_docx(df, df_cost_distn, input$def_report_countries)
        file.rename("Country_Report_Default.docx", file)
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("Custom_Report", ".pdf", sep="")
      },
      content = function(file) {
        generate_custom_pdf(df, df_cost_distn, user_inputs$data)
        file.rename("Country_Report.pdf", file)
      }
    )
    
    output$download_custom_docx <- downloadHandler(
      filename = function() {
        paste("Custom_Report", ".docx", sep="")
      },
      content = function(file) {
        generate_custom_docx(df, df_cost_distn, user_inputs$data)
        file.rename("Custom_Report.docx", file)
      }
    )
    

}

# Run the application
shinyApp(ui = ui, server = server)


