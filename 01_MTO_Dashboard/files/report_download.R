source("files/plot_functions.R")

# This file has the report generation functions



generate_default_report <- function(df, df_cost_distn, countries){

  pdf("Country_Report_Default.pdf", width=10, height=12,
      pagecentre = TRUE,
      title = "Default Country Report")

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

  for (p in plots) {

    if (p == "Cost Burden Distribution") {
      print(
        get_costdistnplot(df_cost_distn, countries)$p
      )
    }

    else {

      for (class in plot_class) {

        if (class == "Single Year"){
          print(get_barplot(
            df,
            2025,
            countries,
            p
          )$p)
        }

        else{
          print(get_multiyear_plot(
            df,
            seq(2021, 2031),
            countries,
            class,
            p
          )$p)
        }

      }
    }
  }

  dev.off()
}




generate_custom_pdf <- function(df, df_cost_distn, user_inputs){
  
  pdf("Country_Report.pdf", paper="letter",
      title = "Custom Report",
      pagecentre = TRUE)
  
  for (i in 1:nrow(user_inputs)){
    
    row <- user_inputs[i,]
    
    if (row$Plot_Type == "Cost Burden Distribution") {
      print(
        get_costdistnplot(df_cost_distn, row$Countries)$p
      )
    }
    else if (row$Plot_Class == 'Single Year' & row$Plot_Type != "Cost Burden Distribution") {
      print(
        get_barplot(
          df,
          row$Single_Year,
          row$Countries,
          row$Plot_Type
        )$p
      )
    }
    else if (row$Plot_Type != "Cost Burden Distribution") {
      print(
        get_multiyear_plot(
          df,
          seq(row$Start_Year, row$End_Year),
          row$Countries,
          row$Plot_Class,
          row$Plot_Type
        )$p
      )
    }
    
  }
  
  dev.off()
  
}





# Function to generate a docx file with centered plots
generate_default_report_docx <- function(df, df_cost_distn, countries) {
  
  doc <- read_docx()
  
  plot_class <- c("Time Series 1", "Time Series 2", "Single Year")
  
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
  
  for (p in plots) {
    
    if (p == "Cost Burden Distribution") {
      doc <- body_add_gg(
        doc,
        get_costdistnplot(df_cost_distn, countries)$p
      )
    }
    
    else {
      
      for (class in plot_class) {
        
        if (class == "Single Year") {
          doc <- body_add_gg(
            doc,
            get_barplot(df, 2025, countries, p)$p
          )
        }
        
        else {
          doc <- body_add_gg(
            doc,
            get_multiyear_plot(df, seq(2020, 2031), countries, class, p)$p
          )
        }
        
      }
    }
  }
  
  
  print(doc, target = "Country_Report_Default.docx")
  
}




generate_custom_docx <- function(df, df_cost_distn, user_inputs){
  
  
  doc <- read_docx()
  
  for (i in 1:nrow(user_inputs)){
    
    row <- user_inputs[i,]
    plot <- NULL
    
    if (row$Plot_Type == "Cost Burden Distribution") {
      plot <- get_costdistnplot(df_cost_distn, row$Countries)$p
    }
    else if (row$Plot_Class == 'Single Year' & row$Plot_Type != "Cost Burden Distribution") {
      plot <- get_barplot(df, row$Single_Year, row$Countries, row$Plot_Type)$p
    }
    else if (row$Plot_Type != "Cost Burden Distribution") {
      plot <- get_multiyear_plot(df, seq(row$Start_Year, row$End_Year), row$Countries, row$Plot_Class, row$Plot_Type)$p
    }
    
    if (!is.null(plot)) {
      doc <- body_add_gg(doc, plot)
    }
  }
  
  print(doc, target = "Custom_Report.docx")
  
}
