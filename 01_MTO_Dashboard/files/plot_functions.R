

# This file contains the functions to create the different plots. There are three functions in total:
# 1. For barplot plotting values for a single year - get_barplot()
# 2. For time series plots - get_multiyear_plot()
# 3. For cost distribution plot from the distribution module - get_costdistn_plot()




# 1. Bar Plots for different plot types

get_barplot <- function (df, year, countries, plot_name) {
  
  df <- filter(df, (Country %in% countries) &  (Year == year))
  
  if (nrow(df) == 0) {
    p <- ggplot() +
      labs(x = "No Plot Available")
    
    return(list(p = p, df = df))
  }
  
  # 2.1 Welfare Benefits Bar Plot
  else if (plot_name == "Welfare Benefits") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("wel", CPATCode) & grepl("pct", CPATCode) & Variable == "wel") %>%
      mutate(`Total Net Benefit (% GDP)` = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", Value * 100, 0)) %>%
      group_by(Country, Scenario, Year) %>%
      mutate(`Total Net Benefit (% GDP)` = sum(`Total Net Benefit (% GDP)`)) %>%
      ungroup() %>%
      mutate(Value = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", NA, Value))
    
    
    p <- ggplot(df) +
      aes(x = Country, y = Value*100, fill = CPATIndicator) +
      geom_col() +
      geom_point(aes(x = Country, y = `Total Net Benefit (% GDP)`), size = 3, color = "#E24346") +
      scale_fill_manual(
        values = c(
          `Air pollution co-benefits (% GDP)` = "#0C0CEA",
          `Climate benefits (real US$ 2021 bn)` = "#1CA851",
          `Efficiency costs (% GDP)` = "#0F0F0F",
          `Transport co-benefits (% GDP)` = "#BEC416",
          `Total Net Benefit (% GDP)` = "#E24346"
        )
      ) +
      labs(x = "Country", y = "% GDP", fill = "", title = "Welfare Costs/Benefits") +
      theme_pubr(base_size = 12) +
      theme(legend.position = "bottom") +
      facet_wrap(vars(Scenario)) +
      guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    df <- df %>%
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)
    
    return(list(p = p, df = df))
  }
  
  # 2.2 Change in GHG Emissions Bar Plot
  else if (plot_name == "Change in GHG Emissions") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("mit.ghg.tot.inc.red.pct", CPATCode))
    
    
    p <- ggplot(df) +
      aes(x = Country, y = Value*100, fill = Scenario) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#0139A1", "#167F00")) +
      labs(y = "% relative to baseline", x = "", title = "Emissions Reduction", subtitle = "Relative to baseline") +
      theme_pubr(base_size = 12) +
      coord_flip() +
      theme(legend.position = "right")
    
    df <- df %>%
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)
    
    return(list(p = p, df = df))
  }
  
  # 2.3 Revenue Gains Bar Plot
  else if (plot_name == "Revenue Gains") {
    
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("mit.rev.new.pct.", CPATCode)) %>%
      mutate(var_p = ifelse(grepl("mit.rev.new.pct.1", CPATCode), "base", "policy")) %>%
      select(Country, Scenario, var_p, Year, Value) %>%
      pivot_wider(names_from = var_p, values_from = Value) %>%
      mutate(rev_gain = (policy - base) * 100)
    
    
    p <- ggplot(df) +
      aes(x = Country, y = rev_gain, fill = Scenario) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#0139A1", "#167F00")) +
      labs(y = "% GDP", x = "", title = "Net Annual Revenue Gain (as % GDP)") +
      theme_pubr(base_size = 12) +
      coord_flip() +
      theme(legend.position = "right")
    
    df <- df %>%
      select(Country, Scenario, Year, rev_gain)
    
    return(list(p = p, df = df))
  }
  
  # 2.4 Price Increases Bar Plot
  else if(plot_name == "Price Increase") {
    
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("mit.rp.all", CPATCode)) %>%
      filter(FuelType %in% c("coa", "die", "ecy", "gso", "nga")) %>%
      filter(CPATIndicator %in% c(
        "Electricity (% change)", "Coal (% change)", "Natural gas (% change)",
        "Gasoline (% change)", "Diesel (% change)"
      )) %>%
      separate(CPATIndicator, c("Fuel"), sep = " ") %>%
      mutate(Fuel = ifelse(FuelType == "nga", "Natural Gas", Fuel))
    
    
    p <- ggplot(df) +
      aes(x = Fuel, fill = Fuel, weight = Value * 100) +
      geom_bar() +
      geom_text(aes(label = paste(Value * 100, "%"), y = Value * 100 + 30), size = 2.5, fontface = "bold") +
      scale_fill_manual(
        values = c(
          Coal = "#000004",
          Diesel = "#501379",
          Electricity = "#B63778",
          Gasoline = "#FA8764",
          `Natural Gas` = "#CECF82"
        )
      ) +
      labs(
        x = " ",
        y = "% Change in Prices",
        title = "Impact on Fossil Fuel Prices",
        fill = "Energy Source"
      ) +
      theme_pubr(base_size = 12) +
      coord_flip() +
      theme(legend.position = "bottom") +
      facet_grid(vars(Country), vars(Scenario))
    
    df <- df %>%
      select(Country, Scenario, Year, Fuel, Value)
    
    return(list(p = p, df = df))
    
  }
  
  # 2.5 Healthcare Savings Bar Plot
  else if (plot_name == "Healthcare Savings") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("air.sav.the", CPATCode)) %>%
      mutate(CPATIndicator = ifelse(grepl("air.sav.the.gov", CPATCode), "Government",
                                    ifelse(grepl("air.sav.the.ppr", CPATCode), "Prepaid Private",
                                           ifelse(grepl("air.sav.the.opo", CPATCode), "Out-of-pocket",
                                                  ifelse(grepl("air.sav.the.das", CPATCode), "Development Assistance", NA)))))
    
    
    p <- ggplot(df) +
      aes(x = Country, y = Value, fill = CPATIndicator) +
      geom_col(position = "stack") +
      scale_fill_brewer(
        palette = "Spectral",
        direction = 1
      ) +
      labs(x = "Country", y = "Real US $million", title = "Health Expenditure Savings", fill = "Source of Savings") +
      theme_pubr(base_size = 12) +
      theme(legend.position = "bottom") +
      facet_wrap(vars(Scenario)) +
      guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))

  }
  
  # 2.6a Net Averted Deaths Bar Plot
  else if (plot_name == "Averted Deaths % baseline") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("air.cad.net|air.mort", CPATCode)) %>%
      mutate(var_plot = ifelse(Variable == "mort", "Baseline", Scenario)) %>%
      select(Year, Country, Scenario, Variable, var_plot, Value) %>%
      group_by(Country, Year, Scenario) %>%
      mutate(deaths_pcbaseline = 1 / ((sum(Value) / Value) - 1)) %>%
      ungroup() %>%
      filter(var_plot != "Baseline")
    
    
    p <-  ggplot(df) +
      aes(x = Country, y = deaths_pcbaseline*100, fill = Scenario) +
      geom_col(position="dodge") +
      scale_fill_brewer(
        palette = "Dark2",
        direction = 1
      ) +
      labs(
        y = "% Deaths Averted", title = "Air Pollution deaths averted",
        subtitle = "as % of baseline air pollution deaths", fill = "Scenario"
      ) +
      theme_pubr(base_size = 12) +
      theme(legend.position = "bottom")
    
    df <- df %>%       
      select(Country, Scenario, Year, Variable, var_plot, Value, deaths_pcbaseline)     
    return(list(p = p, df = df))
  }
  
  # 2.7 Averted Deaths by Age Group Bar Plot
  else if (plot_name == "Averted Deaths (Age Group)") {
    
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("air.ada", CPATCode)) %>%
      mutate(CPATIndicator = ifelse(grepl("air.ada.65", CPATCode), "65+ Years",
                                    ifelse(grepl("air.ada.2464", CPATCode), "24-64 Years",
                                           ifelse(grepl("air.ada.u24", CPATCode), "Below 24", NA))))
    
    
    p <-  ggplot(df) +
      aes(x = Country, y = Value, fill = CPATIndicator) +
      geom_col(position = "stack") +
      scale_fill_brewer(
        palette = "Dark2",
        direction = 1
      ) +
      labs(x = " ", y = "Number of Deaths averted", title = "Air Pollution Deaths Averted (by age group)", fill = "Age Group") +
      theme_pubr(base_size = 12) +
      theme(legend.position = "left") +
      facet_wrap(vars(Scenario)) +
      guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))
    
  }
  
  # 2.8 Road Fatalities Bar Plot
  else if (plot_name == "Road Fatalities") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year == year) %>%
      filter(grepl("tran.deaths", CPATCode)) %>%
      mutate(var_plot = ifelse(CPATIndicator == "Transport accidents, Baseline", "Baseline", Scenario)) %>%
      group_by(Country, Year, Scenario) %>%
      mutate(deaths_pcbaseline = 1 / ((sum(Value) / Value) - 1)) %>%
      ungroup() %>%
      filter(Variable != "vmt")
    
    p <- ggplot(df) +
      aes(x = Country, y = (1 - deaths_pcbaseline)*100, fill = Scenario) +
      geom_col(position = "dodge") +
      scale_fill_brewer(
        palette = "Dark2",
        direction = 1
      ) +
      labs(x = " ", y = "% deaths averted", title = "Road Fatalities averted as % of baseline", fill = "Scenario") +
      theme_pubr(base_size = 12) +
      theme(legend.position = "left")
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))
  }
  
}


# 3. Cost Distribution Plot

get_costdistnplot <- function (df_cost_distn, countries) {
  
  df <- df_cost_distn %>%
    filter(Country %in% countries) %>%
    filter(grepl("distn.", MTCode) & grepl(".tot.", MTCode)) %>%
    filter(!(Variable %in% c("pop_tot_adj", "tot_cons_na"))) %>%
    filter(!(grepl("population", CPATIndicator))) %>%
    filter(!(grepl("total effect", CPATIndicator))) %>%
    pivot_longer(cols = 23:32, names_to = "Deciles", values_to = "Value") %>%
    mutate(Deciles = as.numeric(Deciles) - 2019) %>%
    mutate(Deciles = as.factor(Deciles)) %>%
    mutate(Value = as.numeric(Value)) %>%
    group_by(Country, Scenario, Deciles) %>%
    mutate(net_effect = sum(Value)) %>%
    ungroup() %>%
    separate(CPATIndicator, c("first", "var_plot"), sep = ":") %>%
    mutate(var_plot = ifelse(grepl("current", var_plot), "Current Spending",
                             ifelse(grepl(" direct", var_plot), "Direct Effect",
                                    ifelse(grepl("indirect", var_plot), "Indirect Effect",
                                           ifelse(grepl("labor", var_plot), "Labor Tax Reductions",
                                                  ifelse(grepl("public", var_plot), "Public Investment",
                                                         ifelse(grepl("targeted", var_plot), "Targeted Transfer", NA)))))))
  
  if(nrow(df) == 0){
    
    p <- ggplot() +
      labs(x = "No Plot Available")
    
    return(list(p = p, df = df))
    
  }
  
  else if(nrow(df) > 0 & (unique(is.na(df$Value)))){
    
    p <- ggplot() +
      labs(x = "No Plot Available")
    
    return(list(p = p, df = df))
    
  }
  
  else{
  
  p <- ggplot(df) +
    aes(x=Deciles, y=Value, fill = var_plot) +
    geom_col(position="stack") +
    geom_point(aes(x = Deciles, y = net_effect), size = 2) +
    scale_fill_manual(values = c(
      `Current Spending` = "#5F81F9",
      `Direct Effect` = "#D6604D",
      `Indirect Effect` = "#F18B8B",
      `Labor Tax Reductions` = "#56BDF5",
      `Public Investment` = "#4393C3",
      `Targeted Transfer` = "#053061"
    )) +
    labs(y = "% consumption", x = "Income Deciles", title = "Distribution of cost burden as % of consumption (in 2025)", fill = " ") +
    theme_pubr(base_size = 12) +
    theme(legend.position = "left") +
    coord_flip() +
    facet_grid(vars(Country), vars(Scenario), scales = "free_y")
  
  df <- df %>%       
    select(Country, Scenario, Deciles, var_plot, Value)
  
  return(list(p = p, df = df))
  
  }

}







# 1. New Time Series Plots

get_multiyear_plot <- function(df, years, countries, plot_class, plot_name) {
  
  # 1.1 - Welfare Benefit Time Series Plot
  if (plot_name == "Welfare Benefits") {
    
    # Wrangling plot data
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("wel", CPATCode) & grepl("pct", CPATCode) & Variable == "wel") %>%
      mutate(`Total Net Benefit (% GDP)` = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", Value * 100, 0)) %>%
      group_by(Country, Scenario, Year) %>%
      mutate(`Total Net Benefit (% GDP)` = sum(`Total Net Benefit (% GDP)`)) %>%
      ungroup() %>%
      mutate(Value = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", NA, Value))
    
    if (nrow(df) == 0) {
      print("in no plot")
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Area Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year) +
        geom_area(aes(y = Value * 100, fill = CPATIndicator, group = CPATIndicator), size = 1.5) +
        geom_line(aes(x = Year, y = `Total Net Benefit (% GDP)`), color = "#E24346", size = 1, linetype = "dashed") +
        scale_fill_manual(
          values = c(
            `Air pollution co-benefits (% GDP)` = "#0C0CEA",
            `Climate benefits (real US$ 2021 bn)` = "#1CA851",
            `Efficiency costs (% GDP)` = "#0F0F0F",
            `Transport co-benefits (% GDP)` = "#BEC416",
            `Total Net Benefit (% GDP)` = "#E24346"
          )
        ) +
        labs(x = "Year", y = "% GDP", fill = "", title = "Welfare Costs/Benefits") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_grid(Country ~ Scenario, scales = "free_y") +
        guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)
    
    return(list(p = p, df = df))
  }
  
  
  # 1.2 - Change in GHG Emissions Time Series 
  else if (plot_name == "Change in GHG Emissions") {
    
    # Wrangling plot data 
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("mit.ghg.tot.inc.red.pct", CPATCode))
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Line Plot
    else if (plot_class == "Time Series") {
      
      p <- ggplot(df) +
        aes(x = Year, y = Value * 100, color = Scenario) +
        geom_line(size = 1.5) +
        scale_color_manual(values = c("#0139A1", "#167F00")) +
        labs(y = "% relative to baseline", title = "Emissions Reduction", subtitle = "Relative to baseline") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(vars(Country), ncol = 2) +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
      
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>% 
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    
    return(list(p = p, df = df))
  }
  
  # 1.3 - Revenue Gain Time Series Plot
  else if (plot_name == "Revenue Gains") {
    
    # Wrangling plot data
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("mit.rev.new.pct.", CPATCode)) %>%
      mutate(var_p = ifelse(grepl("mit.rev.new.pct.1", CPATCode), "base", "policy")) %>%
      select(Country, Scenario, var_p, Year, Value) %>%
      pivot_wider(names_from = var_p, values_from = Value) %>%
      mutate(rev_gain = (policy - base) * 100)
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Line Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = rev_gain, color = Scenario) +
        geom_line(size = 1.5) +
        scale_color_manual(values = c("#0139A1", "#167F00")) +
        labs(y = "% GDP", title = "Net Annual Revenue Gain (as % GDP)") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(. ~ Country, ncol=2, scales = "free_y") +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%  
      select(Country, Scenario, Year, rev_gain)     
    return(list(p = p, df = df))
  }
  
  # 1.4 Price Increase time series plot
  else if(plot_name == "Price Increase") {
    
    # Wrangling plot data
    
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("mit.rp.all", CPATCode)) %>%
      filter(FuelType %in% c("coa", "die", "ecy", "gso", "nga")) %>%
      filter(CPATIndicator %in% c(
        "Electricity (% change)", "Coal (% change)", "Natural gas (% change)",
        "Gasoline (% change)", "Diesel (% change)"
      )) %>%
      separate(CPATIndicator, c("Fuel"), sep = " ") %>%
      mutate(Fuel = ifelse(FuelType == "nga", "Natural Gas", Fuel))
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Line Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = Value*100, color = Fuel) +
        geom_line(size = 1) +
        scale_color_manual(values = c(Coal = "#000004",
                                      Diesel = "#501379",
                                      Electricity = "#B63778",
                                      Gasoline = "#FA8764",
                                      `Natural Gas` = "#CECF82")) +
        labs(y = "% Change in Prices", title = "Impact on Fossil Fuel Prices", color = "Energy Source") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_grid(Country ~ Scenario, scales = "free_y") +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%  
      select(Country, Scenario, Year, Fuel, Value)     
    return(list(p = p, df = df))
    
  }
  
  # 1.5 Healthcare Savings Time Series
  else if (plot_name == "Healthcare Savings") {
    
    # Wrangling plot data
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("air.sav.the", CPATCode)) %>%
      mutate(CPATIndicator = ifelse(grepl("air.sav.the.gov", CPATCode), "Government",
                                    ifelse(grepl("air.sav.the.ppr", CPATCode), "Prepaid Private",
                                           ifelse(grepl("air.sav.the.opo", CPATCode), "Out-of-pocket",
                                                  ifelse(grepl("air.sav.the.das", CPATCode), "Development Assistance", NA)))))
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Area Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = Value, fill = CPATIndicator) +
        geom_area(size = 1.5) +
        scale_fill_brewer(
          palette = "Spectral",
          direction = 1
        ) +
        labs(x = " ", y = "Real US $million", title = "Health Expenditure Savings", fill = "Source of Savings") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_grid(vars(Country), vars(Scenario), scales = "free_y") +
        guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))
  }
  
  # 1.6 Net Air Pollution averted deaths time series
  else if (plot_name == "Averted Deaths (Net)") {
    
    # Wrangling plot data - Area plot (number of deaths)
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("air.cad.net|air.mort", CPATCode)) %>%
      mutate(var_plot = ifelse(Variable == "mort", "Baseline", Scenario))
    
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Area plot (number of deaths)
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = Value, fill = var_plot) +
        geom_area(size = 1.5, position = "identity", alpha = 0.5) +
        scale_fill_brewer(
          palette = "Dark2",
          direction = 1
        ) +
        labs(
          y = "Deaths", title = "Air Pollution deaths averted",
          subtitle = "Compared to baseline air pollution deaths", fill = " "
        ) +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(vars(Country), ncol=2, scales = "free_y") +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%       
      select(Country, Scenario, Year, Variable, var_plot, Value)     
    return(list(p = p, df = df))
  }
  
  # 1.6_a Air Pollution averted deaths % baseline
  else if (plot_name == "Averted Deaths % baseline") {
    
    # Wrangling plot data - Area plot (number of deaths)
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("air.cad.net|air.mort", CPATCode)) %>%
      mutate(var_plot = ifelse(Variable == "mort", "Baseline", Scenario))
    
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    
    else if (plot_class == "Time Series") {
      
      # Wrangling data - Line plot - deaths averted as percentage of baseline
      df <- df %>%
        select(Year, Country, Scenario, Variable, var_plot, Value) %>%
        group_by(Country, Year, Scenario) %>%
        mutate(deaths_pcbaseline = 1 / ((sum(Value) / Value) - 1)) %>%
        ungroup() %>%
        filter(var_plot != "Baseline")
      
      # Line plot
      p <- ggplot(df) +
        aes(x = Year, y = deaths_pcbaseline*100, color = Scenario) +
        geom_line(size = 1.5) +
        scale_color_brewer(
          palette = "Dark2",
          direction = 1
        ) +
        labs(
          y = "% Deaths Averted", title = "Air Pollution deaths averted",
          subtitle = "as % of baseline air pollution deaths", color = " "
        ) +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(vars(Country), ncol=2) +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%       
      select(Country, Scenario, Year, Variable, var_plot, Value)     
    return(list(p = p, df = df))
  }
  
  # 1.7 Air pollution deaths averted by age group - time series plots
  else if (plot_name == "Averted Deaths (Age Group)") {
    
    # Wrangling plot data
    df <-  df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("air.ada", CPATCode)) %>%
      mutate(CPATIndicator = ifelse(grepl("air.ada.65", CPATCode), "65+ Years",
                                    ifelse(grepl("air.ada.2464", CPATCode), "24-64 Years",
                                           ifelse(grepl("air.ada.u24", CPATCode), "Below 24", NA))))
    
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Area Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = Value, fill = CPATIndicator) +
        geom_area(size = 1.5) +
        scale_fill_viridis_d(
          option = "inferno",
          direction = 1
        ) +
        labs(x = "Year", y = "Number of Deaths averted", title = "Air Pollution Deaths Averted (by age group)", fill = "Age Group") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_grid(vars(Country), vars(Scenario), scales = "free_y") +
        guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }

    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))
    
  }
  
  #1.8 Road Fatalities time series plot
  else if (plot_name == "Road Fatalities") {
    
    # Wrangling plot data
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("tran.deaths", CPATCode)) %>%
      mutate(var_plot = ifelse(CPATIndicator == "Transport accidents, Baseline", "Baseline", Scenario))
    
    if (nrow(df) == 0) {
      
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    # Line Plot
    else if (plot_class == "Time Series") {
      p <- ggplot(df) +
        aes(x = Year, y = Value, colour = var_plot) +
        geom_line(size = 1) +
        scale_color_hue(direction = 1) +
        labs(x = " ", y = "Road Fatalities (count)", title = "Number of Road Fatalities", color = "Scenario") +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(vars(Country), ncol = 2, scales = "free_y") +
        scale_x_continuous(breaks = pretty(seq(min(df$Year), max(df$Year), 1), n = 3), limits = c(min(df$Year), max(df$Year)))
    }
    else {
      p <- ggplot() +
        labs(x = "No Plot Available")
    }
    
    df <- df %>%       
      select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
    return(list(p = p, df = df))
  }
  else if (plot_name == "Congestion Time") {
    df <- df %>%
      filter(Country %in% countries) %>%
      filter(Year %in% years) %>%
      filter(grepl("tran.congestion", CPATCode)) %>%
      mutate(var_plot = ifelse(CPATIndicator == "Peak hours: baseline", "Baseline", Scenario))
    
    if (plot_class == "Time Series 1") {
      ggplot(df) +
        aes(x = Year, y = Value, color = var_plot) +
        geom_line(size = 1, position = "identity") +
        labs(
          x = " ", y = "% Additional Time duration of marginal car trip", title = "Traffic Congestion",
          color = "Scenario"
        ) +
        theme_pubr(base_size = 12) +
        theme(legend.position = "bottom") +
        facet_wrap(vars(Country))
    }
  }
}




# # 1. Time Series Plots (old)
# 
# get_multiyear_plot_old <- function(df, years, countries, plot_class, plot_name) {
#   
#   # 1.1 - Welfare Benefit Time Series Plot
#   if (plot_name == "Welfare Benefits") {
#     
#     # Wrangling plot data
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("wel", CPATCode) & grepl("pct", CPATCode) & Variable == "wel") %>%
#       mutate(`Total Net Benefit (% GDP)` = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", Value * 100, 0)) %>%
#       group_by(Country, Scenario, Year) %>%
#       mutate(`Total Net Benefit (% GDP)` = sum(`Total Net Benefit (% GDP)`)) %>%
#       ungroup() %>%
#       mutate(Value = ifelse(CPATIndicator == "Total national welfare benefits (% GDP)", NA, Value))
#     
#     if (nrow(df) == 0) {
#       print("in no plot")
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area Plot
#     else if (plot_class == "Time Series 2") {
#       print("in here")
#       p <- ggplot(df) +
#         aes(x = Year) +
#         geom_area(aes(y = Value * 100, fill = CPATIndicator, group = CPATIndicator), size = 1.5) +
#         geom_line(aes(x = Year, y = `Total Net Benefit (% GDP)`), color = "#E24346", size = 1, linetype = "dashed") +
#         scale_fill_manual(
#           values = c(
#             `Air pollution co-benefits (% GDP)` = "#0C0CEA",
#             `Climate benefits (real US$ 2021 bn)` = "#1CA851",
#             `Efficiency costs (% GDP)` = "#0F0F0F",
#             `Transport co-benefits (% GDP)` = "#BEC416",
#             `Total Net Benefit (% GDP)` = "#E24346"
#           )
#         ) +
#         labs(x = "Year", y = "% GDP", fill = "", title = "Welfare Costs/Benefits") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_grid(Country ~ Scenario, scales = "free_y") +
#         guides(fill = guide_legend(ncol = 2, byrow = TRUE))
#     }
#     
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year) +
#         geom_line(aes(y = Value * 100, color = CPATIndicator, group = CPATIndicator), size = 1) +
#         geom_line(aes(x = Year, y = `Total Net Benefit (% GDP)`), color = "#E24346", size = 1.5, linetype = "dashed") +
#         scale_color_manual(
#           values = c(
#             `Air pollution co-benefits (% GDP)` = "#0C0CEA",
#             `Climate benefits (real US$ 2021 bn)` = "#1CA851",
#             `Efficiency costs (% GDP)` = "#0F0F0F",
#             `Transport co-benefits (% GDP)` = "#BEC416",
#             `Total Net Benefit (% GDP)` = "#E24346"
#           )
#         ) +
#         labs(x = "Year", y = "% GDP", fill = "", title = "Welfare Costs/Benefits") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_grid(Country ~ Scenario, scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%
#       select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)
#     
#     return(list(p = p, df = df))
#   }
#   
#   
#   # 1.2 - Change in GHG Emissions Time Series 
#   else if (plot_name == "Change in GHG Emissions") {
#     
#     # Wrangling plot data 
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("mit.ghg.tot.inc.red.pct", CPATCode))
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area Plot
#     else if (plot_class == "Time Series 2") {
#       
#       p <- ggplot(df) +
#         aes(x = Year, y = Value * 100, fill = Scenario) +
#         geom_area(size = 1.5, position = "identity", alpha = 0.5) +
#         scale_fill_manual(values = c("#0139A1", "#167F00")) +
#         labs(y = "% relative to baseline", title = "Emissions Reduction", subtitle = "Relative to baseline") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country), ncol = 2)
#       
#     }
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       
#       p <- ggplot(df) +
#         aes(x = Year, y = Value * 100, color = Scenario) +
#         geom_line(size = 1.5) +
#         scale_color_manual(values = c("#0139A1", "#167F00")) +
#         labs(y = "% relative to baseline", title = "Emissions Reduction", subtitle = "Relative to baseline") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country), ncol = 2)
#       
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>% 
#       select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
#     
#     return(list(p = p, df = df))
#   }
#   
#   # 1.3 - Revenue Gain Time Series Plot
#   else if (plot_name == "Revenue Gains") {
#     
#     # Wrangling plot data
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("mit.rev.new.pct.", CPATCode)) %>%
#       mutate(var_p = ifelse(grepl("mit.rev.new.pct.1", CPATCode), "base", "policy")) %>%
#       select(Country, Scenario, var_p, Year, Value) %>%
#       pivot_wider(names_from = var_p, values_from = Value) %>%
#       mutate(rev_gain = (policy - base) * 100)
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area Plot
#     else if (plot_class == "Time Series 2") {
#       p <- ggplot(df) +
#         aes(x = Year, y = rev_gain, fill = Scenario) +
#         geom_area(size = 1.5, position = "identity", alpha = 0.5) +
#         scale_fill_manual(values = c("#0139A1", "#167F00")) +
#         labs(y = "% GDP", title = "Net Annual Revenue Gain (as % GDP)") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(. ~ Country, ncol=2, scales = "free_y")
#     }
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year, y = rev_gain, color = Scenario) +
#         geom_line(size = 1.5) +
#         scale_color_manual(values = c("#0139A1", "#167F00")) +
#         labs(y = "% GDP", title = "Net Annual Revenue Gain (as % GDP)") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(. ~ Country, ncol=2, scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%  
#       select(Country, Scenario, Year, rev_gain)     
#     return(list(p = p, df = df))
#   }
#   
#   # 1.4 Price Increase time series plot
#   else if(plot_name == "Price Increase") {
#     
#     # Wrangling plot data
#     
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("mit.rp.all", CPATCode)) %>%
#       filter(FuelType %in% c("coa", "die", "ecy", "gso", "nga")) %>%
#       filter(CPATIndicator %in% c(
#         "Electricity (% change)", "Coal (% change)", "Natural gas (% change)",
#         "Gasoline (% change)", "Diesel (% change)"
#       )) %>%
#       separate(CPATIndicator, c("Fuel"), sep = " ") %>%
#       mutate(Fuel = ifelse(FuelType == "nga", "Natural Gas", Fuel))
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value*100, color = Fuel) +
#         geom_line(size = 1) +
#         scale_color_manual(values = c(Coal = "#000004",
#                                       Diesel = "#501379",
#                                       Electricity = "#B63778",
#                                       Gasoline = "#FA8764",
#                                       `Natural Gas` = "#CECF82")) +
#         labs(y = "% Change in Prices", title = "Impact on Fossil Fuel Prices", color = "Energy Source") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_grid(Country ~ Scenario, scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%  
#       select(Country, Scenario, Year, Fuel, Value)     
#     return(list(p = p, df = df))
#     
#   }
#   
#   # 1.5 Healthcare Savings Time Series
#   else if (plot_name == "Healthcare Savings") {
#     
#     # Wrangling plot data
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("air.sav.the", CPATCode)) %>%
#       mutate(CPATIndicator = ifelse(grepl("air.sav.the.gov", CPATCode), "Government",
#                                     ifelse(grepl("air.sav.the.ppr", CPATCode), "Prepaid Private",
#                                            ifelse(grepl("air.sav.the.opo", CPATCode), "Out-of-pocket",
#                                                   ifelse(grepl("air.sav.the.das", CPATCode), "Development Assistance", NA)))))
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area Plot
#     else if (plot_class == "Time Series 2") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, fill = CPATIndicator) +
#         geom_area(size = 1.5) +
#         scale_fill_brewer(
#           palette = "Spectral",
#           direction = 1
#         ) +
#         labs(x = " ", y = "Real US $million", title = "Health Expenditure Savings", fill = "Source of Savings") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "left") +
#         facet_grid(vars(Country), vars(Scenario), scales = "free_y")
#     }
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, color = CPATIndicator) +
#         geom_line(size = 1) +
#         scale_color_brewer(
#           palette = "Spectral",
#           direction = 1
#         ) +
#         labs(x = " ", y = "Real US $million", title = "Health Expenditure Savings", fill = "Source of Savings") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "left") +
#         facet_grid(vars(Country), vars(Scenario), scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%       
#       select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
#     return(list(p = p, df = df))
#   }
#   
#   # 1.6 Net Air Pollution averted deaths time series
#   else if (plot_name == "Averted Deaths Air Pollution (Net)") {
#     
#     # Wrangling plot data - Area plot (number of deaths)
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("air.cad.net|air.mort", CPATCode)) %>%
#       mutate(var_plot = ifelse(Variable == "mort", "Baseline", Scenario))
#     
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area plot (number of deaths)
#     else if (plot_class == "Time Series 2") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, fill = var_plot) +
#         geom_area(size = 1.5, position = "identity", alpha = 0.5) +
#         scale_fill_brewer(
#           palette = "Dark2",
#           direction = 1
#         ) +
#         labs(
#           y = "Deaths", title = "Air Pollution deaths averted",
#           subtitle = "Compared to baseline air pollution deaths", fill = " "
#         ) +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country), scales = "free_y")
#     }
#     
#     else if (plot_class == "Time Series 1") {
#       
#       # Wrangling data - Line plot - deaths averted as percentage of baseline
#       df <- df %>%
#         select(Year, Country, Scenario, Variable, var_plot, Value) %>%
#         group_by(Country, Year, Scenario) %>%
#         mutate(deaths_pcbaseline = 1 / ((sum(Value) / Value) - 1)) %>%
#         ungroup() %>%
#         filter(var_plot != "Baseline")
#       
#       # Line plot
#       p <- ggplot(df) +
#         aes(x = Year, y = deaths_pcbaseline*100, color = Scenario) +
#         geom_line(size = 1.5) +
#         scale_color_brewer(
#           palette = "Dark2",
#           direction = 1
#         ) +
#         labs(
#           y = "% Deaths Averted", title = "Air Pollution deaths averted",
#           subtitle = "as % of baseline air pollution deaths", color = " "
#         ) +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country))
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%       
#       select(Country, Scenario, Year, Variable, var_plot, Value)     
#     return(list(p = p, df = df))
#   }
#   
#   # 1.7 Air pollution deaths averted by age group - time series plots
#   else if (plot_name == "Averted Deaths Air Pollution (Age Group)") {
#     
#     # Wrangling plot data
#     df <-  df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("air.ada", CPATCode)) %>%
#       mutate(CPATIndicator = ifelse(grepl("air.ada.65", CPATCode), "65+ Years",
#                                     ifelse(grepl("air.ada.2464", CPATCode), "24-64 Years",
#                                            ifelse(grepl("air.ada.u24", CPATCode), "Below 24", NA))))
#     
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Area Plot
#     else if (plot_class == "Time Series 2") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, fill = CPATIndicator) +
#         geom_area(size = 1.5) +
#         scale_fill_viridis_d(
#           option = "inferno",
#           direction = 1
#         ) +
#         labs(x = "Year", y = "Number of Deaths averted", title = "Air Pollution Deaths Averted (by age group)", fill = "Age Group") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "left") +
#         facet_grid(vars(Country), vars(Scenario), scales = "free_y")
#     }
#     
#     # Line plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, color = CPATIndicator) +
#         geom_line(size = 1) +
#         scale_color_brewer(
#           palette = "Dark2",
#           direction = 1
#         ) +
#         labs(x = "Year", y = "Number of Deaths averted", title = "Air Pollution Deaths Averted (by age group)", color = "Age Group") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "left") +
#         facet_grid(vars(Country), vars(Scenario), scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%       
#       select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
#     return(list(p = p, df = df))
#     
#   }
#   
#   #1.8 Road Fatalities time series plot
#   else if (plot_name == "Road Fatalities") {
#     
#     # Wrangling plot data
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("tran.deaths", CPATCode)) %>%
#       mutate(var_plot = ifelse(CPATIndicator == "Transport accidents, Baseline", "Baseline", Scenario))
#     
#     if (nrow(df) == 0) {
#       
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     # Line Plot
#     else if (plot_class == "Time Series 1") {
#       p <- ggplot(df) +
#         aes(x = Year, y = Value, colour = var_plot) +
#         geom_line(size = 1) +
#         scale_color_hue(direction = 1) +
#         labs(x = " ", y = "Road Fatalities (count)", title = "Changes in Road Fatalities", color = "Scenario") +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country), scales = "free_y")
#     }
#     else {
#       p <- ggplot() +
#         labs(x = "No Plot Available")
#     }
#     
#     df <- df %>%       
#       select(Country, Scenario, Year, CPATIndicator, CPATCode, Value)     
#     return(list(p = p, df = df))
#   }
#   else if (plot_name == "Congestion Time") {
#     df <- df %>%
#       filter(Country %in% countries) %>%
#       filter(Year %in% years) %>%
#       filter(grepl("tran.congestion", CPATCode)) %>%
#       mutate(var_plot = ifelse(CPATIndicator == "Peak hours: baseline", "Baseline", Scenario))
#     
#     if (plot_class == "Time Series 1") {
#       ggplot(df) +
#         aes(x = Year, y = Value, color = var_plot) +
#         geom_line(size = 1, position = "identity") +
#         labs(
#           x = " ", y = "% Additional Time duration of marginal car trip", title = "Traffic Congestion",
#           color = "Scenario"
#         ) +
#         theme_pubr(base_size = 12) +
#         theme(legend.position = "bottom") +
#         facet_wrap(vars(Country))
#     }
#   }
# }
# 
