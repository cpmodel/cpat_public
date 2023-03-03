library(readxl)
library(tidyverse)
library(countrycode)

# Get the file path of the current R file
filePath <- rstudioapi::getSourceEditorContext()$path

# Set the working directory to the directory containing the R file
setwd(dirname(filePath))

mto_file_path <- "./MTO_Dashboard.xlsx"

df_name <- as.data.frame(readxl::read_xlsx(mto_file_path,
                                           sheet = "Scenario settings", range = "A1:B3", col_names = FALSE
))


df_scenarios <- readxl::read_xlsx(mto_file_path, sheet = "Scenario settings", skip=2, col_names = FALSE) %>%
  filter(.data[[names(.)[1]]] %in% c ("Scenario identification", "Scenario ID")) %>%
  t(.)

df_scenarios <- as.data.frame(df_scenarios)
colnames(df_scenarios) <- df_scenarios[1,]
rownames(df_scenarios) <- NULL
df_scenarios <- df_scenarios[-1,]


  

df_scenario_settings <- readxl::read_xlsx(mto_file_path,
                                          sheet = "Scenario settings", skip=3)



df_raw <- readxl::read_xlsx(mto_file_path, sheet = "Results") %>%
  filter(!is.na(Country)) %>%
  mutate(Country = ifelse(Country == "Turkiye", "Turkey", Country)) %>%
  mutate(iso3 =  countrycode::countrycode(Country, "country.name", "iso3c")) %>%
  mutate(Scenario = ifelse(Scenario == "U1", "Low Ambition", "Paris Aligned"))


df <- df_raw %>%
  filter(!is.na(Country)) %>%
  select(1:40, 91) %>%
  pivot_longer(cols = 23:40, names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(Value = as.numeric(Value)) %>%
  filter((grepl("wel", CPATCode) & grepl("pct", CPATCode) & Variable == "wel") | 
           (grepl("mit.ghg.tot.inc.red.pct", CPATCode)) |
           (grepl("mit.rev.new.pct.", CPATCode)) |
           (grepl("mit.rp.all", CPATCode)) |
           (grepl("air.sav.the", CPATCode)) |
           (grepl("air.cad.net|air.mort", CPATCode)) |
           (grepl("air.ada", CPATCode)) |
           (grepl("tran.deaths", CPATCode)) |
           (grepl("tran.congestion", CPATCode)))

df_cost_distn <- df_raw %>%
  filter(grepl("distn.", MTCode) & grepl(".tot.", MTCode)) %>%
  filter(!(Variable %in% c("pop_tot_adj", "tot_cons_na"))) %>%
  filter(!(grepl("population", CPATIndicator))) %>%
  filter(!(grepl("total effect", CPATIndicator)))

save(df_name, df_scenario_settings, df_cost_distn, df, file = "mt_dashboard.rda")
