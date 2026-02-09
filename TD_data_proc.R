library(patchwork)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(magrittr)
library(data.table)
library(purrr)
library(devtools)

setwd("C:/Minerals/GCAM-TD-CMM")

source("constants.R")

#read in csv
ctry_mapping <- readr::read_csv("input/ctry_mapping.csv")
GCAM_region_names <- readr::read_csv("input/GCAM_region_names.csv", skip = 6)

Kalt_2017_td_ctry <- readr::read_csv("input/Kalt_2017_td_ctry.csv", skip = 7)
Kalt_2017_t_line_kV_reg <- readr::read_csv("input/Kalt_2017_t_line_kV_reg.csv", skip = 7)
Kalt_2017_elec_cap_gw_ctry <- readr::read_csv("input/Kalt_2017_elec_cap_gw_ctry.csv", skip = 7)
Kalt_mineral_intensity_data <- readr::read_csv("input/Kalt_mineral_intensity_data.csv", skip = 7)

Deetman_2016_td_line_underground_share <- readr::read_csv("input/Deetman_2016_td_line_underground_share.csv", skip = 7)
Deetman_2016_td_line_calculation_IMAGE_reg <- readr::read_csv("input/Deetman_2016_td_line_calculation_IMAGE_reg.csv", skip = 7) %>%
  select(Deetman_IMAGE_reg, `Voltage level`, `km value used in Deetman et al.`)
Deetman_2016_td_equip_per_km <- readr::read_csv("input/Deetman_2016_td_equip_per_km.csv", skip = 7)
Deetman_2016_elec_cap_mw_reg <- readr::read_csv("input/Deetman_2016_elec_cap_mw_reg.csv", skip = 7)
Deetman_mineral_intensity_data <- readr::read_csv("input/Deetman_mineral_intensity_data.csv", skip = 7)

A10.mineral_price <- readr::read_csv("input/A10.mineral_price.csv")
A26.globaltech_cost <- readr::read_csv("input/A26.globaltech_cost.csv", skip = 7)
L226.GlobalTechCost_en <- readr::read_csv("input/L226.GlobalTechCost_en.csv")

GCAM_reg_gdp_pc <- readr::read_csv("data_GCAM_runs/GCAM_reg_gdp_pc.csv", skip = 1) %>%
  select(-scenario) %>%
  mutate(Units = "thous90US$/p")

#GCAM Reference case output
elec_gen <- readr::read_csv("data_GCAM_runs/elec_gen.csv")
pop <- readr::read_csv("data_GCAM_runs/pop.csv")

# PROCESSING KALT ET AL INFRASTRUCTURE DATA ----------------------------------------------

# Map Kalt region groups to countries
Kalt_td_ctry_reg <- Kalt_2017_td_ctry %>%
  filter(Country != "Total") %>%
  pivot_longer(cols = c("transmission",
                        "distribution",
                        "transmission_transformer",
                        "distribution_transformer",
                        "genstepup_transformer"), names_to = "System") %>%
  left_join(ctry_mapping) %>%
  mutate(value = as.numeric(value)) %>%
  na.omit()


# For transmission lines, calculate shares of lines by type and voltage by aggregated region
Kalt_t_reg_total <- Kalt_2017_t_line_kV_reg %>%
  group_by(Kalt_reg) %>%
  dplyr::summarise(value_total = sum(`2017`)) %>%
  ungroup()

Kalt_t_reg_share <- Kalt_2017_t_line_kV_reg %>%
  left_join(Kalt_t_reg_total) %>%
  mutate(share = `2017`/value_total) %>%
  select(Kalt_reg, `Line type and voltage level`, share)


# Get transmission lines by type and voltage by country
Kalt_t_line_kV_ctry <- Kalt_td_ctry_reg %>%
  filter(System == "transmission") %>%
  full_join(Kalt_t_reg_share, by = c("Kalt_reg"), relationship = "many-to-many") %>%
  mutate(value_line_kv = value*share) %>%
  select(-value, -share) %>%
  rename(value = value_line_kv) %>%
  select(-Deetman_IMAGE_reg)

# For distribution lines, first take 63% as the share of distribution lines that are low-voltage (<1 kV).
# This is specified in Fig 8 of Kalt et al. (2021) https://doi.org/10.1016/j.dib.2021.107351
# Then, need to use the % underground share for Low Voltage (LV) from Deetman et al. (2021) to split into over head vs. underground.
Kalt_d_line_kV_ctry <- Kalt_td_ctry_reg %>%
  filter(System == "distribution") %>%
  mutate(`<=1 kV` = value*0.63,
         `1-100 kV` = value*(1-0.63)) %>%
  left_join(select(Deetman_2016_td_line_underground_share, c("Deetman_IMAGE_reg", "LV"))) %>%
  mutate(pct_underground = if_else(is.na(LV), 0, LV),
         pct_underground = pct_underground*0.01,
         `Cable 1-100 kV` = `1-100 kV`*pct_underground,
         `OH 1-100 kV` = `1-100 kV`*(1-pct_underground)) %>%
  select(Country, System, Kalt_reg, `<=1 kV`, `Cable 1-100 kV`, `OH 1-100 kV`) %>%
  pivot_longer(cols = c("<=1 kV", "Cable 1-100 kV", "OH 1-100 kV"), names_to = "Line type and voltage level")


# Compile the Kalt 2017 transmission and distribution line data (by type and voltage) together
Kalt_2017_td_line_kV_ctry <- bind_rows(Kalt_t_line_kV_ctry,
                                  Kalt_d_line_kV_ctry) %>%
  select(Country, Kalt_reg, System, `Line type and voltage level`, value)



# PROCESSING KALT ET AL MINERAL INTENSITY DATA ----------------------------------------------

#LINES
# Compile transmission and distribution line mineral intensity data
# Add pylons and conductors intensity together (given separately for transmission lines)
Kalt_td_line_mineral_intensity_data <- Kalt_mineral_intensity_data %>%
  filter(!is.na(`Line type and voltage level`)) %>%
  group_by(System, `Line type and voltage level`, mineral, unit) %>%
  dplyr::summarise(intensity = sum(intensity)) %>%
  ungroup()

#multiply the 2017 transmission and distribution line data by mineral intensity to get the 2017 line mineral requirements
# km * tons/km = tons
Kalt_td_line_mineral_2017 <- Kalt_2017_td_line_kV_ctry %>%
  left_join(Kalt_td_line_mineral_intensity_data, by = c("System", "Line type and voltage level"), relationship = "many-to-many") %>%
  mutate(value = value * intensity,
         unit = "tons") %>%
  na.omit()


#TRANSFORMERS
# Select the transformer mineral intensity data
Kalt_td_transformer_mineral_intensity_data <- Kalt_mineral_intensity_data %>%
  filter(is.na(`Line type and voltage level`)) %>%
  select(System, Component, mineral, intensity, unit)

#multiply transformer data by mineral intensity to get the 2017 transformer mineral requirements
#GVA * tons/GVA = tons
Kalt_td_transformer_mineral_2017 <- Kalt_td_ctry_reg %>%
  filter(! (System %in% c("transmission", "distribution"))) %>%
  #map gen step-up transformer to transmission for calculating mineral requirements
  mutate(System = if_else(System == "distribution_transformer", "distribution", "transmission")) %>%
  group_by(Country, System, Kalt_reg, Deetman_IMAGE_reg) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(Kalt_td_transformer_mineral_intensity_data, by = c("System"), relationship = "many-to-many") %>%
  mutate(value = value * intensity,
         unit = "tons") %>%
  na.omit()

#calculate mineral intensity by dividing by 2017 electricity capacity (GW)
#tons / GW = tons/GW
Kalt_2017_total_elec_cap_gw_ctry <- Kalt_2017_elec_cap_gw_ctry %>%
  group_by(Country, Kalt_reg) %>%
  dplyr::summarise(value = sum(`2017`)) %>%
  ungroup()

Kalt_td_line_mineral_intensity_t_GW <- Kalt_td_line_mineral_2017 %>%
  left_join(Kalt_2017_total_elec_cap_gw_ctry, by = c("Country", "Kalt_reg"), suffix = c(".t", ".GW")) %>%
  mutate(value = value.t/value.GW,
         unit = "t/GW") %>%
  na.omit()

Kalt_td_transformer_mineral_intensity_t_GW <- Kalt_td_transformer_mineral_2017 %>%
  left_join(Kalt_2017_total_elec_cap_gw_ctry, by = c("Country", "Kalt_reg"), suffix = c(".t", ".GW")) %>%
  mutate(value = value.t/value.GW,
         unit = "t/GW") %>%
  na.omit()

# add intensities for all transmission vs. distribution components together
Kalt_td_mineral_intensity_t_GW <- bind_rows(Kalt_td_line_mineral_intensity_t_GW,
                                            Kalt_td_transformer_mineral_intensity_t_GW) %>%
  group_by(Country, Kalt_reg, System, mineral, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()


#Add transmission and distribution intensities to get a final mineral intensity (t/GW) for the T&D sector for each country
# t/GW * 1000 kg/1 t * 1 GW/10^6 kW = kg/kW
Kalt_td_mineral_intensity_final_kg_kW <- Kalt_td_mineral_intensity_t_GW %>%
  group_by(Country, Kalt_reg, mineral, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = value/(10^3),
         unit = "kg/kW") %>%
  select(Country, mineral, unit, value) %>%
  mutate(Source = "Kalt")


# PROCESSING DEETMAN ET AL INFRASTRUCTURE DATA -------------------------

#Calculate over head vs. underground lines of high, medium, and low voltage lines per IMAGE reg in 2016
Deetman_td_line_type_kV_reg <- Deetman_2016_td_line_calculation_IMAGE_reg %>%
  pivot_wider(names_from = `Voltage level`,
              values_from = `km value used in Deetman et al.`) %>%
  left_join(Deetman_2016_td_line_underground_share, by = c("Deetman_IMAGE_reg")) %>%
  mutate(`HV overhead` = High * (1-(HV/100)),
         `HV underground` = High * (HV/100),
         `MV overhead` = Medium * (1-(MV/100)),
         `MV underground` = Medium * (MV/100),
         `LV overhead` = Low * (1-(LV/100)),
         `LV underground` = Low * (LV/100)) %>%
  select(Deetman_IMAGE_reg, `HV overhead`, `HV underground`,`MV overhead`, `MV underground`, `LV overhead`, `LV underground`) %>%
  pivot_longer(cols = c(`HV overhead`, `HV underground`,`MV overhead`, `MV underground`, `LV overhead`, `LV underground`),
               names_to = "Component",
               values_to = "value")

#Calculate transformers and substations for high, medium, and low voltage lines per IMAGE reg in 2016
Deetman_td_transformer_type_reg <- Deetman_2016_td_line_calculation_IMAGE_reg %>%
  left_join(Deetman_2016_td_equip_per_km, by = c("Voltage level")) %>%
  mutate(value_equip = value * `km value used in Deetman et al.`) %>%
  select(Deetman_IMAGE_reg, `Voltage level`, unit, value_equip) %>%
  mutate(`Voltage level` = case_when(`Voltage level` == "High" ~ "HV",
                                     `Voltage level` == "Medium" ~"MV",
                                     `Voltage level` == "Low" ~ "LV"),
         Component = paste0(`Voltage level`, " ", unit),
         value = value_equip) %>%
  select(Deetman_IMAGE_reg, Component, value)

#compile the line and equipment data together
Deetman_td_reg <- bind_rows(Deetman_td_line_type_kV_reg,
                            Deetman_td_transformer_type_reg)

# PROCESSING DEETMAN ET AL MINERAL INTENSITY DATA -------------------------

#multiply the 2016 T&D lines and equipment data by mineral intensity to get the 2016 mineral requirements
Deetman_td_mineral_2016 <- Deetman_td_reg %>%
  left_join(Deetman_mineral_intensity_data, by = c("Component")) %>%
  mutate(value = (value*intensity),
         unit = "kg")

Deetman_total_td_mineral_2016 <- Deetman_td_mineral_2016 %>%
  group_by(Deetman_IMAGE_reg, mineral, unit) %>%
  dplyr::summarise(value = sum(value))

#Map Deetman shares of total mineral consumption to countries based on Kalt electricity capacity in 2017
Deetman_reg_Kalt_ctry_elec_cap <- Kalt_2017_total_elec_cap_gw_ctry %>%
  left_join(ctry_mapping, by = c("Country", "Kalt_reg")) %>%
  # omit NAs, won't be part of the share
  na.omit()

Deetman_reg_Kalt_elec_cap_total <- Deetman_reg_Kalt_ctry_elec_cap %>%
  group_by(Deetman_IMAGE_reg) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

Deetman_reg_Kalt_ctry_elec_cap_share <- Deetman_reg_Kalt_ctry_elec_cap %>%
  left_join(Deetman_reg_Kalt_elec_cap_total, by = c("Deetman_IMAGE_reg"), suffix = c(".ctry", ".total_reg")) %>%
  mutate(share = value.ctry/value.total_reg) %>%
  select(-value.ctry, -value.total_reg)

# Share out Deetman mineral consumption by region to countries
Deetman_ctry_mineral_2016 <- Deetman_reg_Kalt_ctry_elec_cap_share %>%
  left_join(Deetman_total_td_mineral_2016, by = c("Deetman_IMAGE_reg")) %>%
  mutate(value = value*share)

#calculate mineral intensity by dividing by 2017 Kalt electricity capacity (GW)
#kg / GW * 1 GW/10^6 kW = kg/kW
Deetman_td_mineral_intensity_final_kg_kW <- Deetman_ctry_mineral_2016 %>%
  left_join(Kalt_2017_total_elec_cap_gw_ctry, by = c("Country", "Kalt_reg"), suffix = c(".kg", ".GW")) %>%
  mutate(value = value.kg/(value.GW*10^6),
         unit = "kg/kW") %>%
  select(Country, mineral, unit, value) %>%
  mutate(Source = "Deetman")


# #calculate mineral intensity by dividing by 2016 IMAGE electricity capacity (MW)
# #kg / MW * MW/1000 kW = kg/kW
# Deetman_td_mineral_intensity_kg_kW <- Deetman_td_mineral_2016 %>%
#   left_join(Deetman_2016_elec_cap_mw_reg, by = c("Deetman_IMAGE_reg")) %>%
#   mutate(value = value/(`2016`*1000),
#          unit = "kg/kW")
#
# #Add all component intensities to get a final mineral intensity (kg/kW) for the T&D sector for each region
# Deetman_td_mineral_intensity_final_kg_kW <- Deetman_td_mineral_intensity_kg_kW  %>%
#   group_by(Deetman_IMAGE_reg, mineral, unit) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   ungroup()


# MAPPING TO GCAM REGIONS -------------------------------------------------

ctry_td_mineral_intensity_final_kg_kw <- bind_rows(Kalt_td_mineral_intensity_final_kg_kW,
                                                   Deetman_td_mineral_intensity_final_kg_kW)

#Calculate weighted average based on total elec capacity

#First, calculate share of capacity by GCAM region
GCAM_reg_Kalt_ctry_elec_cap <- Kalt_2017_total_elec_cap_gw_ctry %>%
  left_join(ctry_mapping, by = c("Country", "Kalt_reg")) %>%
  # omit NAs, won't be part of the share
  na.omit()

GCAM_reg_Kalt_elec_cap_total <- GCAM_reg_Kalt_ctry_elec_cap %>%
  group_by(GCAM_region_ID) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

GCAM_reg_Kalt_ctry_elec_cap_share <- GCAM_reg_Kalt_ctry_elec_cap %>%
  left_join(GCAM_reg_Kalt_elec_cap_total, by = c("GCAM_region_ID"), suffix = c(".ctry", ".total_reg")) %>%
  mutate(share = value.ctry/value.total_reg) %>%
  select(-value.ctry, -value.total_reg)

# Multiply mineral intensity by share for each country and sum up by GCAM reg
GCAM_td_mineral_intensity_kg_kw <- ctry_td_mineral_intensity_final_kg_kw %>%
  left_join(GCAM_reg_Kalt_ctry_elec_cap_share, by = c("Country")) %>%
  mutate(value = value * share) %>%
  group_by(GCAM_region_ID, mineral, unit, Source) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

GCAM_td_mineral_intensity_kg_kw_comparison <- GCAM_td_mineral_intensity_kg_kw %>%
  pivot_wider(names_from = Source, values_from = value)

# CONVERT TO Mt PER EJ ----------------------------------------------------
  # mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
  # mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760),
  # mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ)

GCAM_td_mineral_intensity_Mt_EJ <- GCAM_td_mineral_intensity_kg_kw %>%
  mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
         mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760),
         mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ)) %>%
  select(GCAM_region_ID, mineral, Source, mineral_intensity_Mt_EJ)


# PROJECT FUTURE MINERAL INTENSITIES --------------------------------------


## GCAM base year mineral intensities
## For steel and aluminum, take the average of Kalt and Deetman
# For copper, use Kalt only
GCAM_td_mineral_intensity_Mt_EJ_2015 <- GCAM_td_mineral_intensity_Mt_EJ %>%
  filter(!(Source == "Deetman" & mineral == "copper")) %>%
  ungroup() %>%
  select(-Source) %>%
  group_by(GCAM_region_ID, mineral) %>%
  dplyr::summarise(value = mean(mineral_intensity_Mt_EJ)) %>%
  mutate(year = 2015)

#Calculate per capita elec gen
total_elec_gen <- elec_gen %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()


total_elec_gen_pc_2050 <- total_elec_gen %>%
  left_join(pop, by = c("scenario", "region", "year"), suffix = c(".EJ", ".thous")) %>%
  mutate(value = value.EJ/value.thous) %>%
  filter(year == 2050) %>%
  select(scenario, region, value) %>%
  mutate(value = value*1000) %>%
  distinct()

# Calculate future (2050) mineral intensities based on the regressions
# y = log(mineral intensity (Mt/EJ))
# x = log(elec gen per capita (EJ/p))
# Steel: y = -1.3 - 0.41x
# Aluminium: -1.5 - 0.32x
# Copper: y = -1.9 = 0.47x
#use Reference scenario for 2050
GCAM_td_steel_intensity_Mt_EJ_2050 <- total_elec_gen_pc_2050 %>%
  filter(scenario == "Reference") %>%
  rename(elec_gen_pc = value) %>%
  mutate(mineral = "steel",
         log_elec_gen_pc = log(elec_gen_pc),
         log_Mt_EJ = -1.3-(0.41*log_elec_gen_pc),
         mineral_intensity_Mt_EJ = exp(log_Mt_EJ))

GCAM_td_aluminium_intensity_Mt_EJ_2050 <- total_elec_gen_pc_2050 %>%
  filter(scenario == "Reference") %>%
  rename(elec_gen_pc = value) %>%
  mutate(mineral = "aluminium",
         log_elec_gen_pc = log(elec_gen_pc),
         log_Mt_EJ = -1.5-(0.32*log_elec_gen_pc),
         mineral_intensity_Mt_EJ = exp(log_Mt_EJ))

GCAM_td_copper_intensity_Mt_EJ_2050 <- total_elec_gen_pc_2050 %>%
  filter(scenario == "Reference") %>%
  rename(elec_gen_pc = value) %>%
  mutate(mineral = "copper",
         log_elec_gen_pc = log(elec_gen_pc),
         log_Mt_EJ = -1.9-(0.47*log_elec_gen_pc),
         mineral_intensity_Mt_EJ = exp(log_Mt_EJ))

GCAM_td_mineral_intensity_Mt_EJ_2050 <- bind_rows(GCAM_td_steel_intensity_Mt_EJ_2050,
                                                  GCAM_td_aluminium_intensity_Mt_EJ_2050,
                                                  GCAM_td_copper_intensity_Mt_EJ_2050) %>%
  select(region, mineral, mineral_intensity_Mt_EJ)

#Compare regression-calculated 2050 values to 2015 values for each region, mineral
#If 2015 value is lower than 2050 value, use 2015 value for all years
#Copy 2050 values to 2100
GCAM_td_mineral_intensity_Mt_EJ_2015_2050_2100 <- GCAM_td_mineral_intensity_Mt_EJ_2050 %>%
  left_join(GCAM_region_names) %>%
  left_join(GCAM_td_mineral_intensity_Mt_EJ_2015, by = c("GCAM_region_ID", "mineral")) %>%
  select(-year) %>%
  rename(value.2050 = mineral_intensity_Mt_EJ,
         value.2015 = value) %>%
  mutate(value.2050 = if_else(value.2015 < value.2050, value.2015, value.2050),
         value.2100 = value.2050) %>%
  pivot_longer(cols = c(value.2015, value.2050, value.2100), names_to = "value.year") %>%
  separate(value.year, into = c(NA, "year"), sep = "\\.") %>%
  mutate(year = as.numeric(year))

GCAM_td_mineral_intensity_Mt_EJ_all_yr <- GCAM_td_mineral_intensity_Mt_EJ_2015_2050_2100 %>%
  select(-year, -value) %>%
  distinct() %>%
  gcamdata::repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
  left_join(GCAM_td_mineral_intensity_Mt_EJ_2015_2050_2100, by = c("GCAM_region_ID", "region", "mineral", "year")) %>%
  mutate(year = as.numeric(year),
         value = as.numeric(value)) %>%
  group_by(GCAM_region_ID, region, mineral) %>%
  mutate(value = gcamdata::approx_fun(year, value)) %>%
  ungroup()

# MINERAL COST  ------------------------------------------------------------


#  Calculate the mineral cost for t&d technologies.
#  Need to convert to a levelized cost using a fixed charge rate
#  Mt/EJ * $1975/kg * 1 EJ/10^9 GJ * 1000 t/1 Mt * 1000 kg/1 t * 0.13 (fixed charge rate) = $/GJ (per year)
GCAM_td_mineral_cost <- GCAM_td_mineral_intensity_Mt_EJ_all_yr %>%
  left_join(A10.mineral_price, by = c("mineral" = "resource", "year")) %>%
  mutate(mineral_cost = (value * mineral_price) * 0.13/ (CONV_T_MT*CONV_KG_T * CONV_EJ_GJ))

GCAM_td_mineral_cost_total <- GCAM_td_mineral_cost %>%
  group_by(GCAM_region_ID, region, year) %>%
  dplyr::summarise(mineral_cost = sum(mineral_cost))

#Find median total mineral cost, across regions and sources.
GCAM_td_mineral_cost_total_median_scalar <- GCAM_td_mineral_cost_total %>%
  group_by(year) %>%
  dplyr::mutate(avg_cost = mean(mineral_cost),
                median_cost = median(mineral_cost)) %>%
  ungroup() %>%
  mutate(scalar = mineral_cost/median_cost)
  #Calculate scalar as each region's mineral cost/median mineral cost (across all 32 reg)

GCAM_td_total_cost <- L226.GlobalTechCost_en %>%
  filter(sector.name %in% c("elect_td_bld", "elect_td_ind", "elect_td_trn") )

# Scale the total t&d cost by regional variation in mineral cost
GCAM_td_total_cost_regional <- GCAM_td_mineral_cost_total_median_scalar %>%
  select(GCAM_region_ID, region, year, scalar) %>%
  left_join(GCAM_td_total_cost, by = c("year")) %>%
  mutate(input.cost = input.cost * scalar)

# calculate non-mineral t&d cost = total t&d cost - mineral t&d cost
# this will be the new "non-energy" cost input into the t&d sectors
GCAM_td_nonmineral_cost_regional <- GCAM_td_total_cost_regional %>%
  select(GCAM_region_ID, region, sector.name, subsector.name, technology, year, minicam.non.energy.input, input.cost) %>%
  rename(supplysector = sector.name,
         subsector = subsector.name,
         stub.technology = technology) %>%
  left_join_error_no_match(GCAM_td_mineral_cost_total, by = c("GCAM_region_ID", "region", "year"), suffix = c(".total", ".mineral")) %>%
  mutate(input.cost = input.cost - mineral_cost)


# WRITE CSV OUTPUTS -------------------------------------------------------

#Write csv outputs for:
# mineral intensity by mineral, region, year
# non-mineral non-energy costs by region, year

A26.td_mineral_coef <- GCAM_td_mineral_intensity_Mt_EJ_all_yr %>%
  select(region, mineral, year, value) %>%
  write.csv("A26.td_mineral_coef.csv", row.names = F)

A26.td_nonenergy_cost_nonmineral <- GCAM_td_nonmineral_cost_regional %>%
  select(region, supplysector, subsector, stub.technology, year, minicam.non.energy.input, input.cost) %>%
  write.csv("A26.td_nonenergy_cost_nonmineral.csv", row.names = F)

