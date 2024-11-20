rm(list=ls())

packages<- c("StrathE2E2", "dplyr", "tidyr", "ggplot2", "patchwork")
lapply(packages, library, character.only = TRUE)

CNRM126<-readRDS("./temp/CNRM126_res.rds")
CNRM370<-readRDS("./temp/CNRM370_res.rds")
GFDL126<-readRDS("./temp/GFDL126_res.rds")
GFDL370<-readRDS("./temp/GFDL370_res.rds")

results_lists<-list(CNRM126,CNRM370,GFDL126,GFDL370)

# Define decade names
decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")
scenarios <- list("CNRM_SSP126", "CNRM_SSP370","GFDL_SSP126","GFDL_SSP370")

names(results_lists)<-scenarios

# Assign decade names to each scenario's results
results_lists <- lapply(results_lists, function(results) {
  names(results) <- decades
  results
})

# Extract "Corpses" data for all scenarios and decades
Fish_biomass <- lapply(names(results_lists), function(scenario) {
  # Access each scenario
  scenario_results <- results_lists[[scenario]]
  
  # Loop through decades
  lapply(names(scenario_results), function(decade) {
    # Access final.year.outputs$mass_results_wholedomain for the decade
    mass_results <- scenario_results[[decade]]$final.year.outputs$mass_results_wholedomain
    production_results<-scenario_results[[decade]]$final.year.outputs$annual_flux_results_wholedomain
    
    # Filter for "Corpses"
    plankti_biomass <- mass_results %>%
      filter(Description == "Planktivorous_fish")
    demer_biomass <- mass_results %>%
      filter(Description == "Demersal_fish")
    
    # Filter for "Corpses"
    plankti_production <- production_results %>%
      filter(Description == "Planktiv.fish_net_production")
    demer_production <- production_results %>%
      filter(Description == "Dem.fish_net_production")
    
    # Create a data frame with Decade, Biomass, and Scenario
    data.frame(
      Decade = decade,
      Biomass_plankti = plankti_biomass$Model_annual_mean,
      Biomass_demer = demer_biomass$Model_annual_mean,
      Production_plankti = plankti_production$Model_annual_flux,
      Production_demer = demer_production$Model_annual_flux,
      Scenario = scenario
    )
  }) %>% bind_rows() # Combine decades for the scenario
}) %>% bind_rows() # Combine all scenarios

# Convert Decade to factor for proper plotting
Fish_biomass$Decade <- factor(Fish_biomass$Decade, levels = unique(Fish_biomass$Decade))

# Plot for planktivorous fish
p1<-ggplot(Fish_biomass, aes(x = Decade, y = Biomass_plankti, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual biomass of plantivorous fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 32))

# Plot for demersal fish
p2<-ggplot(Fish_biomass, aes(x = Decade, y = Biomass_demer, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual biomass of demersal fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 32))

p3<-ggplot(Fish_biomass, aes(x = Decade, y = Production_plankti, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual net production of plantivorous fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 32))

# Plot for demersal fish
p4<-ggplot(Fish_biomass, aes(x = Decade, y = Production_demer, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual net production of demersal fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 32))
(p1|p2)/(p3|p4)+ 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

