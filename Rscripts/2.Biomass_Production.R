rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
results_lists<-readRDS("./temp/list_results.rds")


# Extract the biomass and production data for all scenarios and decades
Fish_mass_prod <- lapply(names(results_lists), function(scenario) { 
  # Access each scenario
  scenario_results <- results_lists[[scenario]] #for each of the scenarios
  
  # Loop through decades
  lapply(names(scenario_results), function(decade) {
    # Access final.year.outputs$mass_results_wholedomain for the decade
    mass_results <- scenario_results[[decade]]$final.year.outputs$mass_results_wholedomain #get the biomass
    production_results<-scenario_results[[decade]]$final.year.outputs$annual_flux_results_wholedomain #get the production fluxes
    
    # Filter biomass for Demersal and planktivorous fish
    plankti_biomass <- mass_results %>%
      filter(Description == "Planktivorous_fish")
    demer_biomass <- mass_results %>%
      filter(Description == "Demersal_fish")
    
    # Filter production for Demersal and planktivorous fish
    plankti_production <- production_results %>%
      filter(Description == "Planktiv.fish_net_production")
    demer_production <- production_results %>%
      filter(Description == "Dem.fish_net_production")
    
    # Create a data frame
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
Fish_mass_prod$Decade <- factor(Fish_mass_prod$Decade, levels = unique(Fish_mass_prod$Decade))

# Plot for planktivorous fish biomass
p1<-ggplot(Fish_mass_prod, aes(x = Decade, y = Biomass_plankti, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual biomass of plantivorous fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))

# Plot for demersal fish biomass
p2<-ggplot(Fish_mass_prod, aes(x = Decade, y = Biomass_demer, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual biomass of demersal fish\nover time for all scenarios",
       x = "",
       y = "Biomass (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))

# Plot for planktivorous fish production
p3<-ggplot(Fish_mass_prod, aes(x = Decade, y = Production_plankti, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual net production of plantivorous fish\nover time for all scenarios",
       x = "",
       y = "Production (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))

# Plot for demersal fish production
p4<-ggplot(Fish_mass_prod, aes(x = Decade, y = Production_demer, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual net production of demersal fish\nover time for all scenarios",
       x = "",
       y = "Production (mMN/m2/y)",
       color = "Scenario") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))


# Open a pdf file
jpeg("Plots/Mass_Prod.jpeg", width = 1200,height = 500) 
# 2. Create a plot
(p1|p2)/(p3|p4)+ 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
# Close the pdf file
dev.off() 


