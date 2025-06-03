rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","tidyr") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
results_lists<-readRDS("./temp/list_results.rds")


# Create a single unified data frame
Mass_prod <- lapply(names(results_lists), function(scenario) { 
  scenario_results <- results_lists[[scenario]]
  
  lapply(names(scenario_results), function(decade) {
    outputs <- scenario_results[[decade]]$final.year.outputs
    
    # Define species/components, groups, units
    biomass_info <- list(
      list(desc = "Surface_layer_phytoplankton", group = "Plankton", component = "Phytoplankton", divisor = 1.257861635),
      list(desc = "Omnivorous_zooplankton", group = "Plankton", component = "Omnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Carnivorous_zooplankton", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Planktivorous_fish", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Demersal_fish", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Migratory_fish", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Pinnipeds", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Cetaceans", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Birds", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthos_susp/dep_feeders", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthos_carn/scav_feeders", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308),
      list(desc = "Surface_layer_ammonia", group = "Nutrient", component = "Ammonia", divisor = 1),
      list(desc = "Surface_layer_nitrate", group = "Nutrient", component = "Nitrate", divisor = 1)
    )
    
    production_info <- list(
      list(desc = "Phytoplankton_net_primary_production", group = "Plankton", component = "Phytoplankton", divisor = 1.257861635),
      list(desc = "Omniv.zooplankton_net_production", group = "Plankton", component = "Omnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Carniv.zooplankton_net_production", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Planktiv.fish_net_production", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_net_production", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Mig.fish_net_production", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Pinniped_net_production", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Cetacean_net_production", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_net_production", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthos_susp/dep_net_production", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthos_carn/scav_net_production", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308)
    )
    
    biomass_df <- lapply(biomass_info, function(info) {
      row <- outputs$mass_results_wholedomain %>%
        filter(Description == info$desc)
      if (nrow(row) == 0) return(NULL)
      data.frame(
        Scenario = scenario,
        Decade = decade,
        Group = info$group,
        Component = info$component,
        Variable = "Biomass",
        Value = row$Model_annual_mean / info$divisor
      )
    }) %>% bind_rows()
    
    production_df <- lapply(production_info, function(info) {
      row <- outputs$annual_flux_results_wholedomain %>%
        filter(Description == info$desc)
      if (nrow(row) == 0) return(NULL)
      data.frame(
        Scenario = scenario,
        Decade = decade,
        Group = info$group,
        Component = info$component,
        Variable = "Production",
        Value = row$Model_annual_flux / info$divisor
      )
    }) %>% bind_rows()
    
    bind_rows(biomass_df, production_df)
  }) %>% bind_rows()
}) %>% bind_rows()

Mass_prod$Decade <- factor(Mass_prod$Decade, levels = sort(unique(Mass_prod$Decade)))

Scenario_colours <- c(
  "CNRM_SSP126" = "#1B9AAA",
  "CNRM_SSP370" = "#A0DDE6",
  "GFDL_SSP126" = "#D7263D",
  "GFDL_SSP370" = "#F26B6B"
)



# Define a plotting function
plot_bars <- function(df, var, comp, ylab, title) {
  ggplot(df %>% filter(Variable == var, Component == comp),
         aes(x = Decade, y = Value, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75,alpha = 0.8) +
    ylim(0, max(df$Value[df$Variable == var & df$Component == comp], na.rm = TRUE) * 1.1) +
    labs(title = title, y = ylab, x = "") +
    scale_fill_manual(values = Scenario_colours) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 26))
}

#For plankton
# Filter only plankton group
Plankton_mass_prod <- Mass_prod %>%
  filter(Group == "Plankton")

p1 <- plot_bars(Plankton_mass_prod, "Biomass", "Phytoplankton", "Concentration (g/m3)", "Concentration of surface phytoplankton")
p2 <- plot_bars(Plankton_mass_prod, "Biomass", "Omnivorous zooplankton", "Biomass (t/km2)", "Biomass of omnivorous zooplankton")
p3 <- plot_bars(Plankton_mass_prod, "Biomass", "Carnivorous zooplankton", "Biomass (t/km2)", "Biomass of carnivorous zooplankton")
p4 <- plot_bars(Plankton_mass_prod, "Production", "Phytoplankton", "Production (t/km2/y)", "Net primary production of phytoplankton")
p5 <- plot_bars(Plankton_mass_prod, "Production", "Omnivorous zooplankton", "Production (t/km2/y)", "Net production of omnivorous zooplankton")
p6 <- plot_bars(Plankton_mass_prod, "Production", "Carnivorous zooplankton", "Production (t/km2/y)", "Net production of carnivorous zooplankton")

(p1 | p4) / (p2 | p5) / (p3 | p6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#For fish

Fish_mass_prod <- Mass_prod %>%
  filter(Group == "Fish")

f1 <- plot_bars(Fish_mass_prod, "Biomass", "Planktivorous fish", "Biomass (t/km2)", "Annual biomass of\n planktivorous fish")
f2 <- plot_bars(Fish_mass_prod, "Biomass", "Demersal fish", "Biomass (t/km2)", "Annual biomass of\n demersal fish")
f3 <- plot_bars(Fish_mass_prod, "Biomass", "Migratory fish", "Biomass (t/km2)", "Annual biomass of\n migratory fish")
f4 <- plot_bars(Fish_mass_prod, "Production", "Planktivorous fish", "Production (t/km2/y)", "Annual net production of\n planktivorous fish")
f5 <- plot_bars(Fish_mass_prod, "Production", "Demersal fish", "Production (t/km2/y)", "Annual net production of\n demersal fish")
f6 <- plot_bars(Fish_mass_prod, "Production", "Migratory fish", "Production (t/km2/y)", "Annual net production of\n migratory fish")

(f1 | f2 | f3) / (f4 | f5 | f6)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


#For top-predators

Toppred_mass_prod <- Mass_prod %>%
  filter(Group =="TopPredators")

tp1 <- plot_bars(Toppred_mass_prod, "Biomass", "Seals", "Biomass (t/km2)", "Annual biomass of\nseals")
tp2 <- plot_bars(Toppred_mass_prod, "Biomass", "Cetaceans", "Biomass (t/km2)", "Annual biomass of\ncetaceans")
tp3 <- plot_bars(Toppred_mass_prod, "Biomass", "Birds", "Biomass (t/km2)", "Annual biomass of\nbirds")

tp4 <- plot_bars(Toppred_mass_prod, "Production", "Seals", "Production (t/km2/y)", "Annual net production of\nseals")
tp5 <- plot_bars(Toppred_mass_prod, "Production", "Cetaceans", "Production (t/km2/y)", "Annual net production of\ncetaceans")
tp6 <- plot_bars(Toppred_mass_prod, "Production", "Birds", "Production (t/km2/y)", "Annual net production of\nbirds")

(tp1 | tp2 | tp3) / (tp4 | tp5 | tp6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#For Benthos

Benthos_mass_prod <- Mass_prod %>%
  filter(Group =="Benthos")

b1 <- plot_bars(Benthos_mass_prod, "Biomass", "Suspensivore/depositivore benthos", "Biomass (t/km2)", "Annual biomass of\nsuspension/deposit feeders")
b2 <- plot_bars(Benthos_mass_prod, "Biomass", "Carnivorous/scavenger benthos", "Biomass (t/km2)", "Annual biomass of\ncarnivorous/scavenger feeders")

b3 <- plot_bars(Benthos_mass_prod, "Production", "Suspensivore/depositivore benthos", "Production (t/km2/y)", "Annual net production of\nsuspension/deposit feeders")
b4 <- plot_bars(Benthos_mass_prod, "Production", "Carnivorous/scavenger benthos", "Production (t/km2/y)", "Annual net production of\ncarnivorous/scavenger feeders")

(b1 | b3) / (b2 | b4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


Nutrient_mass_prod <- Mass_prod %>%
  filter(Group =="Nutrient")

n1 <- plot_bars(Nutrient_mass_prod, "Biomass", "Ammonia", "mMN/m3/y", "Surface ammonia")
n2 <- plot_bars(Nutrient_mass_prod, "Biomass", "Nitrate", "mMN/m3/y", "Surface nitrate")

(n1 | n2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

(n1 | n2)  + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")



#Test Heat plot

Prod_diff <- Mass_prod %>%
  filter(Variable=="Production")%>%
  left_join(
    Mass_prod %>%
      filter(Variable=="Production")%>%
      filter(Decade == "2010-2019") %>%
      select(Scenario, Component, Value_2010_2019 = Value),
    by = c("Scenario", "Component")
  ) %>%
  # Calculate the percentage difference
  mutate(
    DeltaValue = (Value - Value_2010_2019) / Value_2010_2019
  ) %>%
  # Remove the Value_2010_2019 column after calculation
  select(-Value_2010_2019)%>%
  filter(Decade!="2010-2019")

ggplot(Prod_diff, aes(x = Decade, y = Component, fill = DeltaValue)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0,
    name = "Change (%)"
  ) +
  facet_wrap(~ Scenario) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Percentage Change Compared to 2010–2019",
    x = "Decade",
    y = "Component"
  )


#Test difference plot

Diff_df<-Mass_prod%>%
  filter(Variable=="Production",
         Decade %in% c("2010-2019","2060-2069"))%>%
  pivot_wider(names_from = Decade,
              values_from = Value,
              names_prefix="Production_")%>%
  mutate(Difference=(`Production_2060-2069`-`Production_2010-2019`)/`Production_2010-2019`*100,
         Sign=ifelse(Difference>=0,"Positive","Negative"))

color_sign=c(
  "Negative"="#FA7070",
  "Positive"="#A6CF98"
)
scenario_labels <- c(
  CNRM_SSP126 = "CNRM - SSP126",
  CNRM_SSP370 = "CNRM - SSP370",
  GFDL_SSP126 = "GFDL - SSP126",
  GFDL_SSP370 = "GFDL - SSP370"
)

Order_species<-c("Phytoplankton",
                 "Omnivorous zooplankton",
                 "Carnivorous zooplankton",
                 "Suspensivore/depositivore benthos",
                 "Carnivorous/scavenger benthos",
                 "Planktivorous fish",
                 "Demersal fish",
                 "Migratory fish",
                 "Birds",
                 "Seals",
                 "Cetaceans"
                 )

ggplot(Diff_df, aes(x = Difference, y = factor(Component, levels = rev(Order_species)), fill = Sign)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~ Scenario, ncol = 2, labeller = as_labeller(scenario_labels)) +
  scale_fill_manual(values = color_sign) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_discrete(expand = expansion(mult = c(0, 0)))+
  theme_minimal(base_family = "roboto") +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(
                                  margin = margin(b = 20)),
        text = element_text(size = 26),
        strip.text = element_text(
          face = "bold",
          size = 24,
          margin = margin(b = 15, t = 5,l=0,r=10)
        ),
        strip.placement = "outside",
        panel.spacing = unit(2.5, "lines"),
        legend.position = "none")+
  labs(
    title = "Change in production between 2010-2019 and 2060-2069",
    y = "",
    x = ""
  )+
  xlim(-40,10)

ggsave("./Plots/Diff_production.pdf", width = 1866/90, height = 1080/90)
