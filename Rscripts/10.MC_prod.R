rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","tidyr") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
mc_lists<-readRDS("./temp/flux_ci.rds")


# Create a single unified data frame
MC_prod <- lapply(names(mc_lists), function(scenario) { 
  scenario_results <- mc_lists[[scenario]]
  
  lapply(names(scenario_results), function(decade) {
    outputs <- scenario_results[[decade]]
    
    production_info <- list(
      list(desc = "Phytoplankton_net_primary_production", var="Production", group = "Plankton", component = "Phytoplankton", divisor = 1.257861635),
      list(desc = "Omniv.zooplankton_net_production", var="Production", group = "Plankton", component = "Omnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Carniv.zooplankton_net_production", var="Production", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Planktiv.fish_net_production", var="Production", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_net_production", var="Production", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Mig.fish_net_production", var="Production", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Pinniped_net_production", var="Production", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Cetacean_net_production", var="Production", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_net_production", var="Production", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthos_susp/dep_net_production", var="Production", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthos_carn/scav_net_production", var="Production", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308),
      list(desc = "Macrophyte_gross_production", var="Production", group = "Benthos", component = "Macrophytes", divisor = 2.07),
      list(desc = "Prod_primary", var="Production", group = "Benthos", component = "Macrophytes", divisor = 2.07),
      list(desc = "Prod_primary", var="Production", group = "Benthos", component = "Macrophytes", divisor = 2.07),
      list(desc = "Prod_fish", var="Production", group = "Fish", component = "Fish", divisor = 2.037735849),
      list(desc = "Prod_tp", var="Production", group = "Fish", component = "Top Predators", divisor = 2.51572327),
      list(desc = "Prod_zoo", var="Production", group = "Fish", component = "Zooplankton", divisor = 1.257861635)
    ) 
    
    recruitment_info <- list(
      list(desc = "Plank.fish_annual_recruitment", var="Recruitment", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_annual_recruitment", var="Recruitment", group = "Fish", component = "Demersal fish", divisor = 1.295597484)
    ) 
    
    
    landings_info <- list(
      list(desc = "Carniv.zooplankton_landings_live_weight", var="Landings", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Plank.fish_landings_live_weight", var="Landings", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_landings_live_weight", var="Landings", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Pinniped_landings_live_weight", var="Landings", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Mig.fish_landings_live_weight", var="Landings", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Cetacean_landings_live_weight", var="Landings", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_landings_live_weight", var="Landings", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthoss/d_landings_live_weight", var="Landings", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthosc/s_landings_live_weight", var="Landings", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308)
    )
    
    discards_info <- list(
      list(desc = "Carniv.zooplankton_discards", var="Discards", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Plank.fish_discards", var="Discards", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_discards", var="Discards", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Pinniped_discards", var="Discards", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Mig.fish_discards", var="Discards", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Cetacean_discards", var="Discards", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_discards", var="Discards", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthoss/d_discards", var="Discards", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthosc/s_discards", var="Discards", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308)
    ) 
    
    offal_info<-list(
      list(desc = "Carniv.zooplankton_offal", var="Offal", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Plank.fish_offal", var="Offal", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_offal", var="Offal", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Pinniped_offal", var="Offal", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Mig.fish_offal", var="Offal", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Cetacean_offal", var="Offal", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_offal", var="Offal", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthoss/d_offal", var="Offal", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthosc/s_offal", var="Offal", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308)
    ) 
    
    catch_info<-list(
      list(desc = "Carniv.zooplankton_catch", var="Catch", group = "Plankton", component = "Carnivorous zooplankton", divisor = 1.257861635),
      list(desc = "Plank.fish_catch", var="Catch", group = "Fish", component = "Planktivorous fish", divisor = 2.037735849),
      list(desc = "Dem.fish_catch", var="Catch", group = "Fish", component = "Demersal fish", divisor = 1.295597484),
      list(desc = "Pinniped_catch", var="Catch", group = "TopPredators", component = "Seals", divisor = 2.51572327),
      list(desc = "Mig.fish_catch", var="Catch", group = "Fish", component = "Migratory fish", divisor = 2.314465409),
      list(desc = "Cetacean_catch", var="Catch", group = "TopPredators", component = "Cetaceans", divisor = 2.51572327),
      list(desc = "Bird_catch", var="Catch", group = "TopPredators", component = "Birds", divisor = 2.51572327),
      list(desc = "Benthoss/d_catch", var="Catch", group = "Benthos", component = "Suspensivore/depositivore benthos", divisor = 0.503144654),
      list(desc = "Benthosc/s_catch", var="Catch", group = "Benthos", component = "Carnivorous/scavenger benthos", divisor = 1.006289308)
    ) 
    

    extract_df <- function(info_list, outputs, scenario, decade) {
      lapply(info_list, function(info) {
        if (info$desc %in% names(outputs)) {
          data <- if (decade == "2010-2019") outputs[info$desc] / info$divisor else outputs[info$desc]
          df <- as.data.frame(t(data))
          df <- df %>% mutate(
            Desc = info$desc,
            Group = info$group,
            Component = info$component,
            Decade = decade,
            Scenario = scenario,
            Var = info$var
          )
          rownames(df) <- NULL
          return(df)
        } else {
          return(NULL)
        }
      }) %>% bind_rows()
    }
    
    
    production_df <- extract_df(production_info, outputs, scenario, decade)
    recruitment_df <- extract_df(recruitment_info, outputs, scenario, decade)
    landings_df <- extract_df(landings_info, outputs, scenario, decade)
    discards_df <- extract_df(discards_info, outputs, scenario, decade)
    offal_df    <- extract_df(offal_info, outputs, scenario, decade)
    catch_df    <- extract_df(catch_info, outputs, scenario, decade)
    
    
    
    combined_df <- bind_rows(
      production_df,
      recruitment_df,
      landings_df,
      discards_df,
      offal_df,
      catch_df
    )
    
    
  }) %>% bind_rows()
}) %>% bind_rows()

MC_prod$Decade <- factor(MC_prod$Decade, levels = sort(unique(MC_prod$Decade)))

Scenario_colours <- c(
  "CNRM_SSP126" = "#1B9AAA",
  "CNRM_SSP370" = "#A0DDE6",
  "GFDL_SSP126" = "#D7263D",
  "GFDL_SSP370" = "#F26B6B"
)


plot_bars <- function(df, var, comp, ylab) {
  
  ggplot(df, aes(x = Decade)) +
    geom_bar(aes(y = maxlik, fill = Scenario), stat = "identity", position = position_dodge(width = 0.75), width = 0.75, alpha = 0.8) +
    geom_errorbar(aes(ymin = lowlimit, ymax = upplimit, group = Scenario), position = position_dodge(width = 0.75),color="#273043", width = 0.2) +
    labs(title = "", y = ylab, x = "") +
    scale_fill_manual(values = Scenario_colours) +
    theme_minimal() +
    guides(group = "none") +
    theme(text = element_text(size = 26))
}

plot_anomaly_bars <- function(df, var, comp,title) {
  ggplot(df, aes(x = Decade)) +
    geom_bar(aes(y = maxlik , fill = Scenario), stat = "identity",
             position = position_dodge(width = 0.75), width = 0.75, alpha = 0.8) +
    geom_errorbar(aes(ymin = lowlimit , ymax = upplimit, group = Scenario),
                  position = position_dodge(width = 0.75), width = 0.2,color="#273043") +
    labs(title = title, y = "Anomaly (%)", x = "") +
    scale_y_continuous(limits = c(-50,50
                       ))+
    scale_fill_manual(values = Scenario_colours) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.3),
          text = element_text(size = 26))
}


plot_prodbar<-function(df,var, comp, ylab, title){
  
  df_baseline <- df %>%
    filter(Decade == "2010-2019", Component == comp, Var == var)
  
  df_rest <- df %>%
    filter(Decade != "2010-2019", Component == comp, Var == var)
  
  
  p_baseline <- plot_bars(df_baseline, var,comp, ylab)
  
  p_rest <- plot_anomaly_bars(df_rest, var, comp,title)
  
  p_baseline + p_rest + plot_layout(ncol = 2, widths = c(0.25, 1))
}

#For plankton
# Filter only plankton group
Plankton_mass_prod <- MC_prod %>%
  filter(Group == "Plankton")


p4 <- plot_prodbar(Plankton_mass_prod, var="Production", comp="Phytoplankton", ylab="Production (t/km2/y)", title="Net primary production of phytoplankton")
p5 <- plot_prodbar(Plankton_mass_prod, var="Production" , comp="Omnivorous zooplankton",ylab= "Production (t/km2/y)", title="Net production of omnivorous zooplankton")
p6 <- plot_prodbar(Plankton_mass_prod, var="Production", comp="Carnivorous zooplankton", ylab="Production (t/km2/y)", title="Net production of carnivorous zooplankton")


(p4) / (p5) / (p6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#For fish

Fish_mass_prod <- MC_prod %>%
  filter(Group == "Fish")

f1 <- plot_prodbar(Fish_mass_prod, var="Production", comp="Planktivorous fish", ylab="Production (t/km2/y)", title="Annual net production of\n planktivorous fish")
f2 <- plot_prodbar(Fish_mass_prod, var="Production",comp="Demersal fish", ylab="Production (t/km2/y)", title="Annual net production of\n demersal fish")
f3 <- plot_prodbar(Fish_mass_prod, var="Production", comp="Migratory fish", "Production (t/km2/y)", title="Annual net production of\n migratory fish")

f4 <- plot_prodbar(Fish_mass_prod, var="Catch", "Planktivorous fish", ylab="Catches (t/km2/y)", title="Annual catches of\n planktivorous fish")
f5 <- plot_prodbar(Fish_mass_prod, var="Catch","Demersal fish", ylab="Catches (t/km2/y)",title= "Annual catches of\n demersal fish")
f6 <- plot_prodbar(Fish_mass_prod, var="Catch", "Migratory fish", ylab="Catches (t/km2/y)", title="Annual catches of\n migratory fish")

((f1 / f2 / f3))+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


#For top-predators

Toppred_mass_prod <- MC_prod %>%
  filter(Group =="TopPredators")

tp1 <- plot_prodbar(Toppred_mass_prod, var ="Production",  comp="Seals",  ylab="Production (t/km2)", "Annual net production of seals")
tp2 <- plot_prodbar(Toppred_mass_prod,var ="Production",   comp="Cetaceans",  ylab="Production (t/km2)", "Annual net production of cetaceans")
tp3 <- plot_prodbar(Toppred_mass_prod, var ="Production", comp="Birds",  ylab="Production (t/km2)", "Annual net production of birds")

(tp1 / tp2 /tp3)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


Benthos_mass_prod <- MC_prod %>%
  filter(Group =="Benthos")

b1 <- plot_prodbar(Benthos_mass_prod, var ="Production",  comp="Suspensivore/depositivore benthos",  ylab="Production (t/km2)", "Annual net production of susp. benthos")
b2 <- plot_prodbar(Benthos_mass_prod,var ="Production",   comp="Carnivorous/scavenger benthos",  ylab="Production (t/km2)", "Annual net production of carn. benthos")
b3 <- plot_prodbar(Benthos_mass_prod,var ="Production",   comp="Carnivorous/scavenger benthos",  ylab="Production (t/km2)", "Annual net production of carn. benthos")

(b1 / b2 )+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


#For all


a1 <- plot_prodbar(MC_prod, var ="Production",  comp="Phytoplankton",  ylab="Production (t/km2)", "Annual net primary production")
a2 <- plot_prodbar(MC_prod,var ="Production",   comp="Zooplankton",  ylab="Production (t/km2)", "Annual net secondary production")
a3 <- plot_prodbar(MC_prod, var ="Production", comp="Fish",  ylab="Production (t/km2)", "Annual net production of fish")
a4 <- plot_prodbar(MC_prod, var ="Production", comp="Top Predators",  ylab="Production (t/km2)", "Annual net production of\ntop predators")

(a1 / a2 /a3/a4)+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("./Plots/Production_by_group.svg", width = 1866/90, height = 1080/90)


######### Difference plot

Diff_df<-MC_prod%>%
  filter(Var=="Production",
         Decade == "2060-2069")%>%
  mutate(Sign=ifelse(maxlik>=0,"Positive","Negative"))%>%
  filter(!Component %in% c("Fish","Top Predators", "Zooplankton"))

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
                 "Macrophytes",
                 "Suspensivore/depositivore benthos",
                 "Carnivorous/scavenger benthos",
                 "Planktivorous fish",
                 "Demersal fish",
                 "Migratory fish",
                 "Birds",
                 "Seals",
                 "Cetaceans"
)

ggplot(Diff_df, aes(x = maxlik, y = factor(Component, levels = rev(Order_species)), fill = Sign)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin = lowlimit, xmax = upplimit), position = position_dodge(width = 0.75),color="#6C3A5C", width = 0.2) +
  facet_wrap(~ Scenario, ncol = 2, labeller = as_labeller(scenario_labels)) +
  scale_fill_manual(values = color_sign) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(-102, 70)
  )+
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
    title = "Percent change in production between 2010-2019 and 2060-2069",
    y = "",
    x = ""
  )

ggsave("./Plots/Diff_production.svg", width = 1866/90, height = 1900/90)
