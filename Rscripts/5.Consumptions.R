rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","tidyr") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
results_lists<-readRDS("./temp/list_results.rds")
Species_def<-read.csv("Species_def.csv")

#Annual total consumptions

Consumptions_tot<-lapply(names(results_lists), function(scenario) { 
  scenario_results <- results_lists[[scenario]]
  
  lapply(names(scenario_results), function(decade) {
    
    consumption_dt <- scenario_results[[decade]]$final.year.outputs$annual_flux_results_wholedomain
    
    consumption_dt<-consumption_dt%>%
      filter(grepl("^[^ ]+_to_[^ ]+$", Description))%>%
      separate(Description, into = c("Prey", "Predator"), sep = "_to_")%>%
      mutate(Prey = sub("^Flux_", "", Prey))%>%
      rename(Cons_names=Prey)%>%
      left_join(Species_def)%>%
      mutate(Consumption=Model_annual_flux/Conversion_rate)%>%
      select(-c(Cons_names,Conversion_rate))%>%
      rename(Prey=FullName)
    
    Total<-consumption_dt%>%
      group_by(Predator)%>%
      summarise(Consumption=sum(Consumption,na.rm=TRUE))%>%
      mutate(
        Decade=decade,
        Scenario=scenario
      )
    
      Total
  }) %>% bind_rows()  # transpose so we get two lists: one for each data frame
}) %>% bind_rows()

#Replace by full names

Consumptions_tot<-Consumptions_tot%>%
  rename(Cons_names=Predator)%>%
  left_join(Species_def)%>%
  rename(Predator=FullName)%>%
  select(-c(Cons_names,Conversion_rate,Flux_names))%>%
  drop_na(Predator)


#Add species as colnames

Consumptions_tot<-Consumptions_tot%>%
  pivot_wider(
    names_from = Predator,
    values_from = Consumption,
    values_fn = ~ sum(.x, na.rm = TRUE)
  )


f1 <- ggplot(Consumptions_tot, aes(x = Decade, y = `Planktivorous Fish`+`Planktivorous Fish Larvae`, fill = Scenario, group = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75) +
  ylim(0, max(Consumptions_tot$`Planktivorous Fish`+Consumptions_tot$`Planktivorous Fish Larvae`) * 1.1) +
  labs(title = "Consumption Planktivorous fish",
       x = "",
       y = "t/km2",
       color = "Scenario") +
  scale_fill_manual(values = c("#FF8343", "#F1DEC6", "#179BAE", "#4158A6")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 40, 0)),
        text = element_text(size = 26))

#Annual diet

Consumptions_byprey<-lapply(names(results_lists), function(scenario) { 
  scenario_results <- results_lists[[scenario]]
  
  lapply(names(scenario_results), function(decade) {
    
    all_outputs <- scenario_results[[decade]]$output
    consumptions <- all_outputs[grepl("^flux[^_]+_[^_]+$", names(all_outputs))] %>%
      bind_rows()%>%
      slice_tail(n = 360)%>%
      mutate(Day=1:360,
             Month=rep(1:12, each=30))
    
    consumptions<-consumptions%>%
      pivot_longer(
        cols = starts_with("flux"),
        names_to = "Fluxes",
        values_to = "Consumption"
      )%>%
      separate(Fluxes, into = c("Prey", "Predator"), sep = "_")%>%
      mutate(Prey = sub("flux", "", Prey))
    
    consumptions<-consumptions%>%
      mutate(
        Decade=decade,
        Scenario=scenario
      )
    consumptions
  })%>% bind_rows()
})%>% bind_rows()

Consumptions_annual<-Consumptions_byprey%>%
  rename(Flux_names=Prey)%>%
  left_join(Species_def,by="Flux_names")%>%
  mutate(Consumption=Consumption/Conversion_rate)%>%
  select(-c(Flux_names,Conversion_rate,Cons_names))%>%
  rename(Prey=FullName,
         Flux_names=Predator)%>%
  left_join(Species_def,by="Flux_names")%>%
  select(-c(Flux_names,Conversion_rate,Cons_names))%>%
  rename(Predator=FullName)%>%
  drop_na(Prey,Predator)

Consumptions_monthly <- Consumptions_annual %>%
  group_by(Predator, Decade, Scenario, Prey) %>%
  summarise(Total_consumption = sum(Consumption, na.rm = TRUE), .groups = "drop") %>%
  group_by(Predator, Decade, Scenario) %>%
  mutate(
    Total_daily_consumption = sum(Total_consumption),
    Proportion = Total_consumption / Total_daily_consumption
  ) %>%
  ungroup()

pfish<-Consumptions_monthly%>%
  filter(Predator=="Birds")

ggplot(pfish, aes(x = Decade, y = Proportion, fill = Prey)) +
  geom_bar(stat = "identity",
           width = 1,
           colour = "black") +
  labs(y = "", x = "Day") +
  facet_wrap(~Scenario)+
  theme_classic()
