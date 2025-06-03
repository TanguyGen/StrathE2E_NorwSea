rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","tidyr") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
results_lists<-readRDS("./temp/list_results.rds")
Species_def<-read.csv("Species_def.csv")

#Annual total consumptions

Fisheries_byprey<-lapply(names(results_lists), function(scenario) { 
  scenario_results <- results_lists[[scenario]]
  
  lapply(names(scenario_results), function(decade) {
    
    all_outputs <- scenario_results[[decade]]$output
    landings <- all_output %>%
      bind_rows()%>%
      filter(str_detect(category, regex("^(land|disc|offal)", ignore_case = TRUE)))
      slice_tail(n = 360)
    
    landings<-landings%>%
      pivot_longer(
        cols = starts_with(c("land","Land","disc","offal")),
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