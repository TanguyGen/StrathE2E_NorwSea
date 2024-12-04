rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","pbapply","tictoc") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
models_lists<-readRDS("./temp/list_models.rds")



tic()
run_ycurve_with_bisection <- function(model, selection, hr_field, nyears = 10, threshold = 0.01) {
  HR_min <- 0
  HR_max <- 10
  diff_highest_catches <- Inf
  all_yield_data <- tibble()  # To store all intermediate yield data as a tibble
  
  while (diff_highest_catches > threshold) {
    # Generate HRvector with unique, ascending values
    HRvector <- seq(HR_min, HR_max, length.out = 5)  # Ensure at least 5 values
    
    # Run the model
    yield_data <- e2e_run_ycurve(model, selection = selection, nyears = nyears, HRvector = HRvector)
    
    # Convert to a tibble and calculate catches
    yield_data <- yield_data%>%
      mutate(
      Harvest_Ratio = yield_data[[hr_field]],
      Catches = c(yield_data$PlankFishland + yield_data$PlankFishdisc)  # For PLANKTIV
    )
    if (selection == "DEMERSAL") {
      yield_data$Catches <- c(yield_data$DemFishland + yield_data$DemFishdisc)  # For DEMERSAL
    }
    
    # Append the new data, removing duplicates, and sort by Harvest Ratio
    all_yield_data <- all_yield_data %>%
      bind_rows(yield_data) %>%  # Append new data
      distinct(Harvest_Ratio, .keep_all = TRUE) %>%  # Remove duplicates based on Harvest Ratio
      arrange(Harvest_Ratio)
    
    # Calculate catches and identify top harvest ratios
    catches <- all_yield_data$Catches
    hr_values <- all_yield_data$Harvest_Ratio
    sorted_indices <- order(catches, decreasing = TRUE)
    top_hr_values <- hr_values[sorted_indices[1:2]]
    
    # Update the difference between highest catches
    diff_highest_catches <- catches[sorted_indices[1]] - catches[sorted_indices[2]]
    diff_highest_hr<-
    
    # Adjust HR_min and HR_max to focus on the top region
    if (diff_highest_catches > threshold) {
      HR_min <- min(top_hr_values)
      HR_max <- max(top_hr_values)
    } else {
      max_catch_hr <- hr_values[sorted_indices[1]]
      HR_min <- 0.9 * max_catch_hr
      HR_max <- 1.1 * max_catch_hr
      HRvector <- seq(HR_min, HR_max, length.out = 10)  # Higher precision
      yield_data_refined <- e2e_run_ycurve(model, selection = selection, nyears = nyears, HRvector = HRvector)
      
      # Add refined yield data and exit loop
      all_yield_data <- bind_rows(all_yield_data, as_tibble(yield_data_refined)) %>%
        distinct(across(hr_field), .keep_all = TRUE) %>%
        arrange(across(hr_field))
      break
    }
  }
  
  return(all_yield_data)  # Return all intermediate yield data as a tibble
}

# Main execution loop
fisheries_hr <- names(models_lists) %>%
  pblapply(function(scenario) {
    scenario_models <- models_lists[[scenario]]  # Access each scenario
    
    lapply(names(scenario_models), function(decade) {
      model <- scenario_models[[decade]]
      
      # Run adjustments for planktivorous and demersal fish
      pf_yield_data_all <- run_ycurve_with_bisection(
        model, selection = "PLANKTIV", hr_field = "PlankFishHRmult"
      )%>%  # Sort by Harvest Ratio
        select(-Catches,-Harvest_Ratio)
      pd_yield_data_all <- run_ycurve_with_bisection(
        model, selection = "DEMERSAL", hr_field = "DemFishHRmult"
      )%>%  # Sort by Harvest Ratio
        select(-Catches,-Harvest_Ratio)
      
      # Return results for the decade
      list(
        Decade = decade,
        Scenario = scenario,
        PF_Yield_Data_All = pf_yield_data_all,  # All PF yield data as tibble
        PD_Yield_Data_All = pd_yield_data_all   # All PD yield data as tibble
      )
    }) %>% setNames(names(scenario_models)) # Name by decades
  }) %>% setNames(names(models_lists)) # Name by scenarios




saveRDS(fisheries_hr,file="./temp/Fisheries_MSY.RDS")
toc() #3h20

e2e_plot_ycurve(list_models$CNRM_SSP126$`2010-2019`,selection="PLANKTIV",use.saved = FALSE,results=fisheries_hr$CNRM_SSP126$`2010-2019`$PF_Yield_Data_All)
#fisheries_hr<-readRDS(file="./temp/Fisheries_MSY.RDS")

MSY <- lapply(names(fisheries_hr),FUN=function(scenario){
  fisheries_hr[[scenario]]%>%
    group_by(Decade)%>%
    mutate(MSY_pf=max(Plankticatch),
           HR_max_pf = Harvest_ratio[which.max(Plankticatch)],
           MSY_df=max(Demcatch),
           HR_max_df = Harvest_ratio[which.max(Demcatch)])
})
names(MSY)<-scenarios

ggplot(fisheries_hr$CNRM_SSP126,aes(x = Harvest_ratio, y = Plankticatch, color = Decade, group = Decade)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = MSY$CNRM_SSP126$MSY_pf, color = Decade),linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept=MSY$CNRM_SSP126$HR_max_pf,color=Decade),linetype = "dashed", linewidth = 1)+
  labs(title = "Catch of planktivorous fish by the harvest ratio for CNRM and SSP126 scenarios",
       x = "Pelagic fish harvest ratio",
       y = "Catch (mMN/m2/y)",
       color = "Decade") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))

ggplot(fisheries_hr$CNRM_SSP370,aes(x = Harvest_ratio, y = Plankticatch, color = Decade, group = Decade)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = MSY$CNRM_SSP370$MSY_pf, color = Decade),linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept=MSY$CNRM_SSP370$HR_max_pf,color=Decade),linetype = "dashed", linewidth = 1)+
  labs(title = "Catch of planktivorous fish by the harvest ratio for CNRM and SSP370 scenarios",
       x = "Pelagic fish harvest ratio",
       y = "Catch (mMN/m2/y)",
       color = "Decade") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))

ggplot(fisheries_hr$CNRM_SSP370,aes(x = Harvest_ratio, y = Demcatch, color = Decade, group = Decade)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = MSY$CNRM_SSP370$MSY_df, color = Decade),linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept=MSY$CNRM_SSP370$HR_max_df,color=Decade),linetype = "dashed", linewidth = 1)+
  labs(title = "Catch of demersal fish by the harvest ratio for CNRM and SSP126 scenarios",
       x = "Demersal fish harvest ratio",
       y = "Catch (mMN/m2/y)",
       color = "Decade") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))
